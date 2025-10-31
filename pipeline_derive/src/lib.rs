extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    FnArg, Ident, ItemFn, ItemMod, LitStr, PatType, Type,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

// Compile the template into the derive crate binary:
const HTML_TEMPLATE: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/pipeline_graph.html"
));

// Import Spanned for error reporting
use syn::spanned::Spanned;

#[proc_macro_attribute]
pub fn pipeline(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse the attribute arguments and the module item
    let attr_args = parse_macro_input!(attr as PipelineArgs);
    let mut module = parse_macro_input!(item as ItemMod);

    let pipeline_name = attr_args.name;
    let pipeline_name_str = quote! {#pipeline_name}.to_string();
    let constructor_args = attr_args.args;
    let context_name = attr_args.context_name;

    // Extract public functions annotated with #[stage]
    let stages = extract_stages(&mut module);

    // Infer the context type from the stages
    let context_param = if let Some(context_name) = &context_name {
        match infer_context_type(&stages, context_name) {
            Some(ty) => Some((context_name.clone(), ty)),
            None => {
                return syn::Error::new_spanned(
                    context_name,
                    format!(
                        "Context parameter '{}' not found in any stage functions",
                        context_name
                    ),
                )
                .to_compile_error()
                .into();
            }
        }
    } else {
        None
    };

    // Collect unique parameters from all stages, excluding the context parameter
    let (fields, field_names, pipeline_vars) = collect_fields(&stages, context_name.as_ref());

    // Perform Dependency Analysis and Topological Sort
    let compute_calls = match generate_compute_calls(&stages, context_name.as_ref()) {
        Ok(calls) => calls,
        Err(e) => return e.to_compile_error().into(),
    };

    // Collect the names of variables written by any stage.  Only these need resetting.
    let output_vars = collect_outputs(&stages, context_name.as_ref());

    // Determine error type: user-specified or default to pipeline::Error
    let error_ty = attr_args.error_ty.clone().unwrap_or_else(|| {
        syn::parse_str::<Type>("pipeline::Error").expect("Failed to parse default error type")
    });

    // Generate the PUML Diagram Content
    let puml_content = generate_puml(&stages, context_name.as_ref());

    // Generate the JSON for HTML diagram (nodes/edges only)
    let (nodes_json, edges_json) = generate_html_data(&stages, context_name.as_ref());

    // Generate the struct (context is not included)
    let struct_def = generate_struct(&pipeline_name, &fields, &pipeline_vars);

    // Generate the impl block
    let impl_block = generate_impl(
        &pipeline_name,
        &constructor_args,
        &fields,
        &compute_calls,
        &field_names,
        &puml_content,
        &pipeline_name_str,
        &nodes_json,
        &edges_json,
        context_param.as_ref(),
        &output_vars,
        &error_ty,
    );

    // Reconstruct the module with stages unmodified
    let output = quote! {
        #module

        #struct_def

        #impl_block
    };

    output.into()
}

// Custom parser for attribute arguments
struct PipelineArgs {
    name: Ident,
    args: Vec<Ident>,
    context_name: Option<Ident>,
    error_ty: Option<Type>,
}

impl Parse for PipelineArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut name = None;
        let mut args = Vec::new();
        let mut context_name = None;
        let mut error_ty = None;

        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<syn::Token![=]>()?;
            if key == "name" {
                let value: LitStr = input.parse()?;
                name = Some(format_ident!("{}", value.value()));
            } else if key == "args" {
                let value: LitStr = input.parse()?;
                args = value
                    .value()
                    .split(',')
                    .map(|s| format_ident!("{}", s.trim()))
                    .collect();
            } else if key == "context" {
                let value: LitStr = input.parse()?;
                context_name = Some(format_ident!("{}", value.value()));
            } else if key == "error" {
                let value: LitStr = input.parse()?;
                // Parse the error type from the provided string
                error_ty = Some(syn::parse_str::<Type>(&value.value())?);
            } else {
                return Err(syn::Error::new_spanned(
                    key,
                    "Expected 'name', 'args', 'context' or 'error' in pipeline attribute",
                ));
            }
            if input.peek(syn::Token![,]) {
                input.parse::<syn::Token![,]>()?;
            }
        }

        let name = name.ok_or_else(|| {
            syn::Error::new(
                proc_macro2::Span::call_site(),
                "Missing 'name' in pipeline attribute",
            )
        })?;

        Ok(PipelineArgs {
            name,
            args,
            context_name,
            error_ty,
        })
    }
}

fn extract_stages(module: &mut ItemMod) -> Vec<ItemFn> {
    let mut stages = Vec::new();

    if let Some((_, items)) = &mut module.content {
        for item in items.iter_mut() {
            if let syn::Item::Fn(func) = item
                && func.attrs.iter().any(|attr| attr.path().is_ident("stage"))
            {
                stages.push(func.clone());
            }
        }
    }

    stages
}

use std::collections::{HashMap, HashSet};

fn infer_context_type(stages: &[ItemFn], context_name: &Ident) -> Option<Type> {
    for stage in stages {
        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType { pat, ty, .. }) = input
                && let syn::Pat::Ident(pat_ident) = &**pat
                && &pat_ident.ident == context_name
            {
                return Some((**ty).clone());
            }
        }
    }
    None
}

fn collect_fields(
    stages: &[ItemFn],
    context_name: Option<&Ident>,
) -> (Vec<(Ident, Type)>, Vec<Ident>, Vec<String>) {
    let mut fields_map = HashMap::new();

    for stage in stages {
        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType { pat, ty, .. }) = input
                && let syn::Pat::Ident(pat_ident) = &**pat
            {
                let ident = pat_ident.ident.clone();

                // Skip the context parameter
                if Some(&ident) == context_name {
                    continue;
                }

                // Extract the underlying type (dereference references)
                let ty = match &**ty {
                    Type::Reference(type_ref) => (*type_ref.elem).clone(),
                    _ => (**ty).clone(),
                };

                fields_map
                    .entry(ident.to_string())
                    .or_insert((ident.clone(), ty));
            }
        }
    }

    let mut fields: Vec<(Ident, Type)> = fields_map.values().cloned().collect();
    fields.sort_by(|a, b| a.0.to_string().cmp(&b.0.to_string()));

    let field_names = fields.iter().map(|(ident, _)| ident.clone()).collect();

    let pipeline_vars = fields.iter().map(|(ident, _)| ident.to_string()).collect();

    (fields, field_names, pipeline_vars)
}

fn generate_struct(
    pipeline_name: &Ident,
    fields: &[(Ident, Type)],
    pipeline_vars: &[String],
) -> proc_macro2::TokenStream {
    let vars_len = pipeline_vars.len();
    let field_defs: Vec<_> = fields
        .iter()
        .map(|(ident, ty)| quote! { pub #ident: #ty })
        .collect();

    quote! {
        pub struct #pipeline_name {
            pub pipeline_vars: [&'static str; #vars_len],
            #(#field_defs),*
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn generate_impl(
    pipeline_name: &Ident,
    constructor_args: &[Ident],
    fields: &[(Ident, Type)],
    compute_calls: &[proc_macro2::TokenStream],
    field_names: &[Ident],
    puml_content: &str,      // Receive the PUML content
    pipeline_name_str: &str, // for HTML template
    nodes_json: &str,
    edges_json: &str,
    context_param: Option<&(Ident, Type)>,
    output_vars: &HashSet<String>,
    error_ty: &Type,
) -> proc_macro2::TokenStream {
    let constructor_params: Vec<_> = constructor_args
        .iter()
        .filter_map(|ident| {
            fields
                .iter()
                .find(|(field_ident, _)| field_ident == ident)
                .map(|(ident, ty)| quote! { #ident: #ty })
        })
        .collect();

    let constructor_inits: Vec<_> = fields
        .iter()
        .map(|(ident, _)| {
            if constructor_args.contains(ident) {
                quote! { #ident }
            } else {
                quote! { #ident: Default::default() }
            }
        })
        .collect();

    let pipeline_vars_init = field_names
        .iter()
        .map(|ident| {
            let name = ident.to_string();
            quote! { #name }
        })
        .collect::<Vec<_>>();

    // Generate reset calls only for fields that are outputs (written by some stage)
    let reset_calls: Vec<_> = fields
        .iter()
        .filter_map(|(ident, _)| {
            let name = ident.to_string();
            if output_vars.contains(&name) {
                Some(quote! {
                    Reset::reset(&mut self.#ident)
                        .map_err(|e| -> #error_ty { e.into() })?;
                })
            } else {
                None
            }
        })
        .collect();

    // Convert PUML to literal
    let puml_literal = LitStr::new(puml_content, proc_macro2::Span::call_site());

    let html_template_lit = LitStr::new(HTML_TEMPLATE, proc_macro2::Span::call_site());

    // Precompute JSONs and pipeline name as literals to splice into generated code
    let nodes_json_lit = LitStr::new(nodes_json, proc_macro2::Span::call_site());
    let edges_json_lit = LitStr::new(edges_json, proc_macro2::Span::call_site());
    let pipeline_name_lit = LitStr::new(pipeline_name_str, proc_macro2::Span::call_site());

    // Prepare compute method signature
    let compute_params = if let Some((context_name, context_type)) = context_param {
        quote! { &mut self, #context_name: #context_type }
    } else {
        quote! { &mut self }
    };

    quote! {
        // Bring Reset into scope so the trait method is available.
        use ::pipeline::Reset;
        impl #pipeline_name {
            pub fn new(#(#constructor_params),*) -> Self {
                Self {
                    pipeline_vars: [#(#pipeline_vars_init),*],
                    #(#constructor_inits),*
                }
            }

            /// Executes all stages in topological order, propagating any errors.
            pub fn compute(#compute_params) -> Result<(), #error_ty> {
                #(#compute_calls)*
                self.reset()?;
                Ok(())
            }

            /// Resets the update flags on all mutated fields.  Errors from
            /// the `Reset` trait are propagated to the caller.
            pub fn reset(&mut self) -> Result<(), #error_ty> {
                #(#reset_calls)*
                Ok(())
            }

            /// Returns the PlantUML diagram representing the pipeline stages and their dependencies.
            pub fn puml_diagram() -> &'static str {
                #puml_literal
            }

            /// Builds the HTML content visualizing the pipeline, using a template embedded with include_str!.
            /// Returns an owned String (not &'static str) since we perform placeholder substitution.
            pub fn html_diagram() -> String {
                // Use the embedded template (from derive crate) as a literal.
                let template: &str = #html_template_lit;
                template
                    .replace("{{PIPELINE_NAME}}", #pipeline_name_lit)
                    .replace("{{NODES_JSON}}", #nodes_json_lit)
                    .replace("{{EDGES_JSON}}", #edges_json_lit)
            }

            /// Writes the HTML diagram to the specified file path.
            pub fn write_html_to_file<P: AsRef<std::path::Path>>(
                file_path: P,
            ) -> std::io::Result<()> {
                std::fs::write(file_path, Self::html_diagram())
            }
        }
    }
}

fn generate_compute_calls(
    stages: &[ItemFn],
    context_name: Option<&Ident>,
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    // Map of variable to the function that writes to it
    let mut var_writers: HashMap<String, Ident> = HashMap::new();

    // Map of function name to StageInfo
    let mut stage_infos: HashMap<Ident, StageInfo> = HashMap::new();

    // Collect read and write variables for each stage
    for stage in stages {
        let mut input_vars = HashSet::new();
        let mut output_vars = HashSet::new();

        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType { pat, ty, .. }) = input
                && let syn::Pat::Ident(pat_ident) = &**pat
            {
                let var_name = pat_ident.ident.to_string();

                // Skip context parameter
                if Some(&pat_ident.ident) == context_name {
                    continue;
                }

                match &**ty {
                    Type::Reference(syn::TypeReference { mutability, .. }) => {
                        if mutability.is_some() {
                            // Mutable reference - variable is written
                            if let Some(existing_writer) = var_writers.get(&var_name) {
                                let error = syn::Error::new(
                                    input.span(),
                                    format!(
                                        "Variable '{}' is written by multiple functions: '{}' and '{}'",
                                        var_name, existing_writer, stage.sig.ident
                                    ),
                                );
                                return Err(error);
                            }
                            var_writers.insert(var_name.clone(), stage.sig.ident.clone());
                            output_vars.insert(var_name);
                        } else {
                            // Immutable reference - variable is read
                            input_vars.insert(var_name);
                        }
                    }
                    _ => {
                        // Not a reference, treat as read
                        input_vars.insert(var_name);
                    }
                }
            }
        }

        stage_infos.insert(
            stage.sig.ident.clone(),
            StageInfo {
                name: stage.sig.ident.clone(),
                inputs: input_vars,
                outputs: output_vars,
                dependencies: HashSet::new(),
            },
        );
    }

    // Build dependencies between stages
    for stage_info in stage_infos.values_mut() {
        for input_var in &stage_info.inputs {
            if let Some(writer_func) = var_writers.get(input_var)
                && writer_func != &stage_info.name
            {
                stage_info.dependencies.insert(writer_func.clone());
            }
        }
    }

    // Perform topological sort
    let sorted_stages = topological_sort(&stage_infos)?;

    // Generate function calls in sorted order
    let mut compute_calls = Vec::new();
    for stage_name in sorted_stages {
        let stage_info = &stage_infos[&stage_name];
        let stage = stages
            .iter()
            .find(|s| s.sig.ident == stage_info.name)
            .expect("Stage function not found");

        let args = stage
            .sig
            .inputs
            .iter()
            .map(|input| {
                if let FnArg::Typed(PatType { pat, ty, .. }) = input {
                    if let syn::Pat::Ident(pat_ident) = &**pat {
                        let ident = &pat_ident.ident;

                        // Determine whether the parameter is mutable or not
                        let is_mut = match &**ty {
                            Type::Reference(syn::TypeReference { mutability, .. }) => {
                                mutability.is_some()
                            }
                            _ => false,
                        };

                        // Check if this is the context parameter
                        if Some(ident) == context_name {
                            quote! { #ident }
                        } else if is_mut {
                            quote! { &mut self.#ident }
                        } else {
                            quote! { &self.#ident }
                        }
                    } else {
                        quote! {}
                    }
                } else {
                    quote! {}
                }
            })
            .collect::<Vec<_>>();

        // Determine whether the stage returns a Result.  If so, use `?` to propagate errors; otherwise just call it.
        let returns_result = matches!(
            &stage.sig.output,
            syn::ReturnType::Type(_, ty) if is_result_type(ty)
        );

        let call = if returns_result {
            quote! { #stage_name(#(#args),*)?; }
        } else {
            quote! { #stage_name(#(#args),*); }
        };

        compute_calls.push(call);
    }

    Ok(compute_calls)
}

/// Returns true if the provided type is a `Result<_, _>`.
fn is_result_type(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty
        && let Some(segment) = type_path.path.segments.last()
    {
        return segment.ident == "Result";
    }
    false
}

/// Collects the names of variables that are written (i.e. passed as `&mut`) in any stage.
fn collect_outputs(
    stages: &[ItemFn],
    context_name: Option<&Ident>,
) -> std::collections::HashSet<String> {
    use std::collections::HashSet;
    let mut outputs = HashSet::new();
    for stage in stages {
        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType { pat, ty, .. }) = input
                && let syn::Pat::Ident(pat_ident) = &**pat
            {
                // Skip context parameter
                if Some(&pat_ident.ident) == context_name {
                    continue;
                }
                // Mutable references are outputs
                if let Type::Reference(type_ref) = &**ty
                    && type_ref.mutability.is_some()
                {
                    outputs.insert(pat_ident.ident.to_string());
                }
            }
        }
    }
    outputs
}

struct StageInfo {
    name: Ident,
    inputs: HashSet<String>,
    #[allow(dead_code)]
    outputs: HashSet<String>,
    dependencies: HashSet<Ident>,
}

fn topological_sort(stages: &HashMap<Ident, StageInfo>) -> syn::Result<Vec<Ident>> {
    let mut sorted = Vec::new();
    let mut visited = HashSet::new();
    let mut temp_mark = HashSet::new();

    fn visit(
        stage_name: &Ident,
        stages: &HashMap<Ident, StageInfo>,
        sorted: &mut Vec<Ident>,
        visited: &mut HashSet<Ident>,
        temp_mark: &mut HashSet<Ident>,
    ) -> syn::Result<()> {
        if temp_mark.contains(stage_name) {
            return Err(syn::Error::new(
                stage_name.span(),
                format!("Cycle detected involving stage '{}'", stage_name),
            ));
        }

        if !visited.contains(stage_name) {
            temp_mark.insert(stage_name.clone());

            let stage_info = stages.get(stage_name).unwrap();

            for dep in &stage_info.dependencies {
                visit(dep, stages, sorted, visited, temp_mark)?;
            }

            temp_mark.remove(stage_name);
            visited.insert(stage_name.clone());
            sorted.push(stage_name.clone());
        }

        Ok(())
    }

    for stage_name in stages.keys() {
        visit(
            stage_name,
            stages,
            &mut sorted,
            &mut visited,
            &mut temp_mark,
        )?;
    }

    Ok(sorted)
}

fn generate_puml(stages: &[ItemFn], context_name: Option<&Ident>) -> String {
    use std::collections::HashSet;

    let mut puml = String::new();
    puml.push_str("@startuml\n");
    puml.push_str("skinparam linetype ortho\n");
    puml.push_str("left to right direction\n"); // Arrange nodes from left to right

    // Define styles for variables and stages using correct syntax
    puml.push_str("skinparam class {\n");
    puml.push_str("    stereotype {\n");
    puml.push_str("        \"<<Variable>>\" {\n");
    puml.push_str("            Shape ellipse\n");
    puml.push_str("            BackgroundColor LightBlue\n");
    puml.push_str("        }\n");
    puml.push_str("        \"<<Stage>>\" {\n");
    puml.push_str("            Shape rectangle\n");
    puml.push_str("            BackgroundColor LightGreen\n");
    puml.push_str("        }\n");
    puml.push_str("    }\n");
    puml.push_str("}\n");

    // Map variable names to their types
    let mut variables = HashSet::new();

    // Collect variables and functions
    for stage in stages {
        let stage_name = stage.sig.ident.to_string();

        // Add the stage as a node with <<Stage>> stereotype
        puml.push_str(&format!("class {} <<Stage>>\n", stage_name));

        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType { pat, ty, .. }) = input
                && let syn::Pat::Ident(pat_ident) = &**pat
            {
                // Skip context parameter
                if Some(&pat_ident.ident) == context_name {
                    continue;
                }

                let var_name = pat_ident.ident.to_string();

                // Prefix variable names with a dollar sign
                let puml_var_name = format!("${}", var_name);

                // Add the variable as a node with <<Variable>> stereotype if not already added
                if variables.insert(var_name.clone()) {
                    puml.push_str(&format!("class \"{}\" <<Variable>>\n", puml_var_name));
                }

                // Determine the direction of the relationship
                let is_mut = match &**ty {
                    Type::Reference(syn::TypeReference { mutability, .. }) => mutability.is_some(),
                    _ => false,
                };

                // Add relationship between the stage and the variable
                if is_mut {
                    // Stage produces Variable (Stage --> Variable)
                    puml.push_str(&format!("{} --> \"{}\"\n", stage_name, puml_var_name));
                } else {
                    // Stage consumes Variable (Variable --> Stage)
                    puml.push_str(&format!("\"{}\" --> {}\n", puml_var_name, stage_name));
                }
            }
        }
    }

    puml.push_str("@enduml\n");

    puml
}

fn generate_html_data(stages: &[ItemFn], context_name: Option<&Ident>) -> (String, String) {
    use serde_json::json;
    use std::collections::HashSet;

    // Collect nodes and edges
    let mut nodes = Vec::new();
    let mut edges = Vec::new();

    // Keep track of variables to avoid duplicates
    let mut variables = HashSet::new();

    for stage in stages {
        let stage_name = stage.sig.ident.to_string();

        // Add the stage as a node
        nodes.push(json!({
            "id": stage_name,
            "label": stage_name,
            "group": "stage"
        }));

        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType { pat, ty, .. }) = input
                && let syn::Pat::Ident(pat_ident) = &**pat
            {
                // Skip context parameter
                if Some(&pat_ident.ident) == context_name {
                    continue;
                }

                let var_name = pat_ident.ident.to_string();

                // Prefix variable names with a dollar sign to distinguish them
                let vis_var_name = format!("${}", var_name);

                // Add the variable as a node if not already added
                if variables.insert(var_name.clone()) {
                    nodes.push(json!({
                        "id": vis_var_name,
                        "label": var_name,
                        "group": "variable"
                    }));
                }

                // Determine the direction of the relationship
                let is_mut = match &**ty {
                    Type::Reference(syn::TypeReference { mutability, .. }) => mutability.is_some(),
                    _ => false,
                };

                if is_mut {
                    // Stage produces Variable (Edge from Stage to Variable)
                    edges.push(json!({
                        "from": stage_name,
                        "to": vis_var_name,
                        "arrows": "to"
                    }));
                } else {
                    // Stage consumes Variable (Edge from Variable to Stage)
                    edges.push(json!({
                        "from": vis_var_name,
                        "to": stage_name,
                        "arrows": "to"
                    }));
                }
            }
        }
    }

    (
        serde_json::to_string(&nodes).unwrap(),
        serde_json::to_string(&edges).unwrap(),
    )
}

#[proc_macro_attribute]
pub fn stage(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // This attribute macro does nothing; it just returns the item unchanged.
    item
}
