extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    Attribute, Expr, ExprLit, FnArg, Ident, ItemFn, ItemMod, Lit, LitStr, Meta, MetaNameValue,
    PatType, Type,
    parse::{Parse, ParseStream},
    parse_macro_input,
};
// Compile the template into the derive crate binary:
const HTML_TEMPLATE: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/pipeline_graph.html"
));

#[proc_macro_attribute]
pub fn pipeline(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse the attribute arguments and the module item
    let attr_args = parse_macro_input!(attr as PipelineArgs);
    let mut module = parse_macro_input!(item as ItemMod);
    let mod_ident = module.ident.clone();

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
    // Pass constructor_args into generate_compute_calls so it can detect missing inputs.
    let compute_calls = match generate_compute_calls(
        &stages,
        context_name.as_ref(),
        &mod_ident,
        &constructor_args,
    ) {
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
                && func.attrs.iter().any(is_stage_attr)
            {
                stages.push(func.clone());
            }
        }
    }

    stages
}

use proc_macro2::Span;
use std::collections::{HashMap, HashSet};
use syn::parse::Parser;
use syn::punctuated::Punctuated;

fn infer_context_type(stages: &[ItemFn], context_name: &Ident) -> Option<Type> {
    for stage in stages {
        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType {
                attrs: _, pat, ty, ..
            }) = input
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
            if let FnArg::Typed(PatType { attrs, pat, ty, .. }) = input
                && let syn::Pat::Ident(pat_ident) = &**pat
            {
                // Skip context parameter
                if Some(&pat_ident.ident) == context_name {
                    continue;
                }

                // Determine the pipeline field name to bind to:
                //   - prefer #[rename(...)]
                //   - else use the parameter's local name
                let target_ident = normalized_target_ident(attrs, &pat_ident.ident);

                // Extract the underlying type (dereference references)
                let ty = match &**ty {
                    Type::Reference(type_ref) => (*type_ref.elem).clone(),
                    _ => (**ty).clone(),
                };

                fields_map
                    .entry(target_ident.to_string())
                    .or_insert((target_ident, ty));
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
    let clear_updated_calls: Vec<_> = fields
        .iter()
        .filter_map(|(ident, _)| {
            let name = ident.to_string();
            if output_vars.contains(&name) {
                Some(quote! {
                    ::pipeline::ClearUpdated::clear_updated(&mut self.#ident)
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
                self.clear_updated_all()?;
                Ok(())
            }

            /// Clear the update flags on all mutated fields.  Errors from
            /// the `ClearUpdated` trait are propagated to the caller.
            pub fn clear_updated_all(&mut self) -> Result<(), #error_ty> {
                #(#clear_updated_calls)*
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
    mod_ident: &Ident,
    constructor_args: &[Ident],
) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    // Maps pipeline field -> stage that writes it
    let mut var_writers: HashMap<String, Ident> = HashMap::new();
    // Track span of each writer parameter for better unused-output errors
    let mut writer_spans: HashMap<String, Span> = HashMap::new();
    // Tracks all read variables and their first occurrence span
    let mut read_spans: HashMap<String, Span> = HashMap::new();
    let mut stage_infos: HashMap<Ident, StageInfo> = HashMap::new();
    let mut output_unused: HashSet<String> = HashSet::new();
    let mut input_unused: HashSet<String> = HashSet::new();

    // Collect read/write variables for each stage and detect duplicate writers
    for stage in stages {
        let mut input_vars = HashSet::new();
        let mut output_vars = HashSet::new();

        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType { attrs, pat, ty, .. }) = input
                && let syn::Pat::Ident(pat_ident) = &**pat
            {
                // Skip context parameter
                if Some(&pat_ident.ident) == context_name {
                    continue;
                }
                // Determine pipeline field name for this parameter
                let target_ident = normalized_target_ident(attrs, &pat_ident.ident);
                let var_name = target_ident.to_string();
                let is_unused = has_unused_attr(attrs);

                match &**ty {
                    // &mut: writer
                    Type::Reference(syn::TypeReference { mutability, .. }) => {
                        if mutability.is_some() {
                            // Mutable reference — variable is written
                            if let Some(existing_writer) = var_writers.get(&var_name) {
                                return Err(syn::Error::new(
                                    input.span(),
                                    format!(
                                        "variable '{}' is written by multiple stages: '{}' and '{}'",
                                        var_name, existing_writer, stage.sig.ident
                                    ),
                                ));
                            }
                            var_writers.insert(var_name.clone(), stage.sig.ident.clone());
                            writer_spans.insert(var_name.clone(), input.span());
                            output_vars.insert(var_name.clone());
                            if is_unused {
                                output_unused.insert(var_name.clone());
                            }
                        } else {
                            // Immutable reference — variable is read
                            input_vars.insert(var_name.clone());
                            read_spans.entry(var_name.clone()).or_insert(input.span());
                            if is_unused {
                                input_unused.insert(var_name.clone());
                            }
                        }
                    }
                    _ => {
                        // Non-reference => treat as read
                        input_vars.insert(var_name.clone());
                        read_spans.entry(var_name).or_insert(input.span());
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

    // Check for missing inputs: read variables not produced by any stage or passed via args/context
    for (read_var, span) in &read_spans {
        if input_unused.contains(read_var) {
            continue; // user marked this as intentionally unused
        }
        let is_arg = constructor_args
            .iter()
            .any(|arg| arg == &format_ident!("{}", read_var));
        let is_ctx = context_name.is_some_and(|c| c == &format_ident!("{}", read_var));
        if !var_writers.contains_key(read_var) && !is_arg && !is_ctx {
            return Err(syn::Error::new(
                *span,
                format!(
                    "variable '{}' is read but never produced by any stage or passed via constructor args/context",
                    read_var
                ),
            ));
        }
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
    let order_map: HashMap<Ident, usize> = stages
        .iter()
        .enumerate()
        .map(|(idx, stage)| (stage.sig.ident.clone(), idx))
        .collect();
    let sorted_stages = topological_sort(&stage_infos, order_map)?;

    // Report unused outputs: variables written but never read by any stage
    let all_reads: std::collections::HashSet<String> = stage_infos
        .values()
        .flat_map(|s| s.inputs.iter().cloned())
        .collect();
    for (var_name, writer_stage) in &var_writers {
        if !all_reads.contains(var_name)
            && !output_unused.contains(var_name)
            && let Some(span) = writer_spans.get(var_name)
        {
            return Err(syn::Error::new(
                *span,
                format!(
                    "variable '{}' is written by stage '{}' but never read by any stage",
                    var_name, writer_stage
                ),
            ));
        }
    }

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
                if let FnArg::Typed(PatType { attrs, pat, ty, .. }) = input {
                    if let syn::Pat::Ident(pat_ident) = &**pat {
                        // Determine whether the parameter is mutable or not
                        let is_mut = matches!(
                            &**ty,
                            Type::Reference(syn::TypeReference { mutability, .. })
                                if mutability.is_some()
                        );

                        // Context param is passed through as-is
                        if Some(&pat_ident.ident) == context_name {
                            return quote! { #pat_ident };
                        }

                        // Use pipeline field name
                        let target_ident = normalized_target_ident(attrs, &pat_ident.ident);

                        if is_mut {
                            quote! { &mut self.#target_ident }
                        } else {
                            quote! { &self.#target_ident }
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
            quote! { #mod_ident::#stage_name(#(#args),*)?; }
        } else {
            quote! { #mod_ident::#stage_name(#(#args),*); }
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
            if let FnArg::Typed(PatType { attrs, pat, ty, .. }) = input
                && let syn::Pat::Ident(pat_ident) = &**pat
            {
                if Some(&pat_ident.ident) == context_name {
                    continue;
                }
                if let Type::Reference(type_ref) = &**ty
                    && type_ref.mutability.is_some()
                {
                    // skip resettable fields marked #[skip_clear]
                    if has_skip_clear_attr(attrs) {
                        continue;
                    }
                    // existing: get the pipeline field name (respect #[rename])
                    let target_ident = normalized_target_ident(attrs, &pat_ident.ident);
                    outputs.insert(target_ident.to_string());
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

fn topological_sort(
    stages: &HashMap<Ident, StageInfo>,
    order_map: HashMap<Ident, usize>,
) -> syn::Result<Vec<Ident>> {
    // in-degree of each node
    let mut indegree = HashMap::new();
    // adjacency: stage -> dependents
    let mut adj = HashMap::<Ident, Vec<Ident>>::new();
    for (name, info) in stages {
        indegree.insert(name.clone(), info.dependencies.len());
        for dep in &info.dependencies {
            adj.entry(dep.clone()).or_default().push(name.clone());
        }
    }

    // initial zero-in-degree nodes
    let mut zeros: Vec<Ident> = stages
        .keys()
        .filter(|k| indegree[*k] == 0)
        .cloned()
        .collect();

    let mut result = Vec::new();
    while !zeros.is_empty() {
        // pick the earliest-defined stage
        zeros.sort_by_key(|id| order_map[id]);
        let node = zeros.remove(0);
        result.push(node.clone());
        if let Some(children) = adj.get(&node) {
            for child in children {
                let e = indegree.get_mut(child).unwrap();
                *e -= 1;
                if *e == 0 {
                    zeros.push(child.clone());
                }
            }
        }
    }
    if result.len() != stages.len() {
        return Err(syn::Error::new(
            Span::call_site(),
            "Cycle detected in stage dependencies",
        ));
    }
    Ok(result)
}

fn generate_puml(stages: &[ItemFn], context_name: Option<&Ident>) -> String {
    use std::collections::HashSet;

    let mut puml = String::new();
    puml.push_str("@startuml\n");
    puml.push_str("skinparam linetype ortho\n");
    puml.push_str("left to right direction\n");

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

    for stage in stages {
        let stage_name = stage.sig.ident.to_string();
        puml.push_str(&format!("class {} <<Stage>>\n", stage_name));

        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType { attrs, pat, ty, .. }) = input
                && let syn::Pat::Ident(pat_ident) = &**pat
            {
                if Some(&pat_ident.ident) == context_name {
                    continue;
                }
                let target_ident = normalized_target_ident(attrs, &pat_ident.ident);
                let var_name = target_ident.to_string();
                let puml_var_name = format!("${}", var_name);

                if variables.insert(var_name.clone()) {
                    puml.push_str(&format!("class \"{}\" <<Variable>>\n", puml_var_name));
                }

                let is_mut = matches!(&**ty, Type::Reference(syn::TypeReference { mutability, .. }) if mutability.is_some());
                if is_mut {
                    puml.push_str(&format!("{} --> \"{}\"\n", stage_name, puml_var_name));
                } else {
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

    let mut nodes = Vec::new();
    let mut edges = Vec::new();
    let mut variables = HashSet::new();
    let mut output_vars = HashSet::new();

    for stage in stages {
        let stage_name = stage.sig.ident.to_string();

        nodes.push(json!({
            "id": stage_name,
            "label": stage_name,
            "group": "stage"
        }));

        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType { attrs, pat, ty, .. }) = input
                && let syn::Pat::Ident(pat_ident) = &**pat
            {
                if Some(&pat_ident.ident) == context_name {
                    continue;
                }
                let target_ident = normalized_target_ident(attrs, &pat_ident.ident);
                let var_name = target_ident.to_string();
                let vis_var_name = format!("${}", var_name);

                if variables.insert(var_name.clone()) {
                    nodes.push(json!({
                        "id": vis_var_name,
                        "label": var_name,
                        "group": "variable"
                    }));
                }

                let is_mut = matches!(&**ty, Type::Reference(syn::TypeReference { mutability, .. }) if mutability.is_some());
                if is_mut {
                    edges.push(json!({ "from": stage_name, "to": vis_var_name, "arrows": "to" }));
                    output_vars.insert(var_name.clone());
                } else {
                    edges.push(json!({ "from": vis_var_name, "to": stage_name, "arrows": "to" }));
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
    // Parse the function, remove `rename` attributes from parameters, and emit it unchanged.
    let mut func = parse_macro_input!(item as ItemFn);
    for input in func.sig.inputs.iter_mut() {
        if let FnArg::Typed(pat_type) = input {
            pat_type.attrs.retain(|attr| {
                let last = attr.path().segments.last().map(|s| &s.ident);
                last != Some(&Ident::new("rename", Span::call_site()))
                    && last != Some(&Ident::new("skip_clear", Span::call_site()))
                    && last != Some(&Ident::new("unused", Span::call_site()))
            });
        }
    }
    TokenStream::from(quote! { #func })
}

fn is_stage_attr(attr: &syn::Attribute) -> bool {
    attr.path()
        .segments
        .last()
        .is_some_and(|seg| seg.ident == "stage")
}

fn has_skip_clear_attr(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| {
        // Detect both #[skip_clear] and namespaced forms like #[pipeline::skip_clear]
        attr.path()
            .segments
            .last()
            .is_some_and(|seg| seg.ident == "skip_clear")
    })
}

fn has_unused_attr(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| {
        attr.path()
            .segments
            .last()
            .is_some_and(|seg| seg.ident == "unused")
    })
}

fn normalized_target_ident(attrs: &[Attribute], pat_ident: &Ident) -> Ident {
    if let Some(new_name) = get_rename_attr(attrs) {
        return format_ident!("{}", new_name);
    }
    let s = pat_ident.to_string();
    let trimmed = s.trim_start_matches('_');
    if trimmed.is_empty() {
        // Parameter is just "_" or "__": keep as-is so it won't accidentally
        // alias anything. Users can use #[rename="..."] if they want linkage.
        pat_ident.clone()
    } else {
        format_ident!("{}", trimmed)
    }
}

// Import Spanned for error reporting
use syn::spanned::Spanned;

// Extracts #[rename = "field"] or #[rename("field")] into Some("field"), else None.
fn get_rename_attr(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        if !attr.path().is_ident("rename") {
            continue;
        }

        match &attr.meta {
            // #[rename = "field"]
            Meta::NameValue(MetaNameValue {
                value:
                    Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }),
                ..
            }) => {
                return Some(s.value());
            }
            // #[rename("field")]  (tokens are the inside of the parens)
            Meta::List(list) => {
                // Parse the token stream as a comma-separated list of literals and
                // take the first string literal.
                let parser = Punctuated::<Lit, syn::Token![,]>::parse_terminated;
                if let Ok(punct) = parser.parse2(list.tokens.clone())
                    && let Some(Lit::Str(s)) = punct.first()
                {
                    return Some(s.value());
                }
            }
            _ => {}
        }
    }
    None
}
