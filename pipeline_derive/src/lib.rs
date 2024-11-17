extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, FnArg, Ident, ItemFn, ItemMod, LitStr, PatType, Type,
};

// Import Spanned for error reporting
use syn::spanned::Spanned;

#[proc_macro_attribute]
pub fn pipeline(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse the attribute arguments and the module item
    let attr_args = parse_macro_input!(attr as PipelineArgs);
    let mut module = parse_macro_input!(item as ItemMod);

    let pipeline_name = attr_args.name;
    let constructor_args = attr_args.args;

    // Extract public functions annotated with #[stage]
    let stages = extract_stages(&mut module);

    // Collect unique parameters from all stages
    let (fields, field_names, pipeline_vars) = collect_fields(&stages);

    // Perform Dependency Analysis and Topological Sort
    let compute_calls = match generate_compute_calls(&stages) {
        Ok(calls) => calls,
        Err(e) => return e.to_compile_error().into(),
    };

    // Generate the PUML Diagram Content
    let puml_content = generate_puml(&stages);

    // Generate the struct
    let struct_def = generate_struct(&pipeline_name, &fields, &pipeline_vars);

    // Generate the impl block
    let impl_block = generate_impl(
        &pipeline_name,
        &constructor_args,
        &fields,
        &compute_calls,
        &field_names,
        &puml_content, // Pass the PUML content
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
}

impl Parse for PipelineArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut name = None;
        let mut args = Vec::new();

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
            } else {
                return Err(syn::Error::new_spanned(
                    key,
                    "Expected 'name' or 'args' in pipeline attribute",
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

        Ok(PipelineArgs { name, args })
    }
}

fn extract_stages(module: &mut ItemMod) -> Vec<ItemFn> {
    let mut stages = Vec::new();

    if let Some((_, items)) = &mut module.content {
        for item in items.iter_mut() {
            if let syn::Item::Fn(func) = item {
                if func.attrs.iter().any(|attr| attr.path().is_ident("stage")) {
                    stages.push(func.clone());
                }
            }
        }
    }

    stages
}

use std::collections::{HashMap, HashSet};

fn collect_fields(
    stages: &[ItemFn],
) -> (Vec<(Ident, Type)>, Vec<Ident>, Vec<String>) {
    let mut fields_map = HashMap::new();

    for stage in stages {
        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType { pat, ty, .. }) = input {
                if let syn::Pat::Ident(pat_ident) = &**pat {
                    let ident = pat_ident.ident.clone();

                    // Extract the underlying type (dereference references)
                    let ty = match &**ty {
                        Type::Reference(type_ref) => {
                            (*type_ref.elem).clone()
                        }
                        _ => (**ty).clone(),
                    };

                    fields_map.entry(ident.to_string()).or_insert((ident.clone(), ty));
                }
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

    let pipeline_vars_init = pipeline_vars
        .iter()
        .map(|s| {
            let s_lit = s.to_string();
            quote! { #s_lit }
        })
        .collect::<Vec<_>>();

    quote! {
        pub struct #pipeline_name {
            pub pipeline_vars: [&'static str; #vars_len],
            #(#field_defs),*
        }
    }
}

fn generate_impl(
    pipeline_name: &Ident,
    constructor_args: &[Ident],
    fields: &[(Ident, Type)],
    compute_calls: &[proc_macro2::TokenStream],
    field_names: &[Ident],
    puml_content: &String, // Receive the PUML content
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

    // Generate reset calls
    let reset_calls: Vec<_> = fields
        .iter()
        .map(|(ident, _)| quote! { reset(&mut self.#ident); })
        .collect();

    // Convert the PUML content to a string literal
    let puml_literal = LitStr::new(puml_content, proc_macro2::Span::call_site());

    quote! {
        impl #pipeline_name {
            pub fn new(#(#constructor_params),*) -> Self {
                Self {
                    pipeline_vars: [#(#pipeline_vars_init),*],
                    #(#constructor_inits),*
                }
            }

            pub fn compute(&mut self) -> Result<(), Box<dyn std::error::Error>> {
                #(#compute_calls)*
                self.reset();
                Ok(())
            }

            pub fn reset(&mut self) {
                #(#reset_calls)*
            }

            /// Returns the PlantUML diagram representing the pipeline stages and their dependencies.
            pub fn puml_diagram() -> &'static str {
                #puml_literal
            }
        }
    }
}

fn generate_compute_calls(stages: &[ItemFn]) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    // Map of variable to the function that writes to it
    let mut var_writers: HashMap<String, Ident> = HashMap::new();

    // Map of function name to StageInfo
    let mut stage_infos: HashMap<Ident, StageInfo> = HashMap::new();

    // Collect read and write variables for each stage
    for stage in stages {
        let mut reads = HashSet::new();
        let mut writes = HashSet::new();

        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType { pat, ty, .. }) = input {
                if let syn::Pat::Ident(pat_ident) = &**pat {
                    let var_name = pat_ident.ident.to_string();

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
                                writes.insert(var_name);
                            } else {
                                // Immutable reference - variable is read
                                reads.insert(var_name);
                            }
                        }
                        _ => {
                            // Not a reference, treat as read
                            reads.insert(var_name);
                        }
                    }
                }
            }
        }

        stage_infos.insert(
            stage.sig.ident.clone(),
            StageInfo {
                name: stage.sig.ident.clone(),
                reads,
                writes,
                dependencies: HashSet::new(),
            },
        );
    }

    // Build dependencies between stages
    for stage_info in stage_infos.values_mut() {
        for read_var in &stage_info.reads {
            if let Some(writer_func) = var_writers.get(read_var) {
                if writer_func != &stage_info.name {
                    stage_info.dependencies.insert(writer_func.clone());
                }
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

                        if is_mut {
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

        let call = quote! {
            #stage_name(#(#args),*);
        };

        compute_calls.push(call);
    }

    Ok(compute_calls)
}

struct StageInfo {
    name: Ident,
    reads: HashSet<String>,
    writes: HashSet<String>,
    dependencies: HashSet<Ident>,
}

fn topological_sort(
    stages: &HashMap<Ident, StageInfo>,
) -> syn::Result<Vec<Ident>> {
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

// Generate the PUML diagram as a bipartite graph
fn generate_puml(stages: &[ItemFn]) -> String {
    use std::collections::{HashMap, HashSet};

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
            if let FnArg::Typed(PatType { pat, ty, .. }) = input {
                if let syn::Pat::Ident(pat_ident) = &**pat {
                    let var_name = pat_ident.ident.to_string();

                    // Prefix variable names with a dollar sign
                    let puml_var_name = format!("${}", var_name);

                    // Add the variable as a node with <<Variable>> stereotype if not already added
                    if variables.insert(var_name.clone()) {
                        puml.push_str(&format!("class \"{}\" <<Variable>>\n", puml_var_name));
                    }

                    // Determine the direction of the relationship
                    let is_mut = match &**ty {
                        Type::Reference(syn::TypeReference { mutability, .. }) => {
                            mutability.is_some()
                        }
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
    }

    puml.push_str("@enduml\n");

    puml
}


#[proc_macro_attribute]
pub fn stage(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // This attribute macro does nothing; it just returns the item unchanged.
    item
}
