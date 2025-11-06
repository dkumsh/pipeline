extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro_crate::{FoundCrate, crate_name};
use quote::{format_ident, quote};

use syn::{
    Attribute, Expr, ExprLit, FnArg, Ident, ItemFn, ItemMod, Lit, LitStr, Meta, MetaNameValue,
    PatType, Type,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
};
// Compile the template into the derive crate binary:
const HTML_TEMPLATE: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/pipeline_graph.html"
));

#[proc_macro_attribute]
pub fn pipeline(attr: TokenStream, item: TokenStream) -> TokenStream {
    let main_crate_ident = match crate_name("pipeline") {
        Ok(FoundCrate::Itself) => format_ident!("crate"), // when expanding inside pipeline-dsl itself
        _ => format_ident!("pipeline"),                   // everywhere else (tests, external users)
    };
    // Parse the attribute arguments and the module item
    let attr_args = parse_macro_input!(attr as PipelineArgs);
    let mut module = parse_macro_input!(item as ItemMod);
    let mod_ident = module.ident.clone();

    let pipeline_name = attr_args.name;
    let pipeline_name_str = quote! {#pipeline_name}.to_string();
    let constructor_args = attr_args.args;
    let context_names = &attr_args.context_names;

    // Extract public functions annotated with #[stage]
    let stages = extract_stages(&mut module);

    // Infer the context types from the stages.  Multiple context names are supported.
    let context_params: Vec<(Ident, Type)> = if !attr_args.context_names.is_empty() {
        match infer_context_types(&stages, &attr_args.context_names) {
            Ok(v) => v,
            Err(e) => return e.to_compile_error().into(),
        }
    } else {
        Vec::new()
    };

    // Collect unique parameters from all stages, excluding any context parameters
    let (fields, field_names, pipeline_vars) = collect_fields(&stages, &attr_args.context_names);

    // Perform Dependency Analysis and Topological Sort
    // Pass constructor_args into generate_compute_calls so it can detect missing inputs.
    let compute_calls = match generate_compute_calls(
        &stages,
        context_names,
        &mod_ident,
        &constructor_args,
        attr_args.break_ty.as_ref(),
        attr_args.clear_updated_on_break,
    ) {
        Ok(calls) => calls,
        Err(e) => return e.to_compile_error().into(),
    };

    // Collect the names of variables written by any stage.  Only these need resetting.
    let output_vars = collect_outputs(&stages, context_names);

    // Determine error type: user-specified or default to pipeline::Error
    let error_ty = attr_args
        .error_ty
        .clone()
        .unwrap_or_else(|| parse_quote!(#main_crate_ident::Error));

    // Generate the PUML Diagram Content
    let puml_content = generate_puml(&stages, context_names);

    // Generate the JSON for HTML diagram (nodes/edges only)
    let (nodes_json, edges_json) = generate_html_data(&stages, context_names);

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
        &context_params,
        &output_vars,
        &error_ty,
        attr_args.break_ty.as_ref(),
        &main_crate_ident,
    );

    // Reconstruct the module with stages unmodified
    let output = quote! {
        #module

        #struct_def

        #impl_block
    };

    output.into()
}

/// Parsed arguments for the `#[pipeline]` attribute.  The `context_names` field
/// stores zero or more names for context parameters (e.g. `context = "db, metrics"`).
struct PipelineArgs {
    /// Name of the pipeline container type
    name: Ident,
    /// Names of the pipeline constructor arguments, parameters provided via the `new`.
    /// They are passed to stages as parameters.
    args: Vec<Ident>,
    /// Names of the context parameters provided via the `context` attribute.  The
    /// pipeline macro will infer the type for each name separately.
    context_names: Vec<Ident>,
    error_ty: Option<Type>,
    break_ty: Option<Type>,
    clear_updated_on_break: bool,
}

impl Parse for PipelineArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut name = None;
        let mut args = Vec::new();
        let mut context_names: Vec<Ident> = Vec::new();
        let mut error_ty = None;
        let mut break_ty: Option<Type> = None;
        let mut clear_updated_on_break = false;

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
                context_names = value
                    .value()
                    .split(',')
                    .map(|p| format_ident!("{}", p.trim()))
                    .collect();
            } else if key == "error" {
                let value: LitStr = input.parse()?;
                // Parse the error type from the provided string
                error_ty = Some(syn::parse_str::<Type>(&value.value())?);
            } else if key == "controlflow_break" {
                let value: LitStr = input.parse()?;
                break_ty = Some(syn::parse_str::<Type>(&value.value())?);
            } else if key == "clear_updated_on_break" {
                let value: LitStr = input.parse()?;
                let v = value.value();
                clear_updated_on_break = match v.as_str() {
                    "true" | "True" | "TRUE" => true,
                    "false" | "False" | "FALSE" => false,
                    _ => {
                        return Err(syn::Error::new_spanned(
                            value,
                            "clear_updated_on_break must be 'true' or 'false'",
                        ));
                    }
                };
            } else {
                return Err(syn::Error::new_spanned(
                    key,
                    "Expected 'name', 'args', 'context', 'error', 'controlflow_break', or 'clear_updated_on_break' in pipeline attribute",
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
            context_names,
            error_ty,
            break_ty,
            clear_updated_on_break,
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

/// Infers the types of multiple context parameters for a `#[pipeline]`.
///
/// Given all stage functions (`stages`) and the list of declared context names
/// from `context = "..."` (`context_names`), this:
///
/// - **Normalizes** parameter idents before matching to the context list
///   (so `_db` matches `db`) using `normalized_target_ident`.
/// - **Compares underlying element types** across all stages (i.e., it ignores
///   whether a parameter is `&T` or `&mut T`) to ensure consistency.
/// - **Escalates mutability**: if any stage takes a given context as `&mut T`,
///   the generated `compute` signature will use `&mut T` for that context;
///   otherwise it uses `&T`.
/// - On success, returns a vector of `(Ident, Type)` where `Type` is exactly the
///   reference type to appear in the `compute` signature (`&T` or `&mut T`).
/// - On failure, returns a **precise diagnostic** listing:
///     * any **missing** contexts (declared but not referenced by any stage), and
///     * any **type conflicts** where the **underlying** types differ across stages.
///
/// ### Notes
/// - “Underlying type” means: for a parameter typed `&T` or `&mut T`, we compare `T`.
/// - Mutability differences themselves are OK (handled via escalation); **type**
///   differences in `T` are not.
///
/// Returns:
/// - `Ok(Vec<(Ident, Type)>)` on success,
/// - `Err(syn::Error)` with a detailed message on missing/conflicting contexts.
fn infer_context_types(
    stages: &[ItemFn],
    context_names: &[Ident],
) -> syn::Result<Vec<(Ident, Type)>> {
    use std::collections::{BTreeMap, BTreeSet};
    use syn::{FnArg, PatType, TypeReference};

    // Per-context accumulator
    struct Seen {
        underlying_types: BTreeSet<String>, // pretty-printed underlying types we've seen
        representative_underlying: Option<Type>,
        needs_mut: bool,
    }

    let mut per_ctx: BTreeMap<String, Seen> = BTreeMap::new();
    for ctx in context_names {
        per_ctx.insert(
            ctx.to_string(),
            Seen {
                underlying_types: BTreeSet::new(),
                representative_underlying: None,
                needs_mut: false,
            },
        );
    }

    // Walk stages and collect type info for each context
    for stage in stages {
        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType {
                attrs,
                pat,
                ty: pty,
                ..
            }) = input
                && let syn::Pat::Ident(pat_ident) = &**pat
            {
                let target = normalized_target_ident(attrs, &pat_ident.ident);
                let key = target.to_string();
                if let Some(seen) = per_ctx.get_mut(&key) {
                    let (under_ty, is_mut) = match pty.as_ref() {
                        Type::Reference(TypeReference {
                            elem, mutability, ..
                        }) => ((**elem).clone(), mutability.is_some()),
                        other => (other.clone(), false),
                    };

                    // Canonicalize the pretty-printed type to reduce false mismatches due to whitespace.
                    let ty_str = quote!(#under_ty)
                        .to_string()
                        .replace(char::is_whitespace, "");
                    seen.underlying_types.insert(ty_str);

                    if seen.representative_underlying.is_none() {
                        seen.representative_underlying = Some(under_ty);
                    }
                    if is_mut {
                        seen.needs_mut = true;
                    }
                }
            }
        }
    }

    // Build result or a precise error
    let mut missing: Vec<String> = Vec::new();
    let mut conflicts: Vec<(String, Vec<String>)> = Vec::new();

    for (ctx_name, seen) in &per_ctx {
        if seen.representative_underlying.is_none() {
            missing.push(ctx_name.clone());
            continue;
        }
        if seen.underlying_types.len() > 1 {
            conflicts.push((
                ctx_name.clone(),
                seen.underlying_types.iter().cloned().collect(),
            ));
        }
    }

    if !missing.is_empty() || !conflicts.is_empty() {
        let mut msg = String::new();
        if !missing.is_empty() {
            msg.push_str("Missing context parameters (not referenced by any stage): ");
            msg.push_str(&missing.join(", "));
            msg.push('\n');
        }
        if !conflicts.is_empty() {
            msg.push_str("Context type inconsistencies detected:\n");
            for (name, tys) in conflicts {
                msg.push_str(&format!(
                    "  - {name}: seen underlying types [{}]\n",
                    tys.join(", ")
                ));
            }
            msg.push_str(
                "Underlying types must match across all stages (mutability may differ).\n",
            );
        }
        return Err(syn::Error::new(proc_macro2::Span::call_site(), msg));
    }

    // Everything consistent: produce the final (& or &mut) context parameter list
    let mut out = Vec::new();
    for ctx in context_names {
        let seen = per_ctx.get(&ctx.to_string()).expect("ctx tracked");
        let under = seen
            .representative_underlying
            .as_ref()
            .expect("validated above");
        let built_ty: Type = if seen.needs_mut {
            syn::parse_quote! { &mut #under }
        } else {
            syn::parse_quote! { & #under }
        };
        out.push((ctx.clone(), built_ty));
    }

    Ok(out)
}

fn collect_fields(
    stages: &[ItemFn],
    context_names: &[Ident],
) -> (Vec<(Ident, Type)>, Vec<Ident>, Vec<String>) {
    let mut fields_map = HashMap::new();

    for stage in stages {
        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType { attrs, pat, ty, .. }) = input
                && let syn::Pat::Ident(pat_ident) = &**pat
            {
                let target = normalized_target_ident(attrs, &pat_ident.ident);
                // Skip any context parameters
                if context_names.contains(&target) {
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
    context_params: &[(Ident, Type)],
    output_vars: &HashSet<String>,
    error_ty: &Type,
    break_ty: Option<&Type>,
    main_crate_ident: &Ident,
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

    // Generate clear calls only for fields that are outputs (written by some stage)
    let clear_updated_calls: Vec<_> = fields
        .iter()
        .filter_map(|(ident, _)| {
            let name = ident.to_string();
            if output_vars.contains(&name) {
                Some(quote! {
                    #main_crate_ident::Reset::reset(&mut self.#ident)
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

    // Prepare compute method signature.  Multiple context parameters are supported.
    let compute_params = if !context_params.is_empty() {
        let params: Vec<_> = context_params
            .iter()
            .map(|(context_name, context_type)| quote! { #context_name: #context_type })
            .collect();
        quote! { &mut self, #(#params),* }
    } else {
        quote! { &mut self }
    };

    // Two different impls depending on whether break_ty is present
    if let Some(bt) = break_ty {
        quote! {

            impl #pipeline_name {
                pub fn new(#(#constructor_params),*) -> Self {
                    Self {
                        pipeline_vars: [#(#pipeline_vars_init),*],
                        #(#constructor_inits),*
                    }
                }
                /// Executes all stages in topological order, with early-exit support.
                pub fn compute(#compute_params) -> Result<::std::ops::ControlFlow<#bt>, #error_ty> {
                    #(#compute_calls)*
                    self.clear_updated_all()?;
                    Ok(::std::ops::ControlFlow::Continue(()))
                }

                /// Clear the update flags on all mutated fields.
                pub fn clear_updated_all(&mut self) -> Result<(), #error_ty> {
                    #(#clear_updated_calls)*
                    Ok(())
                }

                pub fn puml_diagram() -> &'static str {
                    #puml_literal
                }

                pub fn html_diagram() -> String {
                    let template: &str = #html_template_lit;
                    template
                        .replace("{{PIPELINE_NAME}}", #pipeline_name_lit)
                        .replace("{{NODES_JSON}}", #nodes_json_lit)
                        .replace("{{EDGES_JSON}}", #edges_json_lit)
                }

                pub fn write_html_to_file<P: AsRef<std::path::Path>>(
                    file_path: P,
                ) -> std::io::Result<()> {
                    std::fs::write(file_path, Self::html_diagram())
                }
            }
        }
    } else {
        quote! {
            impl #pipeline_name {
                pub fn new(#(#constructor_params),*) -> Self {
                    Self {
                        pipeline_vars: [#(#pipeline_vars_init),*],
                        #(#constructor_inits),*
                    }
                }

                /// Executes all stages in topological order.
                pub fn compute(#compute_params) -> Result<(), #error_ty> {
                    #(#compute_calls)*
                    self.clear_updated_all()?;
                    Ok(())
                }

                /// Clear the update flags on all mutated fields.
                pub fn clear_updated_all(&mut self) -> Result<(), #error_ty> {
                    #(#clear_updated_calls)*
                    Ok(())
                }

                pub fn puml_diagram() -> &'static str {
                    #puml_literal
                }

                pub fn html_diagram() -> String {
                    let template: &str = #html_template_lit;
                    template
                        .replace("{{PIPELINE_NAME}}", #pipeline_name_lit)
                        .replace("{{NODES_JSON}}", #nodes_json_lit)
                        .replace("{{EDGES_JSON}}", #edges_json_lit)
                }

                pub fn write_html_to_file<P: AsRef<std::path::Path>>(
                    file_path: P,
                ) -> std::io::Result<()> {
                    std::fs::write(file_path, Self::html_diagram())
                }
            }
        }
    }
}

/// Returns true if the provided type is `ControlFlow<..., ...>`.
fn is_controlflow_type(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty
        && let Some(seg) = type_path.path.segments.last()
    {
        return seg.ident == "ControlFlow";
    }
    false
}

/// Returns true if the provided type is `Result<ControlFlow<..., ...>, E>`.
fn is_result_of_controlflow(ty: &Type) -> bool {
    use syn::{GenericArgument, PathArguments};
    let Type::Path(tp) = ty else {
        return false;
    };
    let Some(seg) = tp.path.segments.last() else {
        return false;
    };
    if seg.ident != "Result" {
        return false;
    }
    let PathArguments::AngleBracketed(args) = &seg.arguments else {
        return false;
    };
    if let Some(GenericArgument::Type(ok_ty)) = args.args.first() {
        return is_controlflow_type(ok_ty);
    }
    false
}

fn generate_compute_calls(
    stages: &[ItemFn],
    context_names: &[Ident],
    mod_ident: &Ident,
    constructor_args: &[Ident],
    break_ty: Option<&Type>,
    clear_updated_on_break: bool,
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
                let target_ident = normalized_target_ident(attrs, &pat_ident.ident);
                // Skip any context parameter
                if context_names.contains(&target_ident) {
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
        // With multiple context parameters, test membership in the list
        let is_ctx = context_names
            .iter()
            .any(|c| c == &format_ident!("{}", read_var));
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
                        let target_ident = normalized_target_ident(attrs, &pat_ident.ident);
                        // Context param is passed through as-is
                        if context_names.contains(&target_ident) {
                            return quote! { #target_ident };
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

        // Build call based on return type, with optional ControlFlow handling.
        let call = match &stage.sig.output {
            syn::ReturnType::Default => {
                quote! { #mod_ident::#stage_name(#(#args),*); }
            }
            syn::ReturnType::Type(_, ty) => {
                if is_result_of_controlflow(ty) {
                    // Ensure break_ty is present
                    if break_ty.is_none() {
                        return Err(syn::Error::new(
                            ty.span(),
                            "stage returns ControlFlow but pipeline has no `controlflow_break=\"...\"` type declared",
                        ));
                    }
                    let break_cleanup = if clear_updated_on_break {
                        quote! { self.clear_updated_all()?; }
                    } else {
                        quote! {}
                    };
                    quote! {
                        match #mod_ident::#stage_name(#(#args),*)? {
                            ::std::ops::ControlFlow::Continue(()) => {},
                            ::std::ops::ControlFlow::Break(b) => {
                                #break_cleanup
                                return Ok(::std::ops::ControlFlow::Break(b));
                            }
                        }
                    }
                } else if is_controlflow_type(ty) {
                    if break_ty.is_none() {
                        return Err(syn::Error::new(
                            ty.span(),
                            "stage returns ControlFlow but pipeline has no `controlflow_break=\"...\"` type declared",
                        ));
                    }
                    let break_cleanup = if clear_updated_on_break {
                        quote! { self.clear_updated_all()?; }
                    } else {
                        quote! {}
                    };
                    quote! {
                        match #mod_ident::#stage_name(#(#args),*) {
                            ::std::ops::ControlFlow::Continue(()) => {},
                            ::std::ops::ControlFlow::Break(b) => {
                                #break_cleanup
                                return Ok(::std::ops::ControlFlow::Break(b));
                            }
                        }
                    }
                } else if is_result_type(ty) {
                    quote! { #mod_ident::#stage_name(#(#args),*)?; }
                } else {
                    quote! { #mod_ident::#stage_name(#(#args),*); }
                }
            }
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
    context_names: &[Ident],
) -> std::collections::HashSet<String> {
    use std::collections::HashSet;
    let mut outputs = HashSet::new();
    for stage in stages {
        for input in &stage.sig.inputs {
            if let FnArg::Typed(PatType { attrs, pat, ty, .. }) = input
                && let syn::Pat::Ident(pat_ident) = &**pat
            {
                let target_ident = normalized_target_ident(attrs, &pat_ident.ident);
                // Skip context parameters
                if context_names.iter().any(|name| name == &target_ident) {
                    continue;
                }
                if let Type::Reference(type_ref) = &**ty
                    && type_ref.mutability.is_some()
                {
                    // skip resettable fields marked #[skip_reset]
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

fn generate_puml(stages: &[ItemFn], context_names: &[Ident]) -> String {
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
                let target_ident = normalized_target_ident(attrs, &pat_ident.ident);
                // Skip context parameters
                if context_names.iter().any(|name| name == &target_ident) {
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

fn generate_html_data(stages: &[ItemFn], context_names: &[Ident]) -> (String, String) {
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
                let target_ident = normalized_target_ident(attrs, &pat_ident.ident);
                // Skip context parameters
                if context_names.iter().any(|name| name == &target_ident) {
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
                    && last != Some(&Ident::new("skip_reset", Span::call_site()))
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
        // Detect both #[skip_reset] and namespaced forms like #[pipeline::skip_reset]
        attr.path()
            .segments
            .last()
            .is_some_and(|seg| seg.ident == "skip_reset")
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
