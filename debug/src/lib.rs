use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, AngleBracketedGenericArguments, Data,
    DataStruct, DeriveInput, Field, Fields, FieldsNamed, GenericArgument, Generics, Lit, Meta,
    MetaNameValue, Path, PathArguments, PathSegment, Type, TypeParam, TypePath,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { named, .. }),
        ..
    }) = &input.data
    {
        named
    } else {
        panic!("Must be a struct");
    };

    for field in fields {
        if let Err(e) = parse_attrs(field) {
            return e.into();
        }
    }

    let type_fields: Vec<_> = fields.iter().map(|field| field.ty.clone()).collect();
    let new_generics = add_where_clause(input.generics, type_fields);
    let (impl_generics, ty_generics, where_clause) = new_generics.split_for_impl();

    let name_string = name.to_string();
    let field_calls = fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        let ident_str = ident.to_string();
        if let Ok(Some(f)) = parse_attrs(field) {
            quote! { .field(#ident_str, &format_args!(#f, self.#ident)) }
        } else {
            quote! { .field(#ident_str, &self.#ident) }
        }
    });

    let expanded = quote! {
        impl #impl_generics ::std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(
                &self,
                f: &mut ::std::fmt::Formatter<'_>
            ) -> ::std::result::Result<(), ::std::fmt::Error> {
                f.debug_struct(#name_string)
                    #(#field_calls)*
                    .finish()
            }
        }
    };

    TokenStream::from(expanded)
}

fn parse_attrs(field: &Field) -> Result<Option<String>, proc_macro2::TokenStream> {
    if field.attrs.len() > 0 {
        if let Ok(Meta::NameValue(MetaNameValue { path, lit, .. })) = field.attrs[0].parse_meta() {
            if path.get_ident().unwrap().to_string() == String::from("debug") {
                if let Lit::Str(lit) = lit {
                    return Ok(Some(lit.value()));
                }
            } else {
                return Err(
                    syn::Error::new(path.span(), r#"expected `debug = "..."`"#).to_compile_error()
                );
            }
        }
    }
    Ok(None)
}

fn add_where_clause(mut generics: Generics, fields: Vec<Type>) -> Generics {
    let only_phantoms = only_phantoms(&generics, &fields);
    let where_clause = generics.make_where_clause();
    for field in &fields {
        if !only_phantoms.contains(&field) {
            where_clause
                .predicates
                .push(parse_quote! { #field: ::std::fmt::Debug });
        }
    }
    generics
}

fn only_phantoms<'a>(generics: &Generics, fields: &'a Vec<Type>) -> Vec<&'a Type> {
    let mut phantom_types = Vec::new();
    let type_params: Vec<_> = generics.type_params().collect();
    for type_param in type_params {
        let filtered: Vec<_> = fields
            .iter()
            .filter(|field| type_contains_generic(field, type_param))
            .collect();

        let length = filtered.len();
        if length > 0 {
            let phantoms: Vec<_> = filtered
                .into_iter()
                .filter(|field| inner_ty(field, "PhantomData").is_some())
                .collect();
            if phantoms.len() == length {
                phantom_types.extend(phantoms);
            }
        }
    }
    phantom_types
}

fn type_contains_generic(ty: &Type, generic: &TypeParam) -> bool {
    inner_most_types(ty)
        .into_iter()
        .find(|t| match t {
            Type::Path(TypePath { path, .. }) => path.segments[0].ident == generic.ident,
            _ => false,
        })
        .is_some()
}

fn inner_most_types(ty: &Type) -> Vec<&Type> {
    fn inner_most_types<'a>(ty: &'a Type, types: &mut Vec<&'a Type>) {
        if let Type::Path(ref type_path) = ty {
            if let Some(PathSegment { arguments, .. }) = type_path.path.segments.last() {
                if arguments.is_empty() {
                    types.push(ty);
                } else if let PathArguments::AngleBracketed(arguments) = arguments {
                    for argument in arguments.args.iter() {
                        if let GenericArgument::Type(ty) = argument {
                            inner_most_types(ty, types);
                        }
                    }
                }
            }
        }
    }
    let mut types = Vec::default();
    inner_most_types(ty, &mut types);
    types
}

fn inner_ty<'a>(ty: &'a Type, outer: &str) -> Option<&'a Type> {
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = ty
    {
        if segments[0].ident == outer {
            if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
                &segments[0].arguments
            {
                if let GenericArgument::Type(t) = &args[0] {
                    return Some(t);
                }
            }
        }
    }
    None
}
