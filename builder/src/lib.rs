use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, Data, DataStruct,
    DeriveInput, Field, Fields, FieldsNamed, GenericArgument, Ident, Lit, Meta, MetaList,
    MetaNameValue, NestedMeta, Path, PathArguments, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let builder = Ident::new(&format!("{}Builder", name), name.span());

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

    let struct_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        let attrs = match parse_attrs(field) {
            Ok(attrs) => attrs,
            Err(e) => return e,
        };
        if inner_ty(ty, "Option").is_some() || attrs.is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: ::core::option::Option<#ty> }
        }
    });
    let empty_struct = fields.iter().map(|field| {
        let name = &field.ident;
        let attrs = match parse_attrs(field) {
            Ok(attrs) => attrs,
            Err(e) => return e,
        };
        if attrs.is_some() {
            quote! { #name: Vec::new() }
        } else {
            quote! { #name: core::option::Option::None }
        }
    });
    let methods = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        let attrs = match parse_attrs(field) {
            Ok(attrs) => attrs,
            Err(e) => return e,
        };
        if let Some(ident) = attrs {
            let inner_ty = inner_ty(ty, "Vec").unwrap();
            quote! {
                fn #ident(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name.push(#name);
                    self
                }
            }
        } else {
            if let Some(inner_ty) = inner_ty(ty, "Option") {
                quote! {
                    fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                        self.#name = core::option::Option::Some(#name);
                        self
                    }
                }
            } else {
                quote! {
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = core::option::Option::Some(#name);
                        self
                    }
                }
            }
        }
    });
    let build_method_extractors = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        let attrs = match parse_attrs(field) {
            Ok(attrs) => attrs,
            Err(e) => return e,
        };
        if inner_ty(ty, "Option").is_some() || attrs.is_some() {
            quote! { let #name = self.#name.clone(); }
        } else {
            quote! {
                let #name = if let core::option::Option::Some(#name) = &self.#name {
                    #name.clone()
                } else {
                    return Err(String::from("all fields required").into())
                };
            }
        }
    });
    let build_method_creators = fields.iter().map(|field| {
        let name = &field.ident;
        quote! { #name }
    });

    let expanded = quote! {
        impl #name {
            pub fn builder() -> #builder {
                #builder {
                    #(#empty_struct),*
                }
            }
        }

        pub struct #builder {
            #(#struct_fields),*
        }

        impl #builder {
            #(#methods)*

            pub fn build(&mut self) -> ::core::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {
                #(#build_method_extractors)*
                Ok(#name {
                    #(#build_method_creators),*
                })
            }
        }
    };

    TokenStream::from(expanded)
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

fn parse_attrs(field: &Field) -> Result<Option<Ident>, proc_macro2::TokenStream> {
    if field.attrs.len() > 0 {
        if let Ok(Meta::List(MetaList { nested, path, .. })) = field.attrs[0].parse_meta() {
            if path.get_ident().unwrap().to_string() == String::from("builder") {
                if let NestedMeta::Meta(Meta::NameValue(MetaNameValue { path, lit, .. })) =
                    &nested[0]
                {
                    if path.get_ident().unwrap().to_string() == String::from("each") {
                        if let Lit::Str(lit) = lit {
                            return Ok(Some(Ident::new(&lit.value(), path.span())));
                        }
                    } else {
                        return Err(syn::Error::new(
                            path.span(),
                            r#"expected `builder(each = "...")`"#,
                        )
                        .to_compile_error());
                    }
                }
            }
        }
    }
    Ok(None)
}
