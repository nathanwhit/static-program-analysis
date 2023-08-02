use std::{collections::HashSet, ops::Sub};

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as Toks};
use proc_macro_error::abort;
use quote::{quote, ToTokens};
use syn::{parenthesized, parse::Parse, parse_macro_input, Token};
use synstructure::{decl_derive, VariantInfo};

fn make_ident(s: impl AsRef<str>) -> Ident {
    Ident::new(s.as_ref(), Span::call_site())
}

#[proc_macro_attribute]
pub fn derive_common(attr: TokenStream, input: TokenStream) -> TokenStream {
    let input: Toks = input.into();
    let mut excluded = HashSet::new();
    let meta_parser = syn::meta::parser(|meta| {
        if meta.path.is_ident("except") {
            let content;
            let _open_paren = parenthesized!(content in meta.input);
            let values = content.parse_terminated(Ident::parse, Token![,])?;
            excluded.extend(values.iter().map(|id| id.to_string()));
            Ok(())
        } else {
            Err(meta.error("unsupported attribute arg"))
        }
    });
    parse_macro_input!(attr with meta_parser);
    let derives = HashSet::from(
        [
            "Clone",
            "Debug",
            "PartialEq",
            "Eq",
            "PartialOrd",
            "Ord",
            "Hash",
        ]
        .map(String::from),
    );
    let remaining = derives.sub(&excluded);
    let derives = remaining.into_iter().map(make_ident);
    let serialize = if excluded.contains("Serialize") {
        None
    } else {
        Some(quote!(#[cfg_attr(any(test, feature = "serde"), derive(serde::Serialize))]))
    };
    quote! {
        #[derive( #(#derives),* )]
        #serialize
        #input
    }
    .into()
}

decl_derive!(
    [DebugWithCtx, attributes(debug, id, ctx)] =>
    debug_with_ctx_derive
);

decl_derive!(
    [SerializeWithCtx, attributes(id, ctx)] =>
    serialize_with_ctx_derive
);

decl_derive!(
    [SuperFoldable, attributes(fold, id, ctx)] =>
    super_foldable_derive
);

decl_derive!(
    [Foldable, attributes(fold, id, ctx)] =>
    foldable_derive
);

decl_derive!(
    [IntoOwned, attributes(borrowed, owned)] =>
    #[proc_macro_error::proc_macro_error]
    into_owned_derive
);

fn into_owned_derive(mut s: synstructure::Structure<'_>) -> Toks {
    if let syn::Data::Union(_) = s.ast().data {
        panic!("`#[derive(DebugWithCtx)]` does not support unions");
    }
    let owned = s.ast().attrs.iter().find_map(|attr| {
        if attr.path().is_ident("owned") {
            Some(attr.parse_args::<syn::Path>().unwrap())
        } else {
            None
        }
    });

    let Some(owned) = owned else {
        abort!(s.ast().ident, "missing `#[owned]` attribute")
    };

    s.add_bounds(synstructure::AddBounds::Generics);
    s.bind_with(|_| synstructure::BindStyle::Move);

    let body = s.each_variant(|vi| {
        let bindings = vi.bindings();
        vi.construct(|_, index| {
            let bind = &bindings[index];

            let borrowed = bind.ast().attrs.iter().find_map(|x| {
                if x.path().is_ident("borrowed") {
                    match x.parse_args::<syn::Path>() {
                        Ok(path) => return Some(path),
                        Err(e) => abort!(x, e),
                    };
                }
                None
            });

            if let Some(borrowed) = borrowed {
                quote! {
                    #borrowed(#bind.into_owned())
                }
            } else {
                quote! {
                    #bind
                }
            }
        })
    });
    s.add_bounds(synstructure::AddBounds::None)
        .gen_impl(quote! {
            extern crate common;

            gen impl common::IntoOwned for @Self {
                type Owned = #owned;
                fn into_owned(self) -> <Self as common::IntoOwned>::Owned {
                    match self {
                        #body
                    }
                }
            }
        })
}

fn debug_with_ctx_derive(mut s: synstructure::Structure<'_>) -> Toks {
    if let syn::Data::Union(_) = s.ast().data {
        panic!("`#[derive(DebugWithCtx)]` does not support unions");
    }

    s.add_bounds(synstructure::AddBounds::Generics);
    s.bind_with(|_| synstructure::BindStyle::Ref);
    let debug_body = s.each_variant(|vi| {
        let name = vi.ast().ident.to_string();
        let is_tuple = !matches!(vi.ast().fields, syn::Fields::Named(_));
        let mut ts = if is_tuple {
            quote!(f.debug_tuple(#name))
        } else {
            quote!(f.debug_struct(#name))
        };
        for bind in vi.bindings() {
            let mut needs_ctx = false;

            bind.ast().attrs.iter().for_each(|x| {
                if x.path().is_ident("id") {
                    needs_ctx = true;
                    return;
                }
                if x.path().is_ident("ctx") {
                    needs_ctx = true;
                    return;
                }
                if !x.path().is_ident("debug") {
                    return;
                }
                let _ = x.parse_nested_meta(|nested| {
                    if nested.path.is_ident("id") {
                        needs_ctx = true;
                    }
                    Ok(())
                });
            });
            let field_name = bind.binding.to_string();
            let field_toks = if needs_ctx {
                let wrap = quote!(&<::common::DebugWrapper<_, _>>::new(#bind, ctx));
                if is_tuple {
                    quote! {
                        .field(#wrap)
                    }
                } else {
                    quote! {
                        .field(#field_name, #wrap)
                    }
                }
            } else {
                if is_tuple {
                    quote! {
                        .field(#bind)
                    }
                } else {
                    quote! {
                        .field(#field_name, #bind)
                    }
                }
            };

            ts.extend(field_toks);
        }

        quote! {
                #ts
                .finish()
        }
    });

    s.bound_impl(quote!(::common::DebugWithCtx), quote! {
        type Ctx = ::ast::AstCtx;
        fn fmt_with_ctx(&self, f: &mut ::std::fmt::Formatter<'_>, ctx: &Self::Ctx) -> ::std::fmt::Result {
            match self {
                #debug_body
            }
        }
    })
}

#[derive(Clone, Copy)]
enum Style {
    Tuple,
    Struct,
    Unit,
}

fn style(vi: &VariantInfo<'_>) -> Style {
    match vi.ast().fields {
        syn::Fields::Named(_) => Style::Struct,
        syn::Fields::Unnamed(_) => Style::Tuple,
        syn::Fields::Unit => Style::Unit,
    }
}

fn serialize_with_ctx_derive(mut s: synstructure::Structure<'_>) -> Toks {
    if let syn::Data::Union(_) = s.ast().data {
        panic!("`#[derive(DebugWithCtx)]` does not support unions");
    }

    s.add_bounds(synstructure::AddBounds::Generics);
    s.bind_with(|_| synstructure::BindStyle::Ref);
    let s_name = s.ast().ident.to_string();
    let is_enum = matches!(s.ast().data, syn::Data::Enum(_));
    let mut idx: u32 = 0;
    let serialize_body = s.each_variant(|vi| {
        let name = vi.ast().ident.to_string();
        let len = vi.bindings().len();
        let var_style = style(vi);
        let mut ts = match var_style {
            Style::Tuple => {
                if is_enum {
                    quote! {
                        let mut _s = serializer.serialize_tuple_variant(#s_name, #idx, #name, #len)?;
                    }
                } else {
                    quote! {
                        let mut _s = serializer.serialize_tuple_struct(#len)?;
                    }
                }
            }
            Style::Struct => {
                if is_enum {
                    quote! {
                        let mut _s = serializer.serialize_struct_variant(#s_name, #idx, #name, #len)?;
                    }
                } else {
                    quote! {
                        let mut _s = serializer.serialize_struct(#name, #len)?;
                    }
                }
            }
            Style::Unit => {
                if is_enum {
                    quote! {
                        serializer.serialize_unit_variant(#s_name, #idx, #name)
                    }
                } else {
                    quote! {
                        serializer.serialize_unit_struct(#name)
                    }
                }
            }
        };
        for bind in vi.bindings() {
            let mut needs_ctx = false;

            bind.ast().attrs.iter().for_each(|x| {
                if x.path().is_ident("id") {
                    needs_ctx = true;
                    return;
                }
                if x.path().is_ident("ctx") {
                    needs_ctx = true;
                    return;
                }
            });
            
            let field_toks = match var_style {
                Style::Struct => {
                    let field_name = bind.ast().ident.clone().unwrap().to_string();
                    if needs_ctx {
                        quote! {
                            _s.serialize_field(#field_name, &::common::serde::SerializeWrapper::new(#bind, ctx))
                        }
                    } else {
                        quote! {
                            _s.serialize_field(#field_name, #bind)
                        }
                    }
                }
                Style::Tuple => {
                    if needs_ctx {
                        quote! {
                            _s.serialize_field(&::common::serde::SerializeWrapper::new(#bind, ctx))
                        }
                    } else {
                        quote! {
                            _s.serialize_field(#bind)
                        }
                    }
                }
                Style::Unit => {
                    quote! {}
                }
            };

            if field_toks.is_empty() {
                continue;
            }
            ts.extend(quote! {
                #field_toks?;
            });
        }
        idx += 1;

        match var_style {
            Style::Tuple | Style::Struct => {
                ts.extend(quote! {
                    _s.end()
                });
            }
            Style::Unit => {}
        }

        ts
    });

    s.gen_impl(quote! {
        extern crate common;
        #[cfg(any(test, feature = "serde"))]
        extern crate serde;
        #[cfg(any(test, feature = "serde"))]
        use serde::{Serialize, Serializer, ser::{SerializeTupleVariant, SerializeStruct}};

        #[cfg(any(test, feature = "serde"))]
        gen impl ::common::serde::SerializeWithCtx for @Self {
            type Ctx = ::ast::AstCtx;
            fn serialize_with_ctx<S: ::serde::Serializer>(&self, serializer: S, ctx: &Self::Ctx) -> Result<S::Ok, S::Error> {
                match self {
                    #serialize_body
                }
            }
        }
    })
}

fn foldable_derive_common(
    mut s: synstructure::Structure<'_>,
    impl_path: Toks,
    impl_body: impl FnOnce(Toks) -> Toks,
) -> Toks {
    if let syn::Data::Union(_) = s.ast().data {
        panic!("`#[derive(SuperFoldable)]` does not support unions");
    }

    s.add_bounds(synstructure::AddBounds::Generics);
    s.bind_with(|_| synstructure::BindStyle::Move);
    let fold_body = s.each_variant(|vi| {
        let bindings = vi.bindings();
        vi.construct(|_, index| {
            let bind = &bindings[index];

            let mut identity = false;
            let mut needs_ctx = false;

            // retain value of fields with #[fold(identity)]
            bind.ast().attrs.iter().for_each(|x| {
                if x.path().is_ident("id") {
                    needs_ctx = true;
                    return;
                }
                if !x.path().is_ident("fold") {
                    return;
                }
                let _ = x.parse_nested_meta(|nested| {
                    if nested.path.is_ident("identity") {
                        identity = true;
                    }
                    Ok(())
                });
            });

            if identity {
                bind.to_token_stream()
            } else {
                quote! {
                    crate::fold::Foldable::try_fold_with(#bind, ctx, __folder)?
                }
            }
        })
    });
    s.bound_impl(impl_path, impl_body(fold_body))
}

fn super_foldable_derive(s: synstructure::Structure<'_>) -> Toks {
    foldable_derive_common(s, quote!(::ast::fold::SuperFoldable), |fold_body| {
        quote! {
            fn try_super_fold_with<__F: ::ast::fold::FallibleFolder>(
                self,
                ctx: &mut ::ast::AstCtx,
                __folder: &mut __F
            ) -> Result<Self, __F::Error> {
                Ok(match self { #fold_body })
            }
        }
    })
}

fn foldable_derive(s: synstructure::Structure<'_>) -> Toks {
    foldable_derive_common(s, quote!(::ast::fold::Foldable), |fold_body| {
        quote! {
            fn try_fold_with<__F: ::ast::fold::FallibleFolder>(
                self,
                ctx: &mut ::ast::AstCtx,
                __folder: &mut __F
            ) -> Result<Self, __F::Error> {
                Ok(match self { #fold_body })
            }
        }
    })
}
