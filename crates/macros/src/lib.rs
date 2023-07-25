use proc_macro::TokenStream;
use proc_macro2::TokenStream as Toks;
use quote::{quote, ToTokens};
use synstructure::decl_derive;

#[proc_macro_attribute]
pub fn derive_common(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input: Toks = input.into();
    quote! {
        #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize))]
        #input
    }
    .into()
}

decl_derive!(
    [SuperFoldable, attributes(fold)] =>
    super_foldable_derive
);

decl_derive!(
    [Foldable, attributes(fold)] =>
    foldable_derive
);

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

            // retain value of fields with #[fold(identity)]
            bind.ast().attrs.iter().for_each(|x| {
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
                    crate::fold::Foldable::try_fold_with(#bind, __folder)?
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
                __folder: &mut __F
            ) -> Result<Self, __F::Error> {
                Ok(match self { #fold_body })
            }
        }
    })
}
