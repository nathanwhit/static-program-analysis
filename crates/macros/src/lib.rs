use proc_macro::TokenStream;
use proc_macro2::TokenStream as Toks;
use quote::quote;

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
