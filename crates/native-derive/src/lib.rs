use proc_macro::TokenStream;

extern crate proc_macro;

mod native;

#[proc_macro_attribute]
pub fn native(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attr = attr.to_string();
    native::native(attr, item).into()
}
