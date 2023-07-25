pub use macros::derive_common;

pub trait Boxed {
    fn into_box(self) -> Box<Self>;
}

impl<T> Boxed for T {
    fn into_box(self) -> Box<Self> {
        Box::new(self)
    }
}
