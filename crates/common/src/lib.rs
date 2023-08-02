extern crate self as common;

use core::fmt;
use std::{borrow::Cow, fmt::Debug, hash::Hash};

pub use la_arena;
pub use macros::{derive_common, DebugWithCtx, Foldable, IntoOwned, SuperFoldable};

pub trait Boxed {
    fn into_box(self) -> Box<Self>;
}

impl<T> Boxed for T {
    fn into_box(self) -> Box<Self> {
        Box::new(self)
    }
}

pub mod never {
    #[derive(Copy, Clone)]
    pub enum Never {}

    impl<T> PartialEq<T> for Never {
        fn eq(&self, _: &T) -> bool {
            unreachable(*self)
        }
    }

    impl Eq for Never {}

    impl<T> PartialOrd<T> for Never {
        fn partial_cmp(&self, _other: &T) -> Option<std::cmp::Ordering> {
            unreachable(*self)
        }
    }

    impl Ord for Never {
        fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
            unreachable(*self)
        }
    }

    impl std::fmt::Display for Never {
        fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            unreachable(*self)
        }
    }

    impl std::fmt::Debug for Never {
        fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            unreachable(*self)
        }
    }

    impl std::error::Error for Never {}

    #[inline(always)]
    pub fn unreachable(never: Never) -> ! {
        match never {}
    }

    pub trait ResultNeverExt<T> {
        fn unwrap_never(self) -> T;
    }

    impl<T> ResultNeverExt<T> for Result<T, Never> {
        fn unwrap_never(self) -> T {
            match self {
                Ok(t) => t,
                Err(never) => unreachable(never),
            }
        }
    }
}

pub fn trim_indent(mut text: &str) -> String {
    if text.starts_with('\n') {
        text = &text[1..];
    }
    let indent = text
        .lines()
        .filter(|it| !it.trim().is_empty())
        .map(|it| it.len() - it.trim_start().len())
        .min()
        .unwrap_or(0);
    text.split_inclusive('\n')
        .map(|line| {
            if line.len() <= indent {
                line.trim_start_matches(' ')
            } else {
                &line[indent..]
            }
        })
        .collect()
}

pub trait DebugWithCtx {
    type Ctx;

    fn fmt_with_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &Self::Ctx) -> fmt::Result;
}

pub struct DebugWrapper<'a, 'c, T, C>(&'a T, &'c C);

impl<'a, 'c, T, C> DebugWrapper<'a, 'c, T, C> {
    pub fn new(v: &'a T, ctx: &'c C) -> Self {
        Self(v, ctx)
    }
}

impl<'a, 'c, T, C> fmt::Debug for DebugWrapper<'a, 'c, T, C>
where
    T: DebugWithCtx<Ctx = C>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt_with_ctx(f, self.1)
    }
}

impl<T: DebugWithCtx> DebugWithCtx for Vec<T> {
    type Ctx = T::Ctx;

    fn fmt_with_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &Self::Ctx) -> fmt::Result {
        f.debug_list()
            .entries(self.iter().map(|it| DebugWrapper::new(it, ctx)))
            .finish()
    }
}

impl<T: DebugWithCtx> DebugWithCtx for Option<T> {
    type Ctx = T::Ctx;

    fn fmt_with_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &Self::Ctx) -> fmt::Result {
        match self {
            Some(it) => f
                .debug_tuple("Some")
                .field(&DebugWrapper::new(it, ctx))
                .finish(),
            None => f.debug_tuple("None").finish(),
        }
    }
}

#[cfg(feature = "serde")]
pub mod serde {
    pub use macros::SerializeWithCtx;

    pub trait SerializeWithCtx {
        type Ctx;

        fn serialize_with_ctx<S>(&self, serializer: S, ctx: &Self::Ctx) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer;
    }

    pub struct SerializeWrapper<'a, 'c, T, C>(&'a T, &'c C);

    impl<'a, 'c, T, C> SerializeWrapper<'a, 'c, T, C> {
        pub fn new(v: &'a T, ctx: &'c C) -> Self {
            Self(v, ctx)
        }
    }

    impl<'a, 'c, T, C> ::serde::Serialize for SerializeWrapper<'a, 'c, T, C>
    where
        T: SerializeWithCtx<Ctx = C>,
    {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            self.0.serialize_with_ctx(serializer, self.1)
        }
    }

    impl<T: SerializeWithCtx> SerializeWithCtx for Vec<T> {
        type Ctx = T::Ctx;

        fn serialize_with_ctx<S>(&self, serializer: S, ctx: &Self::Ctx) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            serializer.collect_seq(self.iter().map(|it| SerializeWrapper::new(it, ctx)))
        }
    }

    impl<T: SerializeWithCtx> SerializeWithCtx for Option<T> {
        type Ctx = T::Ctx;

        fn serialize_with_ctx<S>(&self, serializer: S, ctx: &Self::Ctx) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            match self {
                Some(it) => it.serialize_with_ctx(serializer, ctx),
                None => serializer.serialize_none(),
            }
        }
    }

    impl<T: SerializeWithCtx> SerializeWithCtx for Box<T> {
        type Ctx = T::Ctx;

        fn serialize_with_ctx<S>(&self, serializer: S, ctx: &Self::Ctx) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            (**self).serialize_with_ctx(serializer, ctx)
        }
    }
}

pub trait Get<Idx>
where
    Idx: Lookup,
{
    fn get(&self, idx: Idx) -> &Idx::Output;

    fn get_mut(&mut self, idx: Idx) -> &mut Idx::Output;
}

pub trait Lookup: Sized + Copy {
    type Ctx: Get<Self>;
    type Output;

    fn lookup<'ctx>(&self, ctx: &'ctx Self::Ctx) -> &'ctx Self::Output {
        ctx.get(*self)
    }

    fn lookup_mut<'ctx>(&self, ctx: &'ctx mut Self::Ctx) -> &'ctx mut Self::Output {
        ctx.get_mut(*self)
    }
}

pub struct Idx<T>(la_arena::Idx<T>);

impl<T> Clone for Idx<T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<T> Debug for Idx<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T> Copy for Idx<T> {}

impl<T> PartialEq for Idx<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<T> Eq for Idx<T> {}

impl<T> PartialOrd for Idx<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.into_raw().partial_cmp(&other.0.into_raw())
    }
}

impl<T> Ord for Idx<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.into_raw().cmp(&other.0.into_raw())
    }
}

impl<T> Hash for Idx<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

#[cfg(feature = "serde")]
impl<T> ::serde::Serialize for Idx<T> {
    fn serialize<S: ::serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.into_raw().into_u32().serialize(serializer)
    }
}

impl<T> From<Idx<T>> for la_arena::Idx<T> {
    fn from(value: Idx<T>) -> Self {
        value.0
    }
}

impl<T> From<la_arena::Idx<T>> for Idx<T> {
    fn from(value: la_arena::Idx<T>) -> Self {
        Self(value)
    }
}

pub trait Allocable: Sized {
    type Idx: Copy;

    fn alloc_in<A: Alloc<Self>>(self, arena: &mut A) -> Self::Idx;
}

pub trait Alloc<T: Allocable> {
    fn alloc(&mut self, value: T) -> T::Idx;
}

pub trait IntoOwned {
    type Owned;

    fn into_owned(self) -> Self::Owned;
}

impl<B: ?Sized + ToOwned> IntoOwned for Cow<'_, B> {
    type Owned = B::Owned;

    fn into_owned(self) -> B::Owned {
        self.into_owned()
    }
}
