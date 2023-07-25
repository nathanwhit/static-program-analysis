pub use macros::{derive_common, Foldable, SuperFoldable};

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
