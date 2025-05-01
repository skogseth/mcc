use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(Rc<str>);

impl Identifier {
    pub fn new<S: Into<Rc<str>>>(s: S) -> Self {
        Self(s.into())
    }

    pub fn new_temp() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let value = COUNTER.fetch_add(1, Ordering::Relaxed);
        assert_ne!(value, usize::MAX, "max number of temp values exceeded");
        Self::new(format!("tmp.{value}"))
    }

    pub fn new_label(prefix: &str) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let value = COUNTER.fetch_add(1, Ordering::Relaxed);
        assert_ne!(value, usize::MAX, "max number of labels exceeded");
        Self::new(format!("{prefix}.{value}"))
    }

    pub fn new_loop() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let value = COUNTER.fetch_add(1, Ordering::Relaxed);
        assert_ne!(value, usize::MAX, "max number of loops exceeded");
        let string: Rc<str> = format!("loop.{value}").into();
        Self(string)
    }

    pub fn with_prefix(self, prefix: &str) -> Self {
        Self::new(format!("{prefix}{}", self.0))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::ops::Deref for Identifier {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
