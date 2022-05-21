pub use Rewrite::{Clean, Dirty};

/// Trait for types that support a monad-like bind operation.
///
/// `FS` is the type of `Self` wrapped in some effect.
pub trait Bind<FS>: Sized {
    /// Binds an effectful [`FnMut`] to an already wrapped value.
    ///
    /// [`FnMut`]: std::ops::FnMut
    fn bind_mut(wrapped: FS, f: impl FnMut(Self) -> FS) -> FS;
}

/// The trivial effect that does nothing, with binding just applying the
/// function directly.
impl<T> Bind<Self> for T {
    fn bind_mut(wrapped: Self, mut f: impl FnMut(Self) -> Self) -> Self {
        f(wrapped)
    }
}

/// Trait for tree-like structures that can be recursively transformed with
/// effectful computations.
pub trait TreeWalk<FS>: Bind<FS> {
    /// Applies an effectful function to each branch of the tree, wrapping the
    /// final value in the same type of effect.
    fn each_branch(self, f: impl FnMut(Self) -> FS) -> FS;

    /// Applies an effectful function to every node of a tree, including the
    /// root itself, in a bottom-up manner.
    fn bottom_up(self, mut f: impl FnMut(Self) -> FS) -> FS {
        let rest_transformed = self.each_branch(&mut f);
        Bind::bind_mut(rest_transformed, f)
    }
}

/// Enum representing a value that has passed through a transformation that may
/// or may not have affected it.
pub enum Rewrite<T> {
    /// The contained value *was not* affected by the transformation.
    Clean(T),
    /// The contained value *was* affected by the transformation.
    Dirty(T),
}

impl<T> Rewrite<T> {
    /// Returns `true` if the rewrite is [`Dirty`].
    ///
    /// [`Dirty`]: Rewrite::Dirty
    #[must_use]
    pub(crate) fn is_dirty(&self) -> bool {
        matches!(self, Self::Dirty(..))
    }

    /// Takes the contained value, forgetting whether it's [`Clean`] or
    /// [`Dirty`].
    ///
    /// [`Clean`]: Rewrite::Clean
    /// [`Dirty`]: Rewrite::Dirty
    pub fn into_inner(self) -> T {
        match self {
            Clean(clean) => clean,
            Dirty(dirty) => dirty,
        }
    }

    /// Maps a function over `self`, retaining whether it's [`Clean`] or
    /// [`Dirty`].
    ///
    /// [`Clean`]: Rewrite::Clean
    /// [`Dirty`]: Rewrite::Dirty
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Rewrite<U> {
        match self {
            Clean(t) => Clean(f(t)),
            Dirty(t) => Dirty(f(t)),
        }
    }

    /// Repeatedly applies a function until its result is [`Clean`].
    ///
    /// [`Clean`]: Rewrite::Clean
    pub fn repeat(initial: T, mut f: impl FnMut(T) -> Self) -> Self {
        let mut val = Clean(initial);
        loop {
            match val.map(&mut f).transpose() {
                Clean(done) => break done,
                Dirty(keep_going) => val = Dirty(keep_going.into_inner()),
            }
        }
    }

    /// Binds an [`FnOnce`] to `self`.
    ///
    /// [`FnOnce`]: std::ops::FnOnce
    pub fn bind(self, f: impl FnOnce(T) -> Self) -> Self {
        match self {
            Clean(clean) => f(clean),
            Dirty(dirty) => Dirty(f(dirty).into_inner()),
        }
    }
}

impl<T> Rewrite<Rewrite<T>> {
    /// Swaps two layers of [`Rewrite`]
    ///
    /// [`Rewrite`]: Rewrite
    pub fn transpose(self) -> Self {
        match self {
            Clean(inner) => inner.map(Clean),
            Dirty(inner) => inner.map(Dirty),
        }
    }
}

impl<T> Bind<Rewrite<Self>> for T {
    fn bind_mut(
        wrapped: Rewrite<Self>,
        f: impl FnMut(Self) -> Rewrite<Self>,
    ) -> Rewrite<Self> {
        wrapped.bind(f)
    }
}

/// Makes the entire collection [`Dirty`] if any of the elements are.
///
/// [`Dirty`]: Rewrite::Dirty
impl<T, C> FromIterator<Rewrite<T>> for Rewrite<C>
where
    C: FromIterator<T>,
{
    fn from_iter<I: IntoIterator<Item = Rewrite<T>>>(iter: I) -> Self {
        let mut is_dirty = false;
        let collected = iter
            .into_iter()
            .inspect(|item| is_dirty |= item.is_dirty())
            .map(Rewrite::into_inner)
            .collect();
        if is_dirty {
            Dirty(collected)
        } else {
            Clean(collected)
        }
    }
}
