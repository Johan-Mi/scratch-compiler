/// Trait for types that support a monad-like bind operation.
///
/// `FS` is the type of `Self` wrapped in some effect.
pub(crate) trait Bind<FS>: Sized {
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
pub(crate) trait TreeWalk<FS>: Bind<FS> {
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
