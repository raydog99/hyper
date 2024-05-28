use std::ops::Deref;

struct Id<T>(T);

impl<T> Deref for Id<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

struct Compose<F, G>(F, G);

impl<A, B, C, F, G> Fn<(A,)> for Compose<F, G>
where
    F: Fn<(B,)> -> C,
    G: Fn<(A,)> -> B,
{
    extern "rust-call" fn call(&self, (a,): (A,)) -> C {
        (self.0)((self.1)((a,)))
    }
}

struct Tensor<A, B>(A, B);

struct TensorMor<F, G>(F, G);

impl<A, A_, B, B_, F, G> Fn<(Tensor<A, B>,)> for TensorMor<F, G>
where
    F: Fn<(A,)> -> A_,
    G: Fn<(B,)> -> B_,
{
    extern "rust-call" fn call(&self, (a, b): (Tensor<A, B>,)) -> Tensor<A_, B_> {
        Tensor((self.0)((a.0,)), (self.1)((b.0,)))
    }
}