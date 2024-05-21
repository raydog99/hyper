pub fn s<F, G, X>(f: F, g: G, x: X) -> <F as Fn(X, <G as Fn(X)>)>(X, <G as Fn(X)>)
where
    F: Fn(X, <G as Fn(X)>),
    G: Fn(X),
{
    f(x, g(x))
}