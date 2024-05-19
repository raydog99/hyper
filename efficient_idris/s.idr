module SCombinator

s : (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)