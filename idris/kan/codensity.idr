module Codensity

import Data.Monoid

%default total

Codensity : (Type -> Type) -> Type -> Type
Codensity m a = ((a -> m b) -> m b)

return : Monoid m => a -> Codensity m a
return x = \k => k x

bind : Monoid m => Codensity m a -> (a -> Codensity m b) -> Codensity m b
bind m f = \k => m (\x => (f x) k)

map : Monoid m => (a -> b) -> Codensity m a -> Codensity m b
map f m = \k => m (\x => k (f x))

lowerCodensity : Monoid m => Codensity m a -> m a
lowerCodensity m = m id

reset : Monoid m => Codensity m a -> m a
reset = lowerCodensity

shift : Monoid m => ((a -> Codensity m b) -> Codensity m a) -> Codensity m a
shift f = \k => let aux : Codensity m a
                     aux = f (\x => return x)
                 in aux k