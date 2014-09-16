Generalised bifunctors - that is, bifunctors which can be in any category, as opposed to just (->).
A bifunctor (short for binary functor, that is 2-ary) or functor of two variables is simply a functor whose domain is the product of two categories.

For for $C_1$, $C_2$ and $D$ categories, a functor

\[ F : C_1 \times C_2 \to D \]
is also called a bifunctor from $C_1$ and $C_2$ to $D$.

\begin{code}
{-#LANGUAGE PolyKinds #-}
{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE FunctionalDependencies#-}
{-#LANGUAGE NoImplicitPrelude#-}
module GHC.Arrows.Experimental.GBifunctor where
import Control.Category
import Data.Either

    -- | Minimal definition: @bimap@, @left@, @right@
class (Category r, Category s, Category t) => Bifunctor p r s t | p r -> s t, p s -> r t, p t -> r s, p r s -> t where
    {-# MINIMAL bimap, construct #-}
    bimap :: r a b -> s c d -> t (p a c) (p b d)
    left :: r a b -> t (p a c) (p b c)
    left f = bimap f id
    right :: s a b -> t (p c a) (p c b)
    right f = bimap id f
    construct :: a -> b -> p a b

instance Bifunctor (,) (->) (->) (->) where
    bimap f g (a,b)= (f a, g b)
    construct a b = (a,b)

instance Bifunctor Either (->) (->) (->) where
    bimap f _ (Left a) = Left (f a)
    bimap _ g (Right a) = Right (g a)
\end{code}
