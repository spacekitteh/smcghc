Braiding introduces a "swap" function, which switches the objects in a bifunctor. A symmetric braid is when swap . swap = id; that is, when it is its own inverse.



\begin{code}

{-#LANGUAGE PolyKinds, MultiParamTypeClasses, NoImplicitPrelude #-}
{-#LANGUAGE TypeOperators #-}


module GHC.Arrows.Experimental.Braided where

import GHC.Arrows.Experimental.Associative
import GHC.Arrows.Experimental.Isomorphism

import Data.Either

class Associative k p => Braided k p where
    {-#MINIMAL braiding | (braid,unbraid) #-}
    braiding :: Isomorphism (p a b) k (p b a)
    braiding = Isomorphism (braid, unbraid)
    braid :: (p a b) `k` (p b a)
    braid = isoTo braiding
    unbraid :: (p b a) `k` (p a b)
    unbraid = isoFrom braiding

instance Braided (->) Either where
    braid (Left a) = Right a
    braid (Right b) = Left b
    unbraid = braid
instance Braided (->) (,) where
    braid ~(a,b) = (b,a)
    unbraid = braid

class Braided k p => Symmetric k p

swap :: Symmetric k p => (p a b) `k` (p b a)
swap = braid

instance Symmetric (->) Either
instance Symmetric (->) (,)

\end{code}
