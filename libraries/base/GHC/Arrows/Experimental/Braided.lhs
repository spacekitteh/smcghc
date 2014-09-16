Braiding introduces a "swap" function, which switches the objects in a bifunctor. A symmetric braid is when swap . swap = id; that is, when it is its own inverse.



\begin{code}

{-#LANGUAGE PolyKinds, MultiParamTypeClasses, NoImplicitPrelude, TypeOperators #-}

module GHC.Arrows.Experimental.Braided where

import GHC.Arrows.Experimental.Associative

import Control.Category
import Data.Either
import Data.Tuple

class Associative k p => Braided k p where
    braiding :: Isomorphism (p a b) k (p b a)
    braiding = Isomorphism (braid, unbraid)
    braid :: (p a b) `k` (p b a)
    braid = isoTo braiding
    unbraid :: (p b a) `k` (p a b)
    unbraid = isoFrom braiding

instance Braided (->) Either where
    braid (Left a) = Right a
    braid (Right b) = Left b

instance Braided (->) (,) where
    braid ~(a,b) = (b,a)

class Braided k p => Symmetric k p

swap :: Symmetric k p => k (p a b) (p b a)
swap = braid

instance Symmetric (->) Either
instance Symmetric (->) (,)

\end{code}
