\begin{code}

{-#LANGUAGE PolyKinds, MultiParamTypeClasses, NoImplicitPrelude #-}

module GHC.Arrows.Experimental.Braided where

import GHC.Arrows.Experimental.Associative
import Data.Either

class Associative k p => Braided k p where
    braid :: k (p a b) (p b a)

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
