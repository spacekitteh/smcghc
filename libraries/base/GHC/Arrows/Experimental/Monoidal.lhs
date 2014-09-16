\begin{code}

{-#LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, PolyKinds, TypeFamilies #-}

module GHC.Arrows.Experimental.Monoidal where
import GHC.Arrows.Experimental.Associative
import GHC.Arrows.Experimental.Binoidal
import Control.Category
import Data.Either

class (Binoidal k p, Associative k p) => PreMonoidal k p where
    type Id k p :: *
    eliminateLeft :: k (p (Id k p) b) b
    eliminateRight ::k (p a (Id k p)) a
    introduceLeft :: k b (p (Id k p) b)
    introduceRight :: k a (p a (Id k p))

instance PreMonoidal (->) (,) where
    type Id (->) (,) = ()
    eliminateLeft ((), b) = b
    eliminateRight (a, ()) = a
    introduceLeft b = ((),b)
    introduceRight a = (a, ())

class PreMonoidal k p => Monoidal k p

instance Monoidal (->) (,)

\end{code}
