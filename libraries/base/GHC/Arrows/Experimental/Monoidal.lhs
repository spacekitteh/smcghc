\begin{code}

{-#LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, PolyKinds, TypeFamilies, TypeOperators #-}

module GHC.Arrows.Experimental.Monoidal where
import GHC.Arrows.Experimental.Associative
import GHC.Arrows.Experimental.Binoidal
import Control.Category
import Data.Either

class (Binoidal k p, Associative k p) => PreMonoidal k p where
    type Id k p :: *
    eliminateLeft :: ((Id k p) `p` b) `k` b
    eliminateRight :: (a `p` (Id k p)) `k` a
    introduceLeft :: b `k` ((Id k p) `p` b)
    introduceRight :: a `k` (a `p` (Id k p))

instance PreMonoidal (->) (,) where
    type Id (->) (,) = ()
    eliminateLeft ((), b) = b
    eliminateRight (a, ()) = a
    introduceLeft b = ((),b)
    introduceRight a = (a, ())

class PreMonoidal k p => Monoidal k p

instance Monoidal (->) (,)

\end{code}
