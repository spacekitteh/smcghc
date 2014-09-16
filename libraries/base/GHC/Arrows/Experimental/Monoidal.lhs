\begin{code}

{-#LANGUAGE NoImplicitPrelude, MultiParamTypeClasses #-}
{-#LANGUAGE PolyKinds, TypeFamilies, TypeOperators #-}
{-#LANGUAGE OverlappingInstances, FlexibleInstances #-}
module GHC.Arrows.Experimental.Monoidal where
import GHC.Arrows.Experimental.Associative
import GHC.Arrows.Experimental.Binoidal
import GHC.Arrows.Experimental.Isomorphism

import Control.Arrow

class (Binoidal k p, Associative k p) => PreMonoidal k p where
    {-# MINIMAL (leftUnitor, rightUnitor) |
     (introduceLeft, introduceRight, eliminateLeft, eliminateRight) #-}
    type Id k p :: *
    leftUnitor :: Isomorphism ((Id k p) `p` b) k b
    leftUnitor = Isomorphism (eliminateLeft, introduceLeft)
    rightUnitor :: Isomorphism (a `p` (Id k p)) k a
    rightUnitor = Isomorphism (eliminateRight, introduceRight)
    eliminateLeft :: ((Id k p) `p` b) `k` b
    eliminateLeft = isoTo leftUnitor
    eliminateRight :: (a `p` (Id k p)) `k` a
    eliminateRight = isoTo rightUnitor
    introduceLeft :: b `k` ((Id k p) `p` b)
    introduceLeft = isoFrom leftUnitor
    introduceRight :: a `k` (a `p` (Id k p))
    introduceRight = isoFrom rightUnitor

instance PreMonoidal (->) (,) where
    type Id (->) (,) = ()
    eliminateLeft ((), b) = b
    eliminateRight (a, ()) = a
    introduceLeft b = ((),b)
    introduceRight a = (a, ())
instance Arrow a => PreMonoidal a (,) where
    type Id a (,) = ()
    eliminateLeft = arr (\((),b) -> b)
    introduceLeft = arr (\b -> ((),b))
    eliminateRight = arr (\(a,()) -> a)
    introduceRight = arr (\a -> (a, ()))

class PreMonoidal k p => Monoidal k p

instance Monoidal (->) (,)

\end{code}
