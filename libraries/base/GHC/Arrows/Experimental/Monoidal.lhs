\begin{code}

{-#LANGUAGE NoImplicitPrelude, MultiParamTypeClasses #-}
{-#LANGUAGE PolyKinds, TypeFamilies, TypeOperators #-}
{-#LANGUAGE OverlappingInstances, FlexibleInstances #-}
{-#LANGUAGE UndecidableInstances, DefaultSignatures#-}
module GHC.Arrows.Experimental.Monoidal where
import GHC.Arrows.Experimental.Associative
import GHC.Arrows.Experimental.Binoidal
import GHC.Arrows.Experimental.GBifunctor
import GHC.Arrows.Experimental.Isomorphism

import Control.Arrow
import Control.Category

class (Binoidal k p, Associative k p) => PreMonoidal k p where
    {-# MINIMAL (leftUnitor, rightUnitor) |
     (introduceLeft, introduceRight, eliminateLeft, eliminateRight) #-}
    type Id k p :: *
    leftUnitor :: Isomorphism k ((Id k p) `p` b) b
    leftUnitor = Isomorphism (eliminateLeft, introduceLeft)
    rightUnitor :: Isomorphism k (a `p` (Id k p)) a
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

class Category k => Reified k where
    --should this perhaps be :: (a -> b) k (k a b)???
    reify :: (a -> b) -> (k a b)
    default reify :: ExtractableReification k => (a -> b) -> (k a b)
    reify = isoTo reification

class Reified k => ExtractableReification k where
    {-#MINIMAL reification | unreify #-}
    -- should this perhaps be (a -> b) k (k a b)???
    reification :: Isomorphism (->) (a -> b)  (k a b)
    reification = Isomorphism (reify, unreify)
    unreify :: (k a b) -> (a -> b)
    unreify = isoFrom reification

instance Arrow a => Reified a where
    reify = arr

class (Reified k, GBifunctor p k k k) => Application k p where
    apply :: k (p (k a b) a) b


instance Application (->) (,) where
    apply (f,x) = f x

class PreMonoidal k p => Trace k p where
    loop :: ((p a c) `k` (p b c)) -> (a `k` b)

instance Trace (->) (,) where
    loop f a = let (b,c) = f (a,c) in b


--TODO
class Terminal k p
class Initial k p
class SemiCartesian k p
class Cartesian k p
class Choice k p


\end{code}
