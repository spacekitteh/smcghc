A premonoidal category is a binoidal category equipped with:

*  an object $I$;
*  for each triple $x,y,z$ of objects, a central isomorphism $\alpha_{x,y,z}\colon (x \otimes y) \otimes z \to x \otimes (y \otimes z)$; and
*  for each object $x$, central isomorphisms $\lambda_x\colon x \otimes I \to x$ and $\rho_x\colon I \otimes x \to x$;

such that the following conditions hold.

*  all possible naturality squares for $\alpha$, $\lambda$, and $\rho$ (which make sense since we have central morphisms) commute.  Note that when written out explicitly in terms of the functors $x\rtimes -$ and $-\ltimes x$, we need three different naturality squares for $\alpha$.
*  the pentagon law holds for $\alpha$, as in a monoidal category.
*  the triangle law holds for $\alpha$, $\lambda$, and $\rho$, as in a monoidal category.

A monoidal category is a premonoidal category where $\otimes$ commutes in time.
\begin{code}

{-#LANGUAGE NoImplicitPrelude, MultiParamTypeClasses #-}
{-#LANGUAGE PolyKinds, TypeFamilies, TypeOperators, ConstraintKinds #-}
{-#LANGUAGE OverlappingInstances, FlexibleInstances, InstanceSigs #-}
{-#LANGUAGE UndecidableInstances, DefaultSignatures#-}
module GHC.Arrows.Experimental.Monoidal where
import GHC.Arrows.Experimental.Associative
import GHC.Arrows.Experimental.Binoidal
import GHC.Arrows.Experimental.GBifunctor
import GHC.Arrows.Experimental.Isomorphism

import Control.Arrow
import Control.Category

import Data.Monoid
import GHC.Num
import GHC.Exts

class (Binoidal k p, Associative k p) => PreMonoidal k p where
    {-# MINIMAL (leftUnitor, rightUnitor) |
     (introduceLeft, introduceRight, eliminateLeft, eliminateRight) #-}
    type ID k p :: *
    leftUnitor :: Isomorphism k ((ID k p) `p` b) b
    leftUnitor = Isomorphism (eliminateLeft, introduceLeft)
    rightUnitor :: Isomorphism k (a `p` (ID k p)) a
    rightUnitor = Isomorphism (eliminateRight, introduceRight)
    eliminateLeft :: ((ID k p) `p` b) `k` b
    eliminateLeft = isoTo leftUnitor
    eliminateRight :: (a `p` (ID k p)) `k` a
    eliminateRight = isoTo rightUnitor
    introduceLeft :: b `k` ((ID k p) `p` b)
    introduceLeft = isoFrom leftUnitor
    introduceRight :: a `k` (a `p` (ID k p))
    introduceRight = isoFrom rightUnitor

instance PreMonoidal (->) (,) where
    type ID (->) (,) = ()
    eliminateLeft ((), b) = b
    eliminateRight (a, ()) = a
    introduceLeft b = ((),b)
    introduceRight a = (a, ())
instance Arrow a => PreMonoidal a (,) where
    type ID a (,) = ()
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
class PreMonoidal k p => Terminal k p
class Initial k p
class (Terminal k p, PreMonoidal k p) => SemiPreCartesian k p
class (SemiPreCartesian k p, Monoidal k p) => SemiCartesian k p
class SemiPreCartesian k p => PreCartesian k p where
    copy :: k a (p a a)
    delete :: k a (ID k p)

class (SemiPreCartesian k p) => PreCoCartesian k p (c :: * -> Constraint) where
    merge :: c a =>k (p a a) a
    initial :: c a=> k (ID k p) a ---can this just be c a=> a?
instance Terminal (->) (,)
instance SemiPreCartesian (->) (,)
instance PreCartesian (->) (,) where
    copy x = (x,x)
    delete _ = ()
instance PreCoCartesian (->) (,) (Num) where
    merge (a,b) = a+b
    initial _ = 0
instance PreCoCartesian (->) (,) (Monoid) where
    merge (a,b) = a <> b
    initial _ = mempty
class (PreCartesian k p, Monoidal k p) => Cartesian k p
class Choice k p
class PreMonoidal k p => Dagger k p where
    --alternate name for involute could be dagger
    involute :: k (k a b) (k b a)
class PreMonoidal k p => LeftRigid k p where
    leftUnit :: k (ID k p) (p v vl)
    leftCoUnit :: k (vl v) (ID k p)

class PreMonoidal k p => RightRigid k p where
    rightUnit :: k (ID k p) (p vr v)
    rightCoUnit :: k (p v vr) (ID k p)
type Rigid k p = (LeftRigid k p, RightRigid k p)



--I -> A* x A can be thought of as a source; A x A* -> I is a sink

\end{code}
