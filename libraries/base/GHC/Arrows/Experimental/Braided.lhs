Braiding introduces a "swap" function, which switches the objects in a bifunctor. A symmetric braid is when swap . swap = id; that is, when it is its own inverse.



\begin{code}

{-#LANGUAGE PolyKinds, MultiParamTypeClasses, NoImplicitPrelude #-}
{-#LANGUAGE TypeOperators, OverlappingInstances, FlexibleInstances #-}
{-#LANGUAGE UndecidableInstances,CPP#-}


module GHC.Arrows.Experimental.Braided where

import GHC.Arrows.Experimental.Associative
import GHC.Arrows.Experimental.Isomorphism

import Control.Arrow
import Control.Category
import Data.Either


{-# RULES "braid.unbraid = id" braid . unbraid = id;
           "unbraid.braid = id" unbraid . braid = id #-}


class Associative k p => Braided k p where
    {-#MINIMAL braiding | (braid,unbraid) #-}
    braiding :: Isomorphism k (p a b) (p b a)
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

instance Arrow a => Braided a (,) where
    braid = arr (\(x,y) -> (y,x))
    unbraid = braid

{-# RULES
    "braid . twist . braid = twist"
     braid . twist . braid = twist;
    "unbraid . twist . braid = twist"
     unbraid . twist . unbraid = twist;
    "braid . untwist . braid = untwist"
     braid . untwist . braid = untwist;
    "unbraid . untwist . unbraid = untwist"
     unbraid . untwist . unbraid = untwist
 #-}
class Braided k p => Balanced k p where
    {-#MINIMAL twisting | (twist,untwist) #-}
    twisting :: Isomorphism k (a `p` b) (a `p` b)
    twisting = Isomorphism (twist, untwist)
    twist :: (p a b) `k` (p a b)
    twist = (isoTo twisting)
    untwist :: (p b a) `k` (p b a)
    untwist = isoFrom twisting

{-# RULES
        "swap.swap= id 1"
  (braid :: Symmetric k p => (p a b) `k` (p b a)) . braid = id;
        "swap.swap= id 2"
  (unbraid :: (Symmetric k p) => (p b a) `k` (p a b)) . unbraid = id#-}
class Braided k p => Symmetric k p
instance Symmetric k p => Balanced k p where
    twisting = Isomorphism (id, id)
swap :: Symmetric k p => (p a b) `k` (p b a)
swap = braid

instance Symmetric (->) Either
instance Symmetric (->) (,)
instance Arrow a => Symmetric a (,)



#if FALSE
-- need injective type families for this part.

instance (Symmetric k1 p1, LeftRigid k p, ID k1 p1 ~ ID k p, k~k1, p~p1) =>
 RightRigid k p  where
    rightUnit = leftUnit
    rightCoUnit :: k (p v vr) (ID k p)
    rightCoUnit = leftCoUnit

instance (Symmetric k1 p1, RightRigid k p, ID k1 p1 ~ ID k p, k~k1, p~p1) =>
 LeftRigid k p where
    leftUnit = rightUnit
    leftCoUnit = rightCoUnit
#endif
\end{code}
