An associator in category theory and higher category theory is an isomorphism that relaxes the ordinary associativity equality of a binary operation.

\begin{code}
{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, PolyKinds, TypeOperators #-}

module GHC.Arrows.Experimental.Associative where

import GHC.Arrows.Experimental.Binoidal
import GHC.Arrows.Experimental.GBifunctor

import Control.Category
import Data.Either
import Data.Tuple 

newtype Isomorphism a k b = Isomorphism {getMorphisms :: (a `k` b, b `k` a)}

isoTo :: Category k => Isomorphism a k b -> a `k` b
isoTo = (fst . getMorphisms)
isoFrom :: Category k => Isomorphism a k b -> b `k` a
isoFrom = (snd . getMorphisms)
class (Bifunctor p k k k, Binoidal k p) => Associative k p where
    {-# MINIMAL associator | (associateLeft, associateRight) #-}
    associator :: Isomorphism ((a `p` b) `p` c) k  (a `p` (b `p` c))
    associator = Isomorphism (associateRight, associateLeft)
    associateRight :: ((a `p` b) `p` c) `k` (a `p` (b `p` c))
    associateRight =  (fst . getMorphisms) associator
    associateLeft :: (a `p` (b `p` c)) `k` ((a `p` b) `p` c)
    associateLeft = (snd . getMorphisms) associator

instance Associative (->) (,) where
        associateRight ((a,b),c) = (a,(b,c))
        associateLeft (a,(b,c)) = ((a,b),c)

instance Associative (->) Either where
        associateRight (Left (Left a)) = Left a
        associateRight (Left (Right b)) = Right (Left b)
        associateRight (Right c) = Right (Right c)
        associateLeft (Left a) = Left (Left a)
        associateLeft (Right (Left b)) = Left (Right b)
        associateLeft (Right (Right c)) = Right c


\end{code}
