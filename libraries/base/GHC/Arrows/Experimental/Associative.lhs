An associator in category theory and higher category theory is an isomorphism that relaxes the ordinary associativity equality of a binary operation.

\begin{code}
{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, PolyKinds, TypeOperators #-}

module GHC.Arrows.Experimental.Associative where

import GHC.Arrows.Experimental.Binoidal
import GHC.Arrows.Experimental.GBifunctor

import Control.Category
import Data.Either
import Data.Tuple 

newtype Isomorphism k a b = Isomorphism {getMorphisms ::(b `k` a, a `k` b)}

class (Bifunctor p k k k, Binoidal k p) => Associative k p where
    associator :: Isomorphism k ((a `p` b) `p` c) (a `p` (b `p` c))
    associator = Isomorphism (associateLeft, associateRight)
    associateRight :: k ((a `p` b) `p` c) (a `p` (b `p` c))
    associateRight =  (snd . getMorphisms) associator
    associateLeft :: k (a `p` (b `p` c)) ((a `p` b) `p` c)
    associateLeft = (fst . getMorphisms) associator

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
