\begin{code}
{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, PolyKinds #-}

module GHC.Arrows.Experimental.Associative where
import Control.Category
import GHC.Arrows.Experimental.GBifunctor
import Data.Either

class (Bifunctor p k k k, Category k) => Associative k p where
    associateRight :: k (p (p a b) c) (p a (p b c))
    associateLeft :: k (p a (p b c)) (p (p a b) c)

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
