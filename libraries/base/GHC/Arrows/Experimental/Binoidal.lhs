\begin{code}
{-#LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, PolyKinds #-}

module GHC.Arrows.Experimental.Binoidal where
import Control.Category
import GHC.Arrows.Experimental.GBifunctor
import Data.Either

class (Category k, Bifunctor p k k k) => Binoidal k p where
  inLeft :: k a (k b (p a b))
  inRight :: k b (k a (p a b))

instance Binoidal (->) (,) where
  inLeft a = \x -> (a,x)
  inRight b = \x -> (x,b)
  
instance Binoidal (->) Either where
  inLeft a = \_ -> Left a
  inRight b = \_ -> Right b
\end{code}
