A \textbf{binoidal category} is a category $C$ equipped with

\begin{itemize}%
\item for each pair $x,y$ of objects of $C$, an object $x \otimes y$;
\item for each object $x$ a functor $x \rtimes -$ whose action on objects sends $y$ to $x \otimes y$
\item for each object $x$ a functor $- \ltimes x$ whose action on objects sends $y$ to $y \otimes x$

\end{itemize}

\begin{code}
{-#LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, PolyKinds, TypeOperators #-}

module GHC.Arrows.Experimental.Binoidal where

import GHC.Arrows.Experimental.GBifunctor

import Control.Category
import Data.Either

class (Category k, Bifunctor p k k k) => Binoidal k p where
  inLeft :: a `k` (b `k` (p a b))
  inRight :: b `k` (a `k` (p a b))

instance Binoidal (->) (,) where
  inLeft a = \x -> (a,x)
  inRight b = \x -> (x,b)
  
instance Binoidal (->) Either where
  inLeft a = \_ -> Left a
  inRight b = \_ -> Right b
\end{code}
