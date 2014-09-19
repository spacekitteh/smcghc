\begin{code}
{-#LANGUAGE  PolyKinds, ConstraintKinds, GADTs, Arrows, StandaloneDeriving #-}
module GHC.Arrows.Experimental.ArrowTranslator  where
{-import GHC.Arrows.Experimental.GBifunctor
import GHC.Arrows.Experimental.Braided
import GHC.Arrows.Experimental.Monoidal
import GHC.Arrows.Experimental.Binoidal
import GHC.Arrows.Experimental.Associative-}

import Control.Category
import Control.Arrow
import Prelude hiding ((.))
data ArrowExpr :: * -> * -> * where
    ID :: ArrowExpr c c
    Compose :: ArrowExpr c d -> ArrowExpr b c -> ArrowExpr b d
    Arr :: (b -> c) -> ArrowExpr b c
    First :: ArrowExpr b c -> ArrowExpr (b,d) (c,d)
    Second :: ArrowExpr b c -> ArrowExpr (d,b) (d,c)
    Split :: ArrowExpr b c -> ArrowExpr b' c' -> ArrowExpr (b,b') (c,c')
    Fan :: ArrowExpr b c -> ArrowExpr b c' -> ArrowExpr b (c,c')
    Zero :: ArrowExpr b c
    MonOp :: ArrowExpr b c -> ArrowExpr b c -> ArrowExpr b c
    ChooseLeft :: ArrowExpr b c -> ArrowExpr (Either b d) (Either c d)
    ChooseRight :: ArrowExpr b c -> ArrowExpr (Either d b) (Either d c)
    Apply :: ArrowExpr (ArrowExpr b c, b) c
    Loop :: ArrowExpr (b,d) (c,d) -> ArrowExpr b c 
    Return :: ArrowExpr b b
instance Show (ArrowExpr a b) where
    show ID = "ID"
    show (Compose a b)= show a ++ "." ++ show b
    show (Arr _) = "func"
    show (First a) = "First(" ++ show a ++ ")"
    show (Second a) = "Second("++ show a ++ ")"
    show Return = "Return"
    show (Split a b) = "Split ("++ show a ++","++ show b ++ ")"
    show _ = "whatever"
instance Category ArrowExpr where
    id = ID
    (.) = Compose
{-#RULES ">>>" (>>>) = (>>>>) #-}
(>>>>) :: ArrowExpr a b -> ArrowExpr b c -> ArrowExpr a c
(>>>>) l r = Compose r l
instance Arrow ArrowExpr where
    arr = Arr
    first = First
    second = Second
    (***) = Split
    (&&&) = Fan
    returnA = Return
instance ArrowZero ArrowExpr where
    zeroArrow = Zero
instance ArrowPlus ArrowExpr where
    (<+>) = MonOp
instance ArrowChoice ArrowExpr where
    left = ChooseLeft
    right = ChooseRight
instance ArrowApply ArrowExpr where
    app = Apply
instance ArrowLoop ArrowExpr where
    loop = Loop

test1 :: ArrowExpr b Int -> ArrowExpr b Int -> ArrowExpr b Int
test1 f g = proc x -> do
                    y <- f -< x
                    z <- g -< x
                    returnA -< y + z
test2 :: ArrowExpr a a
test2 = proc x -> do
--             y <- id -<  x
             returnA -<x

\end{code}
