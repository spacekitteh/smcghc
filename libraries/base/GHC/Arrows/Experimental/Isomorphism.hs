{-# LANGUAGE NoImplicitPrelude, TypeOperators #-}
module GHC.Arrows.Experimental.Isomorphism where

import Control.Category
import Data.Tuple

newtype Isomorphism a k b = Isomorphism {getMorphisms :: (a `k` b, b `k` a)}

isoTo :: Category k => Isomorphism a k b -> a `k` b
isoTo = (fst . getMorphisms)
isoFrom :: Category k => Isomorphism a k b -> b `k` a
isoFrom = (snd . getMorphisms)
