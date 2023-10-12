module Lib (getFoldAggregate) where

import Control.Exception
import Text.Printf

import qualified Data.Map.Strict as Map


-- |
getFoldAggregate :: (Ord k, Ord v, Num v) => String -> Map.Map k v -> (k, v) -> Map.Map k v
getFoldAggregate "max" = \m (k, v) -> Map.insertWith max k v m
getFoldAggregate "min" = \m (k, v) -> Map.insertWith min k v m
getFoldAggregate "sum" = \m (k, v) -> Map.insertWith (+) k v m
getFoldAggregate "count" = \m (k, _) -> Map.insertWith (+) k 1 m

getFoldAggregate func = throw . PatternMatchFail . printf "Invalid aggregate function: \"%s\"" $ func
