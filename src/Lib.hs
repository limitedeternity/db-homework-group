module Lib (getAggregate) where

import Control.Exception
import Text.Printf

import Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Map.Strict as Map


-- |
getAggregate :: Monad m => String -> ConduitT (T.Text, Double) o m (Map.Map T.Text Double)
getAggregate "max" = CL.fold (\m (k, v) -> Map.insertWith max k v m) Map.empty
getAggregate "min" = CL.fold (\m (k, v) -> Map.insertWith min k v m) Map.empty
getAggregate "sum" = CL.fold (\m (k, v) -> Map.insertWith (+) k v m) Map.empty
getAggregate "count" = CL.map (\(k, _) -> (k, 1.0)) .| getAggregate "sum"

getAggregate aggFunc = throw . PatternMatchFail $ printf "getAggregate used on \"%s\"" aggFunc
