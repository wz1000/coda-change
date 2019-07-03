module Coda.Diff
  ( module Coda.Syntax.Change
  , diffToChange
  , getChange
  , Diff(..)
  ) where

import Coda.Syntax.Change
import Coda.Relative.Delta

import Data.Algorithm.Diff

diffToChange :: Eq t => [Diff [t]] -> Change
diffToChange = foldMap go
  where
    go (First xs) = del  (Delta $ length xs)
    go (Second xs) = ins (Delta $ length xs)
    go (Both xs _) = cpy (Delta $ length xs)

getChange :: Eq a => [a] -> [a] -> Change
getChange xs ys = diffToChange (getGroupedDiff xs ys)
