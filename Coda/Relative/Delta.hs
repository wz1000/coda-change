{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Stuff we an measure in UTF-16 code units
---------------------------------------------------------------------------------

module Coda.Relative.Delta
  ( Delta(..)
  , HasDelta(..)
  , units
  , HasMonoidalDelta
  , HasOrderedDelta
  , HasRelativeDelta
  ) where

import Coda.Relative.Absolute
import Coda.Relative.Class
import Coda.Relative.Delta.Type
import Data.FingerTree
import Data.Text
import Data.Text.Unsafe

--------------------------------------------------------------------------------
-- Something that has a delta
--------------------------------------------------------------------------------

-- | Something we can measure.
class HasDelta t where
  delta :: t -> Delta

-- | extract the number of utf-16 code units from a delta
units :: HasDelta t => t -> Int
units y = case delta y of
  Delta x -> x

instance HasDelta Delta where
  delta = id

instance HasDelta Text where
  delta = Delta . lengthWord16

instance HasDelta a => HasDelta (Absolute a) where
  delta (Absolute a) = delta a

instance (Measured v a, HasDelta v) => HasDelta (FingerTree v a) where
  delta = delta . measure

--------------------------------------------------------------------------------
-- Monoidal deltas
--------------------------------------------------------------------------------

-- |
-- 'delta' for this type is a monoid homomorphism
--
-- @
-- 'delta' (m '<>' n) = 'delta' m <> 'delta' n
-- 'delta' mempty = 0
-- @
class (Monoid t, HasDelta t) => HasMonoidalDelta t where
instance HasMonoidalDelta Delta
instance HasMonoidalDelta Text
instance HasMonoidalDelta a => HasMonoidalDelta (Absolute a)
instance (Measured v a, HasMonoidalDelta v) => HasMonoidalDelta (FingerTree v a)

--------------------------------------------------------------------------------
-- Monotone deltas
--------------------------------------------------------------------------------

-- |
-- Requires that 'delta' is monotone
--
-- @m <= n@ implies @'delta' m <= 'delta' n@
class (Ord t, HasDelta t) => HasOrderedDelta t
instance HasOrderedDelta Delta
instance HasOrderedDelta a => HasOrderedDelta (Absolute a)

-- TODO: supply old instances for all Coda.Relative.*

--------------------------------------------------------------------------------
-- Relative deltas
--------------------------------------------------------------------------------

-- |
-- 'delta' and 'rel'
--
-- @
-- 'delta' ('rel' d p) = d <> 'delta' p
-- @
class (Relative t, HasDelta t) => HasRelativeDelta t
instance HasRelativeDelta Delta
