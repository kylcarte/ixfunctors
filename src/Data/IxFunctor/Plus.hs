{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.IxFunctor.Plus
  ( module Data.IxFunctor.Plus
  -- Convenience Reexports
  , module Data.IxFunctor.Empty
  , module Data.IxFunctor.Alt
  ) where

import Data.IxFunctor.Empty
import Data.IxFunctor.Alt

type IxPlus m = (IxEmpty m, IxAlt m)

