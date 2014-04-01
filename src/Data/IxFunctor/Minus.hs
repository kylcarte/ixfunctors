{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.IxFunctor.Minus
  ( module Data.IxFunctor.Minus
  -- Convenience Reexports
  , module Data.IxFunctor.Empty
  , module Data.IxFunctor.Coalt
  ) where

import Data.IxFunctor.Empty
import Data.IxFunctor.Coalt

type IxMinus m = (IxEmpty m, IxCoalt m)

