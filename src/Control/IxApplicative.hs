{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.IxApplicative
  ( module Control.IxApplicative
  -- Convenience Reexports
  , module Data.IxFunctor.Pointed
  , module Data.IxFunctor.Apply
  , module Data.IxFunctor.Empty
  , module Data.IxFunctor.Alt
  ) where

import Data.IxFunctor
-- Applicative
import Data.IxFunctor.Pointed
import Data.IxFunctor.Apply
-- Alternative
import Data.IxFunctor.Empty
import Data.IxFunctor.Alt

type IxApplicative m = (IxPointed m, IxApply m)

type IxAlternative m = (IxApplicative m, IxEmpty m, IxAlt m)

