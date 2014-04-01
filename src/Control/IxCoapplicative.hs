{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.IxCoapplicative
  ( module Control.IxCoapplicative
  -- Convenience Reexports
  , module Data.IxFunctor.Copointed
  , module Data.IxFunctor.Coapply
  , module Data.IxFunctor.Empty
  , module Data.IxFunctor.Coalt
  ) where

import Data.IxFunctor
-- Coapplicative
import Data.IxFunctor.Copointed
import Data.IxFunctor.Coapply
-- Coalternative
import Data.IxFunctor.Empty
import Data.IxFunctor.Coalt

type IxCoapplicative w = (IxCopointed w, IxCoapply w)

type IxCoalternative w = (IxCoapplicative w, IxEmpty w, IxCoalt w)

