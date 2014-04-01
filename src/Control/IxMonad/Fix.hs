{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.IxMonad.Fix where

import Control.IxMonad

class IxMonad m => IxMonadFix m where
  imfix :: (a -> m i i a) -> m i i a

