{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.IxComonad.Cofix where

import Control.IxComonad

class IxComonad w => IxComonadCofix w where
  iwfix :: (w i i a -> a) -> w i i a

