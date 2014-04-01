{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Control.IxMonad.Trans.Class where

import Control.IxMonad

class IxMonadTrans
  (t :: (k -> l)           -- f
     -> (k -> l)           -- g
     -> (k -> k -> * -> *) -- m
     -> l                  -- f i
     -> l                  -- g j
     -> *                  -- a
     -> *) where
  ilift  :: IxMonad m => m i j a -> t f g m (f i) (g j) a

