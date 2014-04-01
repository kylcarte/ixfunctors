{-# LANGUAGE PolyKinds #-}

module Data.IxFunctor.Pointed where

import Data.IxFunctor

class IxFunctor m => IxPointed m where
  ireturn :: a -> m i i a

