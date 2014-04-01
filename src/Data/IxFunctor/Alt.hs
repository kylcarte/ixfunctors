{-# LANGUAGE PolyKinds #-}

module Data.IxFunctor.Alt where

import Data.IxFunctor

class IxFunctor m => IxAlt m where
  ialt  :: m i j a -> m i j a -> m i j a
  isome :: m i j a -> m i j [a]
  imany :: m i j a -> m i j [a]

(<||>) :: IxAlt m => m i j a -> m i j a -> m i j a
(<||>) = ialt

