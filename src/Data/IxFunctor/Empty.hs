{-# LANGUAGE PolyKinds #-}

module Data.IxFunctor.Empty where

import Data.IxFunctor

class IxFunctor f => IxEmpty f where
  iempty :: f i i a

