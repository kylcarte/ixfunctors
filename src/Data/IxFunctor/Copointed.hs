{-# LANGUAGE PolyKinds #-}

module Data.IxFunctor.Copointed where

import Data.IxFunctor

class IxFunctor w => IxCopointed w where
  iextract :: w i i a -> a

