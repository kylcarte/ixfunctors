{-# LANGUAGE PolyKinds #-}

module Data.IxFunctor.Coalt where

import Data.IxFunctor
import Data.IxFunctor.Coapply

class IxCoapply w => IxCoalt w where
  icoalt  :: w i j a -> (w i j a,w i j a)
  icosome :: w i j a -> [w i j a]
  icomany :: w i j a -> [w i j a]

