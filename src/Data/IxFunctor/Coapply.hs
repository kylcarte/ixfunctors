{-# LANGUAGE PolyKinds #-}

module Data.IxFunctor.Coapply where

import Data.IxFunctor

class IxFunctor w => IxCoapply w where
  icoap    :: w j k (a -> b) -> w i k a -> w i j b

icothenL :: IxCoapply w => w j k a -> w i k b -> w i j a
icothenL = iliftCA2 const

icothenR :: IxCoapply w => w j k a -> w i k b -> w i j b
icothenR = iliftCA2 (const id)

(<@@>) :: IxCoapply w => w j k (a -> b) -> w i k a -> w i j b
(<@@>) = icoap
infixl 4 <@@>

(<@@) :: IxCoapply w => w j k a -> w i k b -> w i j a
(<@@) = icothenL
infixl 4 <@@

(@@>) :: IxCoapply w => w j k a -> w i k b -> w i j b
(@@>) = icothenR
infixl 4 @@>

iliftCA :: IxCoapply w => (a -> b) -> w i j a -> w i j b
iliftCA = (<$$>)

iliftCA2  :: IxCoapply w => (a -> b -> c) -> w j k a -> w i k b -> w i j c
iliftCA2 f a b = f <$$> a <@@> b

iliftCA3 :: IxCoapply w => (a -> b -> c -> d)
  -> w k l a -> w j l b -> w i k c -> w i j d
iliftCA3 f a b c = f <$$> a <@@> b <@@> c

