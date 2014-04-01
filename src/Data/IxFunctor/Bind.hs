{-# LANGUAGE PolyKinds #-}

module Data.IxFunctor.Bind where

import Data.IxFunctor
import Data.IxFunctor.Apply

class IxApply m => IxBind m where
  ibind :: m i j a -> (a -> m j k b) -> m i k b

ijoin :: IxBind m => m i j (m j k a) -> m i k a
ijoin = flip ibind id

(>>>=) :: IxBind m => m i j a -> (a -> m j k b) -> m i k b
(>>>=) = ibind
infixr 1 >>>=

(=<<<) :: IxBind m => (a -> m j k b) -> m i j a -> m i k b
(=<<<) = flip (>>>=)
infixl 1 =<<<

(>>>) :: IxBind m => m i j a -> m j k b -> m i k b
m1 >>> m2 = m1 >>>= const m2
infixl 1 >>>

(>>=>) :: IxBind m => (a -> m i j b) -> (b -> m j k c) -> a -> m i k c
(f >>=> g) x = f x >>>= g
infixr 1 >>=>

(<=<<) :: IxBind m => (b -> m j k c) -> (a -> m i j b) -> a -> m i k c
(f <=<< g) x = g x >>>= f
infixr 1 <=<<

