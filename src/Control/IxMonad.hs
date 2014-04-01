{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.IxMonad
  ( module Control.IxMonad
  -- Convenience Reexports
  , module Data.IxFunctor.Pointed
  , module Data.IxFunctor.Bind
  , module Data.IxFunctor.Empty
  , module Data.IxFunctor.Plus
  ) where

-- IxMonad
import Data.IxFunctor.Pointed
import Data.IxFunctor.Apply
import Data.IxFunctor.Bind

-- IxMonadPlus
import Data.IxFunctor.Empty
import Data.IxFunctor.Plus

type IxMonad m = (IxPointed m, IxBind m)

-- IxMonad {{{

iapIxMonad :: IxMonad m => m i j (a -> b) -> m j k a -> m i k b
iapIxMonad f x = f >>>= \f' -> x >>>= \x' -> ireturn (f' x')

iliftM :: IxMonad m => (a -> b) -> m i j a -> m i j b
iliftM = iliftA

iliftM2 :: IxMonad m => (a -> b -> c) -> m i j a -> m j k b -> m i k c
iliftM2 = iliftA2

iliftM3 :: IxMonad m => (a -> b -> c -> d)
  -> m i j a -> m j k b -> m k l c -> m i l d
iliftM3 = iliftA3

isequence :: IxMonad m => [m i i a] -> m i i [a]
isequence = foldr (iliftM2 (:)) $ ireturn []

imapM :: IxMonad m => (a -> m i i b) -> [a] -> m i i [b]
imapM f = isequence . map f

isequence_ :: IxMonad m => [m i i a] -> m i i ()
isequence_ = foldr (>>>) $ ireturn ()

imapM_ :: IxMonad m => (a -> m i i b) -> [a] -> m i i ()
imapM_ f = isequence_ . map f

-- }}}

type IxMonadPlus m = (IxMonad m, IxPlus m)

-- IxMonadPlus {{{

imzero :: IxMonadPlus m => m i i a
imzero = iempty

implus :: IxMonadPlus m => m i j a -> m i j a -> m i j a
implus = ialt

-- }}}

