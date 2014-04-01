{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.IxFunctor.Wrapper where

import Data.IxFunctor
import Control.IxApplicative
import Control.IxMonad
import Control.IxMonad.Fix
import Control.IxCoapplicative
import Control.IxComonad

import Control.Applicative
import Control.Comonad hiding ((<@@>))
import Control.Monad
import Control.Monad.Fix

newtype Ixed i j f a = WrapIx 
  { unwrapIx :: f i j a
  } deriving (Eq,Show)

-- Ixed {{{

instance IxFunctor f => Functor (Ixed i j f) where
  fmap f = WrapIx . imap f . unwrapIx

instance IxApplicative m => Applicative (Ixed i i m) where
  pure = WrapIx . ireturn
  w1 <*> w2 = WrapIx $ iap (unwrapIx w1) (unwrapIx w2)

instance IxAlternative m => Alternative (Ixed i i m) where
  empty = WrapIx iempty
  w1 <|> w2 = WrapIx $ ialt (unwrapIx w1) (unwrapIx w2)

instance IxMonad m => Monad (Ixed i i m) where
  return  = WrapIx . ireturn
  w >>= f = WrapIx $ ibind (unwrapIx w) (unwrapIx . f)

instance IxMonadFix m => MonadFix (Ixed i i m) where
  mfix f = WrapIx $ imfix $ unwrapIx . f

instance IxMonadPlus m => MonadPlus (Ixed i i m) where
  mzero = WrapIx imzero
  mplus w1 w2 = WrapIx $ implus (unwrapIx w1) (unwrapIx w2)

instance IxComonad m => Comonad (Ixed i i m) where
  extract = iextract . unwrapIx
  duplicate = fmap WrapIx . WrapIx . iduplicate . unwrapIx

instance (IxComonad w, IxCoapply w) => ComonadApply (Ixed i i w) where
  w1 <@> w2 = WrapIx $ unwrapIx w1 <@@> unwrapIx w2

-- }}}

{-

-- a yak to shave another time

newtype UnIxed f i j a = WrapUnIx
  { unwrapUnIx :: f a
  } deriving (Eq,Show)

-- UnIxed {{{

instance Functor f => IxFunctor (UnIxed f i j) where
  fmap f = WrapIx . imap f . unwrapIx

instance Applicative m => IxApplicative (UnIxed m i i) where
  pure = WrapIx . ireturn
  w1 <*> w2 = WrapIx $ iap (unwrapIx w1) (unwrapIx w2)

instance IxAlternative m => Alternative (UnIxed m i i) where
  empty = WrapIx iempty
  w1 <|> w2 = WrapIx $ ialt (unwrapIx w1) (unwrapIx w2)

instance IxMonad m => Monad (UnIxed m i i) where
  return  = WrapIx . ireturn
  w >>= f = WrapIx $ ibind (unwrapIx w) (unwrapIx . f)

instance IxMonadFix m => MonadFix (UnIxed m i i) where
  mfix f = WrapIx $ imfix $ unwrapIx . f

instance IxMonadPlus m => MonadPlus (UnIxed m i i) where
  mzero = WrapIx imzero
  mplus w1 w2 = WrapIx $ implus (unwrapIx w1) (unwrapIx w2)

instance IxComonad m => Comonad (UnIxed m i i) where
  extract = iextract . unwrapIx
  duplicate = fmap WrapIx . WrapIx . iduplicate . unwrapIx

instance (IxComonad w, IxCoapply w) => ComonadApply (UnIxed w i i) where
  w1 <@> w2 = WrapIx $ unwrapIx w1 <@@> unwrapIx w2

-- }}}
-}

