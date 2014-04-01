{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.IxComonad
  ( module Control.IxComonad
  -- Convenience Reexports
  , module Data.IxFunctor.Copointed
  , module Data.IxFunctor.Extend
  , module Data.IxFunctor.Empty
  , module Data.IxFunctor.Minus
  ) where

-- Comonad
import Data.IxFunctor.Copointed
import Data.IxFunctor.Coapply
import Data.IxFunctor.Extend

-- ComonadMinus
import Data.IxFunctor.Empty
import Data.IxFunctor.Minus

type IxComonad w = (IxCopointed w, IxExtend w)

-- TODO: icoapIxComonad, iliftW, iliftW2, iliftW3, icosequence, icomapM (?), <fn>_ varieties

{-
icoapIxComonad :: IxComonad w => 
-}

-- }}}

type IxComonadMinus w = (IxComonad w, IxMinus w)

-- IxComonadMinus {{{

iwzero :: IxComonadMinus w => w i i a
iwzero = iempty

iwminus :: IxComonadMinus w => w i j a -> (w i j a,w i j a)
iwminus = icoalt

-- }}}

