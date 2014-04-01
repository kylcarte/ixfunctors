{-# LANGUAGE PolyKinds #-}

module Data.IxFunctor where

class IxFunctor (f :: k -> l -> * -> *) where
  imap :: (a -> b) -> f i j a -> f i j b

(<$$>) :: IxFunctor f => (a -> b) -> f i j a -> f i j b
(<$$>) = imap
infixl 4 <$$>

(<$$) :: IxFunctor f => a -> f i j b -> f i j a
a <$$ f = const a <$$> f
infixl 4 <$$

($$>) :: IxFunctor f => f i j b -> a -> f i j a
($$>) = flip (<$$)

ivoid :: IxFunctor f => f i j a -> f i j ()
ivoid = (() <$$)

