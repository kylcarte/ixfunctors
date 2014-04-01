
module Control.IxMonad.Trans.Reader where

{-

data IxReaderT
  (r  :: l_ -> l_ -> *)
  (im :: k_ -> k_ -> * -> *)
  (fi :: l_)
  (gj :: l_)
  (a  :: *) where
  IxReaderT
    :: (r (f i) (g j) -> im i j a)
    -> IxReaderT r im (f i) (g j) a

instance IxMonadTrans (IxReaderT r) where
  ilift m = IxReaderT $ \_rFiFj -> m

instance IxFunctor m => IxFunctor (IxReaderT r m) where
  imap f (IxReaderT m) = IxReaderT $ imap f . m

instance IxPointed m => IxPointed (IxReaderT r m) where
  ireturn = ilift . ireturn

instance IxPointed m => IxPointed (IxReaderT r m) where
  ireturn (a :: a) = IxReaderT f
    where
    f :: r (f i) (f i) -> m i i a
    f _rFiFi = m
    m :: m i i a
    m = ireturn a

instance IxApply m => IxApply (IxReaderT r m) where
  iap (IxReaderT (mf :: r (f i) (g j) -> m i j (a -> b)))
      (IxReaderT (ma :: r (g k) (h l) -> m k l a))
    = (IxReaderT m :: IxReaderT r m f h b)
    where
    m :: r (f i) (h k) -> m i k b
    m = undefined

ma :: r (j i2) (k2 j2) -> m i2 j2 a
mf :: r (i i1) (j j1) -> m i1 j1 (a -> b)
res :: 
iap :: IxReaderT r m i j (a -> b)
-> IxReaderT r m j k2 a -> IxReaderT r m i k2 b

instance IxBind m => IxBind (IxReaderT r m) where
  ibind (IxReaderT (m :: r (f i) (g j) -> m i j a)) f = undefined

-}

