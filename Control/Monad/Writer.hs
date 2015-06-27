
-- | A simple strict Writer monad implementation
--
-- This module is not intended to be exposed and only servers
-- to create a trivial and simple implementation for internal
-- use within this package.
--
-- Many of the implementations in this module were derived directly
-- from mtl and transformers. This WriterT implementations is actually
-- a StateT with functionality exposed suitable for a Writer.
module Control.Monad.Writer
      (
        Writer
      , execWriter
      , tell
      ) where


import Control.Applicative
import Data.Monoid


-- * Identity

-- | Trivial Identity monad.
--
-- Used as a base for the Writer
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f m = Identity (f (runIdentity m))

instance Applicative Identity where
    pure a = Identity a
    Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
    return a = Identity a
    m >>= k  = k (runIdentity m)


-- * Writer

type Writer w = WriterT w Identity

runWriter :: Monoid w => Writer w a -> (a, w)
runWriter = runIdentity . runWriterT

execWriter :: Monoid w => Writer w a -> w
execWriter m = snd (runWriter m)


-- * WriterT

newtype WriterT w m a = WriterT { unWriterT :: w -> m (a, w) }

instance Functor m => Functor (WriterT w m) where
    fmap f m = WriterT $ \ w ->
        fmap (\(a, w') -> (f a, w')) $ unWriterT m w

instance (Functor m, Monad m) => Applicative (WriterT w m) where
    pure a = WriterT $ \w -> return (a,w)
    WriterT mf <*> WriterT mx = WriterT $ \w -> do
        (f, w') <- mf w
        (x, w'') <- mx w'
        return (f x, w'')
    {-# INLINE (<*>) #-}

instance (Monad m, Monoid w) => Monad (WriterT w m) where
    return a = WriterT $ \w -> return (a, w)
    m >>= f  = WriterT $ \w -> do
        (a, w') <- unWriterT m w
        unWriterT (f a) w'

runWriterT :: (Monoid w) => WriterT w m a -> m (a, w)
runWriterT m = unWriterT m mempty

tell :: (Monad m, Monoid w) => w -> WriterT w m ()
tell w = WriterT $ \w' ->
    let wt = w' `mappend` w
    in wt `seq` return ((), w' `mappend` w)
