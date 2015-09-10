import Control.Monad (join)

data Free f a
    = Pure a
    | Roll (f (Free f a))

liftF :: (Functor f) => f a -> Free f a
liftF = Roll . fmap Pure

instance (Functor f) => Functor (Free f) where
    fmap h (Pure x) = Pure (h x)
    fmap h (Roll u) = Roll (fmap (fmap h) u)

instance (Functor f) => Monad (Free f) where
    return x = Pure x
    m >>= k  = join_ $ fmap k m
        where
        join_ (Pure u) = u
        join_ (Roll v) = Roll (fmap join_ v)

can :: (Functor f, Monad m)
    => (forall a. f a -> m a)
    -> (forall a. Free f a -> m a)
can phi (Pure x) = return x
can phi (Roll u) = join $ phi . fmap (can phi) $ u
          -- oder: join $ fmap (can phi) . phi $ u
