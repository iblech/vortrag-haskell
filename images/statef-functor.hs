instance Functor StateF where
    fmap phi (Get k)    = Get (f . k)
    fmap phi (Put st k) = Put st (phi k)
