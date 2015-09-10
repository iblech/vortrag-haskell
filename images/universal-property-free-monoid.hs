can :: (Monoid m) => (a -> m) -> ([a] -> m)
can phi []     = mzero
can phi (x:xs) = phi x `mappend` can phi xs
