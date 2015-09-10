can :: (Monoid m) => (a -> m) -> ([a] -> m)
can phi []     = mzero
can phi (x:xs) = phi x <> can phi xs
