cata :: (Monoid m) => (a -> m) -> ([a] -> m)
cata phi []     = unit
cata phi (x:xs) = phi x <> cata phi xs
