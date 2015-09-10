data Instr s next = Get (s -> next) | Put s next
    deriving (Functor)

put x = liftF (Put x ())

get :: Free (Instr s) s
get = Roll $ Get $ \s -> Pure s

eval :: Free (Instr s) a -> s -> (a,s)
eval (Pure x) s = (x,s)
eval (Roll u) s
    | Put s' v <- u = eval v s'
    | Get k    <- u = eval (k s) s

uneval :: (s -> (a,s)) -> Free (Instr s) a
uneval k = do
    s <- get
    let (x,s) = k s
    put s
    return x

-- eval . uneval == id, aber nicht uneval . eval == id.
