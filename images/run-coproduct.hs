runM :: (Show e) => s -> Coprod (State s) (Err e) a -> IO a
runM st m = do
    ref <- newIORef st
    elim (embedState ref) embedErr m

embedState :: IORef s -> State s a -> IO a
embedState ref m = do
    st <- readIORef ref
    let (x,st') = runState m st
    writeIORef ref st'
    return x

embedErr :: (Show e) => Err e a -> IO a
embedErr (Left  e) = putStrLn ("Fehler: " ++ show e) >> exitFailure
embedErr (Right x) = return x
