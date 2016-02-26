module Main where

type Nat = Int


-------------------------------------------------------------------------------
-- Allgemeines zu Games
-------------------------------------------------------------------------------

-- Ein Game besteht aus der Angabe einer Liste von Games, in die der linke
-- Spieler ziehen kann, und einer Liste von Games, in die der rechte Spieler
-- ziehen kann.
data Game = MkGame { left :: [Game], right :: [Game] }
    deriving (Show)

-- Beispiele für Games
zero    = MkGame []     []      -- geboren an Tag 0
one     = MkGame [zero] []      -- geboren an Tag 1
star    = MkGame [zero] [zero]  -- geboren an Tag 1
onehalf = MkGame [zero] [one]   -- geboren an Tag 2

-- Das Game `neg x` beschreibt dieselbe Situation wie `x`, nur mit den Rollen
-- des linken und rechten Spielers vertauscht.
neg :: Game -> Game
neg (MkGame xl xr) = MkGame (map neg xr) (map neg xl)

-- Die folgenden Prädikate bestimmen, ob der linke, rechte, als erstes ziehende
-- oder als zweites ziehende Spieler eine Gewinnstrategie besitzen.
isLeftPlayerWin :: Game -> Bool
isLeftPlayerWin (MkGame xl xr) = any isSecondPlayerWin xl && all isFirstPlayerWin xr

isRightPlayerWin :: Game -> Bool
isRightPlayerWin g = isLeftPlayerWin (neg g)

isFirstPlayerWin :: Game -> Bool
isFirstPlayerWin (MkGame xl xr) = any isSecondPlayerWin xl && any isSecondPlayerWin xr

isSecondPlayerWin :: Game -> Bool
isSecondPlayerWin (MkGame xl xr) = all isFirstPlayerWin xl && all isFirstPlayerWin xr

-- Seien `x` und `y` Games. Dann beschreibt `x <+> y` die Zusammensetzung
-- aus `x` und `y`. Das kann man sich so vorstellen, dass beide Spiele auf dem
-- Tisch liegen. Ein erlaubter Zug im zusammengesetzten Spiel besteht dann
-- (frei nach Wahl des ziehenden Spielers) entweder aus einem Zug im Spiel `x`
-- oder aus einem Zug im Spiel `y`.
infixl 6 <+>
(<+>) :: Game -> Game -> Game
x@(MkGame xl xr) <+> y@(MkGame yl yr) =
    MkGame
        ([ x' <+> y | x' <- xl ] ++ [ x <+> y' | y' <- yl ])
        ([ x' <+> y | x' <- xr ] ++ [ x <+> y' | y' <- yr ])

-- Praktische Synonyme
isZero     = isSecondPlayerWin
isFuzzy    = isFirstPlayerWin
isPositive = isLeftPlayerWin
isNegative = isRightPlayerWin

instance Eq Game where
    -- Zwei Games `x` und `y` gelten genau dann als gleich, wenn ihre Differenz
    -- Null ist.
    x == y = isZero (x <+> neg y)


-------------------------------------------------------------------------------
-- Beispiel: Streichholzspiel
-------------------------------------------------------------------------------

-- Beim Streichholzspiel liegen zu Beginn `n` Streichhölzer auf dem Tisch.
-- Abwechselnd müssen der linke und rechte Spieler jeweils zwischen einem und
-- drei Streichhölzer entfernen. Verlierer ist, wer keinen Zug mehr tätigen
-- kann.
--
-- Das Game `streichholz n` beschreibt die Spielsituation bei `n` Streichhölzern.
streichholz :: Nat -> Game
streichholz n = MkGame gs gs
    where gs = map streichholz $ filter (>= 0) [n-3..n-1]

demo :: [Bool]
demo = map (isFirstPlayerWin . streichholz) [0..20]


-------------------------------------------------------------------------------
-- Nimbers
-------------------------------------------------------------------------------

-- Ein Haufen beim Nim-Spiel besteht aus `n` Münzen. Beide Spieler dürfen von
-- einem solchen Haufen eine beliebige Anzahl Münzen wegnehmen (aber mindestens
-- eine).
--
-- Das Game `nim n` beschreibt einen Nim-Haufen mit `n` Münzen.
nim :: Nat -> Game
nim n
    | n == 0    = zero
    | otherwise = let gs = map nim [0..n-1] in MkGame gs gs

-- "Minimum excludant": Bestimmt die kleinste natürliche Zahl, die nicht
-- in der übergebenen Liste enthalten ist.
mex :: [Nat] -> Nat
mex xs = head $ filter (not . (`elem` xs)) [0..]

-- Die "Nimber-Addition" auf den natürlichen Zahlen
infixl 6 <++>
(<++>) :: Nat -> Nat -> Nat
n <++> m = mex $ [ n' <++> m | n' <- [0..n-1] ] ++ [ n <++> m' | m' <- [0..m-1] ]

-- Es gilt: nim n <+> nim m = nim (n <++> m)
