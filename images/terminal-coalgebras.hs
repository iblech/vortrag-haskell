data NatF a = Zero | Succ a
type Nat    = Nu NatF
-- Typ der Peano-Zahlen, mit einem Element für +infty:
-- MkNu (Succ (MkNu (Succ (...))))

data IntListF a = Nil | Cons Int a
type IntList    = Nu IntListF
-- Typ der (endlichen und unendlichen) Listen von Ints

data IntTreeF a = Nil | Fork Int a a
type IntTree    = Nu IntTreeF
-- Typ der (endlichen und unendlichen) binären Int-Bäume
