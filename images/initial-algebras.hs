data NatF a = Zero | Succ a
type Nat    = Mu NatF
-- Typ der (endlichen) Peano-Zahlen

data IntListF a = Nil | Cons Int a
type IntList    = Mu IntListF
-- Typ der (endlichen) Listen von Ints

data IntTreeF a = Nil | Fork Int a a
type IntTree    = Mu IntTreeF
-- Typ der (endlichen) binären Bäume von Ints
