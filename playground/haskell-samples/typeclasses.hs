data S = Let E ( [E], S )
          | HoleS
          deriving (Show)

data E = Plus E E
        | Num
        | Var String
        | HoleE
        deriving (Show)

data WellFormed = WellFormed

class Substitutable a where
    substitute :: a -> a -> a

instance Substitutable a where
    substitute _ replacement = replacement

example = substitute 1 2

anotherExample = substitute (Let Num ([Num], HoleS)) HoleS

anotherAnotherExample = substitute Num HoleE


class Satisfiable op where
    at :: WellFormed -> op -> Bool
    maybe :: WellFormed -> op -> Bool
    necessarily :: WellFormed -> op -> Bool

instance Satisfiable S where
    at ast op = True
    at _ _ = False

    maybe _ HoleS = False
    maybe _ _ = True

    necessarily _ HoleS = False
    necessarily _ _ = True