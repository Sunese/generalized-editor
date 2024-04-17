data S = Let E ( [E], S )
          | HoleS
          deriving (Show)

data E = Plus E E
        | Num
        | Var String
        | HoleE
        deriving (Show)

class Substitutable a where
    substitute :: a -> a -> a

instance Substitutable a where
    substitute _ replacement = replacement

example = substitute 1 2

anotherExample = substitute (Let Num ([Num], HoleS)) HoleS

anotherAnotherExample = substitute Num HoleE