-- Define the types
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

data S
    = Let E (Bind E S)
    | Exp E
    | Hole_s
    | Cursor_s S
    deriving (Show)

data E
    = Plus E E
    | Num
    | Var
    | Hole_e
    | Cursor_e E
    deriving (Show)

type Bind a b = ([(a, b)], b)


-- Define the Substitutable type class

class Substitutable a where
    substitute :: a -> a -> a


-- Define instances of Substitutable for E and S

instance Substitutable E where
    substitute old new = new

instance Substitutable S where
    substitute old new = new

-- Example usage
-- Note how Haskell can infer the types and does not need 
-- an explicit instance of Substitutable for S

main :: IO ()
main = do
    let oldS = Exp Num
        newS = Exp (Plus Num Num)
    print $ substitute oldS newS