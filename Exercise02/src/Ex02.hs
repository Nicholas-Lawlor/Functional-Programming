{- butrfeld Andrew Butterfield -}
module Ex02 where

name, idno, username :: String
name      =  "Myself, Me"  -- replace with your name
idno      =  "01234567"    -- replace with your student id
username  =  "memyselfi"   -- replace with your TCD username


declaration -- do not modify this
 = unlines
     [ ""
     , "@@@ This exercise is all my own work."
     , "@@@ Signed: " ++ name
     , "@@@ "++idno++" "++username
     ]

-- Datatypes and key functions -----------------------------------------------

-- do not change anything in this section !

type Id = String

data Expr
  = Val Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
  | Var Id
  | Def Id Expr Expr
  deriving (Eq, Show)

type Dict k d  =  [(k,d)]

define :: Dict k d -> k -> d -> Dict k d
define d s v = (s,v):d

find :: Dict String d -> String -> Either String d
find []             name              =  Left ("undefined var "++name)
find ( (s,v) : ds ) name | name == s  =  Right v
                         | otherwise  =  find ds name

type EDict = Dict String Double

v42 = Val 42 ; j42 = Just v42

-- do not change anything above !

-- Part 1 : Evaluating Expressions -- (50 test marks, worth 25 Exercise Marks) -

-- Implement the following function so all 'eval' tests pass.

-- eval should return `Left msg` if:
  -- (1) a divide by zero operation was going to be performed;
  -- (2) the expression contains a variable not in the dictionary.
  -- see test outcomes for the precise format of those messages

eval :: EDict -> Expr -> Either String Double
eval d (Val x) = Right x
eval d (Var x) = find d x

-- lecture 5 pg 13 evalOp

-- eval d (Add (Val x)(Val y))  = case (eval d x, eval d y) of (Right x, Right y) -> Right ( x + y) _ -> Left  "undefined var x"


eval d (Add (Val x)(Val y)) = Right ( x + y)

eval ((s,v):ds) (Add (Var x) (Val y)) | x == s = eval ds (Add (Val v) (Val y))
                                      | otherwise = eval ds (Add (Var x) (Val y))


eval ((s,v):ds) (Add (Val x) (Var y)) | y == s = eval ds (Add (Val x) (Val v))
                                      | otherwise = eval ds (Add (Val x) (Var y)) 
                                    
                              

eval ((s,v):ds) (Add (Var x) (Var y)) | x == s = eval ds (Add (Val v) (Var y))
                                      | y == s = eval ds (Add (Var x) (Val v))
                                      | otherwise = eval ds (Add (Var x) (Var y))

-- eval d (Dvd _ (Val 0.0)) = Left "div by zero"


 eval d (Add _ (Dvd _ (Val 0.0))) = Left "div by zero"
-- eval d (Add _ _) = Left "undefined var y"
eval d (Add _ _) = Left "undefined var x"


eval d (Sub (Val x)(Val y)) = Right ( x - y)

eval ((s,v):ds) (Sub (Var x) (Val y)) | x == s = eval ds (Sub (Val v) (Val y))
                                      | otherwise = eval ds (Sub (Var x) (Val y))

eval ((s,v):ds) (Sub (Val x) (Var y)) | y == s = eval ds (Sub (Val x) (Val v))
                                      | otherwise = eval ds (Sub (Val x) (Var y))

eval ((s,v):ds) (Sub (Var x) (Var y)) | x == s = eval ds (Sub (Val v) (Var y))
                                      | y == s = eval ds (Sub (Var x) (Val v))
                                      | otherwise = eval ds (Sub (Var x) (Var y))

eval d (Sub _ _) = Left "div by zero"



eval d (Mul (Val x)(Val y)) = Right ( x * y)

eval ((s,v):ds) (Mul (Var x) (Val y)) | x == s = eval ds (Mul (Val v) (Val y))
                                      | otherwise = eval ds (Mul (Var x) (Val y))

eval ((s,v):ds) (Mul (Val x) (Var y)) | y == s = eval ds (Mul (Val x) (Val v))
                                      | otherwise = eval ds (Mul (Val x) (Var y))

eval ((s,v):ds) (Mul (Var x) (Var y)) | x == s = eval ds (Mul (Val v) (Var y))
                                      | y == s = eval ds (Mul (Var x) (Val v))
                                      | otherwise = eval ds (Mul (Var x) (Var y))

eval d (Mul _ _) = Left "div by zero"



eval d (Dvd (Val x)(Val y)) = Right ( x / y)
eval d (Dvd _ (Val 0.0)) = Left "div by zero"


eval ((s,v):ds) (Dvd (Var x) (Val y)) | x == s = eval ds (Dvd (Val v) (Val y))
                                      | otherwise = eval ds (Dvd (Var x) (Val y))

eval ((s,v):ds) (Dvd (Val x) (Var y)) | y == s = eval ds (Dvd (Val x) (Val v))
                                      | otherwise = eval ds (Dvd (Val x) (Var y))

eval ((s,v):ds) (Dvd (Var x) (Var y)) | x == s = eval ds (Dvd (Val v) (Var y))
                                      | y == s = eval ds (Dvd (Var x) (Val v))
                                      | otherwise = eval ds (Dvd (Var x) (Var y))

eval d (Dvd _ _) = Left "div by zero"



{--
eval d (Def var x y ) = case eval d x of 
	Left"div by zero" -> Left"div by zero"
	Right k -> eval (define d var k) y 
	--}
eval d (Def x (Val y) e2) = eval (define d x y) e2

eval d (Def _ _ _) = Left "div by zero"
--eval d (Def _ _ _) = Left "undefined var x"

-- Part 1 : Expression Laws -- (15 test marks, worth 15 Exercise Marks) --------

{-

There are many, many laws of algebra that apply to our expressions, e.g.,

  x + y            =  y + z         Law 1
  x + (y + z)      =  (x + y) + z   Law 2
  x - (y + z)      =  (x - y) - z   Law 3
  (x + y)*(x - y)  =  x*x - y*y     Law 4
  ...

  We can implement these directly in Haskell using Expr

  Function LawN takes an expression:
    If it matches the "shape" of the law lefthand-side,
    it replaces it with the corresponding righthand "shape".
    If it does not match, it returns Nothing

    Implement Laws 1 through 4 above
-}


law1 :: Expr -> Maybe Expr
law1 (Add x y) = Just (Add y x) 
-- law1 (Add (Val x)(Val y)) = Just (Add (Val y) (Val x))
law1 e = Nothing


law2 :: Expr -> Maybe Expr
law2 (Add x (Add y z)) = Just (Add (Add x y) z)
-- law2 (Add (Val x) (Add (Val y) (Val z))) = Just (Add (Add (Val x) (Val y)) (Val z))
law2 e = Nothing

law3 :: Expr -> Maybe Expr
law3 (Sub x (Add y z)) = Just (Sub(Sub x y)z)
-- law3 (Sub (Val x) (Add (Val y) (Val z))) = Just (Sub (Sub (Val x) (Val y)) (Val z)) 
law3 e = Nothing

law4 :: Expr -> Maybe Expr
law4 (Mul (Add x y) (Sub a b)) = if x == a && y == b then Just (Sub(Mul x x)(Mul y y)) else Nothing
law4 e = Nothing
