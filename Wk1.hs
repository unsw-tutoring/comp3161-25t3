module Wk1 where

-- ------------------
-- our first function
-- ------------------
addOne :: Int -> Int
addOne x = x + 1

-- ------------------
-- Tut Question
-- ------------------
mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

myBinop :: (Int -> Int -> Int) -> Int -> ([Int] -> Int)
myBinop f z [] = z
myBinop f z (n:ns) = n `f` (myBinop f z ns)

mySum2 :: [Int] -> Int
mySum2 ns = myBinop (+) 0 ns

myProduct2 :: [Int] -> Int
myProduct2 ns = (((myBinop (*)) 1) ns)

-- ------------------
-- POSTFIX vs. PREFIX operators
-- ------------------
add :: Int -> (Int -> Int)
add a b = a + b
add2 :: Int -> Int -> Int
add2 a b = (+) a b
add3 :: Int -> Int -> Int
add3 a b = a `add` b
add4 :: Int -> Int -> Int
add4 a b = add a b

-- ------------------
-- example of Partial Application
-- ------------------
myrepeat :: String -> Int -> String
myrepeat _ 0 = ""
myrepeat s n = s ++ myrepeat s (n - 1)

repeatHi :: Int -> String
repeatHi = myrepeat "hi"

-- ------------------
-- type vs. data
-- ------------------
type Item = Int

data RBColour
  = Red
  | Black

data RBTree
  = RBLeaf
  | RBNode RBColour Item RBTree RBTree 

-- ------------------
-- user defined types + pattern matching on those types
-- ------------------
data Clock = Clock Int Int

clockToString :: Clock -> String
clockToString (Clock hour minute) = "The time is " ++ show hour ++ ":" ++ show minute

-- ------------------
-- if expressions
-- ------------------
allowedToDrink :: Int -> Bool
allowedToDrink age = if age > 15 then True else False
-- allowedToDrink age = age > 15 -- better style

-- ------------------
-- yet another tut question
-- ------------------

{-

definition of list concatenation (++)

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys             -- definition 1
(x:xs) ++ ys = x : (xs ++ ys) -- definition 2

-}

{-
left identity: [] ++ xs == xs
    proven by definition 1
-}

{-
right identity: xs ++ [] == xs

base case: [] ++ [] == []
    proven by definition 1

inductive hypothesis (IH): xs ++ [] == xs

we want to prove: (x:xs) ++ [] == (x:xs)
    LHS = (x:xs) ++ []
        = x : (xs ++ []) -- by the definition 2
        = x : xs -- by the IH
        = RHS
-}

{-
associativity: (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

base case: ([] ++ ys) ++ zs == [] ++ (ys ++ zs)
    LHS = ([] ++ ys) ++ zs 
        = ys ++ zs -- by def 1
    RHS = [] ++ (ys ++ zs)
        = ys ++ zs -- by def 1
        = LHS

IH: (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

we want to prove: ((x:xs) ++ ys) ++ zs == (x:xs) ++ (ys ++ zs)
    LHS = ((x:xs) ++ ys) ++ zs
        = x : (xs ++ ys) ++ zs -- by def 2
        = x : ((xs ++ ys) ++ zs) -- by def 2
        = x : (xs ++ (ys ++ zs)) -- by IH
        = (x:xs) ++ (ys ++ zs) -- by def 2 backwards
        = RHS
-}
