swap :: (a', b') -> (b', a')
swap (a, b) = (b, a)

data MyMaybe a = MyNothing | MyJust a

data Shape a = MyLeft a | MyRight a

-- myFunc :: (Bool, a) -> Shape a -- inferred
myFunc (b, a) = if b then MyLeft a else MyRight a


-- Monads...

-- data Either a b = Left a | Right b
type Error = String

foo :: Int -> Either Error Char
foo _ = Right 'c'

bar :: Char -> Either Error [Int]
bar _ = Right [1,2,3]

baz :: [Int] -> Either Error Bool
baz _ = Right True

return' :: a -> Either Error a
return' = Right

bind' :: Either Error a -> (a -> Either Error b) -> Either Error b
bind' (Left a') _ = Left a'
bind' (Right b') f = f b'

main :: Either Error Bool
main = do
    c <- foo 3
    xs <- bar c
    baz xs

main' :: Either Error Bool
main' = return' 3 >>= foo >>= bar >>= baz

main'' :: Either Error Bool
main'' = return' 3 `bind'` foo `bind'` bar `bind'` baz

main''' :: Either Error Bool
main''' = ((Left 3 `bind'` foo) `bind'` bar) `bind'` baz

main'''' :: Either Error Bool
main'''' = bind' (bind' (bind' (Left 3) foo) bar) baz

main''''' :: Either Error Bool
main''''' = case foo 3 of
            Left e -> Left e
            Right c ->
                case bar c of
                    Left e -> Left e
                    Right xs -> baz xs

--

--   1    2    3    4    5
--   🪦   🪦   🪦    🪦   🪦 

-- A mole is disturbing a row of five graves. The mole is always
-- underneath one of the graves.

-- 1. Each day, we may attempt to catch the mole by
--    digging up a grave. If we found the mole, we win;
--    otherwise, we put the grave back in order and go to sleep.
-- 2. Each night, the mole must move from its current position to an
--    adjacent grave.

-- Find a sequence of digs that is guaranteed to find the mole.

move_mole :: Int -> [Int]
move_mole 5 = [4]
move_mole 1 = [2]
move_mole x = [x-1, x+1]

kill_mole :: Int -> Int -> [Int]
kill_mole d m = if d == m then [] else [m]

myReturn :: a -> [a]
myReturn x = [x]

myBind   :: [a] -> (a -> [b]) -> [b]
myBind x f = concatMap f x

move :: [Int] -> Int -> [Int]
move [] m = myReturn m
move (x:xs) m =
  let ys = kill_mole x m
      zs = myBind ys move_mole  -- concat (map move_mole ys)
  in
    myBind zs (move xs) -- concat (map (move xs) zs)

move' :: [Int] -> Int -> [Int]
move' [] m = myReturn m
move' (x:xs) m = do
    ys <- kill_mole x m
    zs <- move_mole ys
    move' xs zs

play :: [Int] -> [Int]
play xs = do
    x <- [1,2,3,4,5]
    move xs x
