
module One where

import Data.List

fst' (a,b) = a
--fst' (1,2) = 1

snd' (a,b) = b
--snd' (2,3) =3

head' (x1:[x]) = x1
--head' (1,2,3,4) = 1

tail' (x:xs) = [xs]
--tail' (2,3,4) = 4

min' x y = x - y

--pembatas

max' x y = x + y

--pembatas

length' [] = 0
length' (x:xs) = 1 + (length' xs)

--length' [1,2,3] -> 3
-- 1 + length' [2,3] -> 1 + 2
-- 1 + length' [3] -> 1 + 1
-- 1 + length [] = 1

last' [x] = x
last' (x:xs) = last' xs

-- pembatas

sum' [] = 0
sum' (x:xs) = x + sum' xs
-- sum' [3,4,5] = 3 + sum' [4,5] => 3+9
-- sum' [4,5] = 4 + sum' [5] => 4+5
-- sum' [5] = 5 + sum' [] =

take' n [] = []
take' 0 (x:xs) = []
take' n (x:xs) = x: take' (n-1) xs

--pembatas

drop' n [] = []
drop' 0 (x:xs) = (x:xs)
drop' n (x:xs) = drop' (n-1) xs

--pembatas

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
-- reverse' [1,2,3] = reverse [3,2] ++ [1] => [3,2,1]
-- reverse' [1,2,3,4] = reverse [2,3,4] ++ [1] => [4,3,2,1]

zip' [] [] = []
zip' [x] [y] = [(x,y)]
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys
--zip' [1,2,3] [4,5,6] = [(1,4)] ++ zip' (2,3) (5,6) => [(1,4),(2,5),(3,6)]
---zip' [2,3,4] [5,5,7] = [(2,5)] ++ zip' (3,4) (5,7) => [(2,5),(3,5),(4,7)]

null' [] = True
null' _ = False

--pembatas

product' [] = 0
product' [x] = x
product' (x:xs) = x* product' (xs)
--product' [1,2,3,4] = 1 * product' (2,3,4)
--product' [2,3,4,5] = 2 * product' (3,4,5)

concat' [] = []
concat' [[]] = []

--pembatas

zip3' [] [] [] = []
zip3' (x:xs) (y:ys) (z:zs) = [(x,y,z)] ++ zip3 xs ys zs
--zip3' [1,2,3] [4,5,6] [7,8,9] = [(1,4,7)] ++ zip3' (2,3) (5,6) (8,9) => [(1,4,7),(2,5,8),(3,6,9)]
---zip3' [2,3,4] [8,0,9] [6,7,8] = [(2,8,6)] ++ zip3' (3,4) (0,9) (7,8) => [(2,8,6),(3,0,7) (4,9,8)]
---zip3' [6,5,7] [6,9,4] [2,3,5] = [(6,6,2)] ++ zip3' (5,7) (9,4) (3,5) => [(6,6,2),(5,9,3),(7,4,5)]

elem' x [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs
--elem' 0 [1,2,3,4] = False
--elem' 1 [1,24,5,6] = True
--elem' 9 [1,2,34,5] = False
--elem' 1 [2,3,4,1] = True


