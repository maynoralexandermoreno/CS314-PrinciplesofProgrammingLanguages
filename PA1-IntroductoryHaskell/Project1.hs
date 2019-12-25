module Project1 where
-- =============================== Triples ==========================================
-- triples returns the number of items in a list that are divisible by 3.
--
-- > triples [1,2,3,4,5,6]
-- 2
-- > triples [3,33,333]
-- 3
-- > triples [0,1,4]
-- 1

triples :: [Integer] -> Integer
triples [] = 0
triples (x:xs) = if mod x 3 == 0 then 1 + triples xs else triples xs

-- ================================== Hailstone ======================================
-- The hailstone sequence takes a positive number n and repeatedly applies
-- this transformation: if n is even, it divides n by 2; otherwise, it
-- multiplies n by 3 and adds one. The sequence ends when it reaches 1.
--
-- hailstone returns the complete sequence beginning with a particular number.
-- You may assume that the number is positive.
--
-- > hailstone 4
-- [4,2,1]
-- > hailstone 6
-- [6,3,10,5,16,8,4,2,1]
-- > hailstone 7
-- [7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]

hailstone :: Integer -> [Integer]
hailstone 1 = [1]
hailstone n = if (even n) then (n:hailstone(div n 2)) else (n:hailstone(n*3+1))

-- =============================== Centroid ======================================
-- The centroid of a list of points is a point whose x (and y) coordinates are
-- the means of the x (and y) coordinates of the points in the list.
--
-- You may assume the list contains at least one point.
--
-- > centroid [Pt 1 1, Pt 2 2]
-- Pt 1.5 1.5
-- > centroid [Pt (-1.5) 0, Pt 3 2, Pt 0 1]
-- Pt 0.5 1.0

data Point = Pt Double Double deriving (Show, Eq)

denom :: [Point] -> Double
denom [] = 0.0
denom (x:xs) = 1.0 + denom(xs)

getx :: Point -> Double
getx (Pt a _) = a

gety :: Point -> Double
gety (Pt _ b) = b

sumx :: [Point] -> Double
sumx [] = 0.0
sumx (h:hs) = getx(h) + sumx(hs)

sumy :: [Point] -> Double
sumy [] = 0.0
sumy (h:hs) = gety(h) + sumy(hs)

centroid :: [Point] -> Point
centroid [Pt x y] = Pt x y
centroid (h:hs) = Pt ((getx(h) + sumx(hs))/(1.0 + denom(hs))) ((gety(h) + sumy(hs))/(1.0 + denom(hs)))

-- ============================= Tree Flip =====================================
-- mirror returns a tree with the same shape and contents as its argument, but
-- flipped left for right.
--
-- > mirror (Bin (Bin Tip 1 (Bin Tip 2 Tip)) 3 Tip)
-- Bin Tip 3 (Bin (Bin Tip 2 Tip) 1 Tip)

data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)
mirror :: Tree a -> Tree a
mirror Tip = Tip
mirror (Bin x y z) = Bin (mirror z) y (mirror x)

-- ============================= Strictly Binary Tree ==========================
-- In a strictly binary tree, each node has either 0 children or 2 children.
--
-- > strict (Bin Tip 1 Tip)
-- True
-- > strict (Bin Tip 1 (Bin Tip 2 Tip))
-- False

strict :: Tree a -> Bool
strict Tip  = True
strict (Bin Tip a (Bin Tip b Tip)) = False
strict (Bin (Bin Tip a Tip) b Tip) = False
strict (Bin x y z) = (strict x) && (strict z)

-- ============================= Near Balanced Tree ========================
-- A tree is near balanced if the left and right sub-trees of each node differ
-- in height by at most 1.
--
-- > near_balanced (Bin Tip 1 (Bin Tip 2 Tip))
-- True
-- > near_balanced (Bin Tip 1 (Bin Tip 2 (Bin Tip 3 Tip)))
-- False
-- > near_balanced (Bin (Bin Tip 2 Tip) 1 (Bin Tip 2 (Bin True 3 Tip)))
-- True
-- > near_balanced (Bin (Bin Tip 2 Tip) 1 (Bin Tip 2 (Bin (Bin Tip 4 Tip) 3 Tip)))
-- False

tree_height :: Tree a -> Integer
tree_height Tip = 0
tree_height (Bin x y z) = 1 + (max (tree_height x) (tree_height z))


near_balanced :: Tree a -> Bool
near_balanced Tip = True
near_balanced (Bin Tip a Tip) = True
near_balanced (Bin x y z) =
	if ((tree_height x) - (tree_height z) == 0) then True
	else if ((tree_height x) - (tree_height z) == 1) then True
	else if ((tree_height x) - (tree_height z) == -1) then True
	else False


-- near_balanced (
