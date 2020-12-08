--Christien Hotchkiss
--CS381
--Assignment 1
--The hardest functions for me were alignLeft, subbag, intersection of bags, and cycle. ALl others were fairly straitforward although I overlooked alot of the specifics in the inside function when I first did the assignment

import Data.List (nub, sort)
import Data.List

norm :: Ord a => [a] -> [a]
norm = sort . nub

--problem 1 define bag
type Bag a = [(a, Int)]
--	deriving (Show, Eq, Ord)
--b :: Bag Int
--b = [(5,1), (7,3), (2,1), (3,2), (8,1)]

--inserts element into multiset
ins :: Eq a => a -> Bag a -> Bag a
--ins a x = [(a, 1)] ++ x
ins a [] = [(a,1)]
ins a ((x, n):xs)
	| x == a = (x,n+1):xs
	| otherwise = [(x,n)] ++ ins a xs

 
--delete element from multiset
del :: Eq a => a -> Bag a -> Bag a
del a [] = []
del a ((x,n):xs)
	--the following two lines are commented out, but would be used if the goal of this function is to just remove one occurence of an element
	| x == a && n > 1 = (x, n-1):xs
	| x == a && n == 1 = xs
	--used if goal of function this function is supposed to remove all occurneces of the leement, setting the count to 0 rather than just decrementing it.
--	| x == a = xs
	| otherwise = [(x,n)] ++ del a xs

-- take a list of values and produce multiset
bag :: Eq a => [a] -> Bag a
bag [] = []
bag (x:xs) = ins x (bag xs)

--sort
--insSort ::  Bag a -> Bag a
--insSort [] = []
--insSort ((x,n):(x1,n1):xs)
--	| x < x1 = insSort ((x1,n1):xs)
--	| x > x1 = [(x1,n1), (x,n)] ++ insSort ((x,n):xs)
--insort ((x,n):(x1,n1))
--	| x < x1 = insSort []
--	| x > x1 = [(x1,n1), (x,n)] ++ insSort []
--	deriving (Show, Eq, Ord)

--changed function definition to have Ord a to utilize sorting function
-- determine if a bag is a subbag
subbag :: (Eq a, Ord a) => Bag a -> Bag a -> Bool
subbag [] [] = True
subbag [] b1 = True
subbag b1 [] = False
subbag b1 b2 = _subbag (quicksort b1) (quicksort b2)
--subbag ((x1,n1):xs) ((x2,n2):xs1)
--	| x1 == x2 && n1 <= n2 = subbag xs xs1
--	| x1 == x2 && n1 > n2 = False
--	| x1 /= x2  = subbag xs ((x2,n2):xs1)
--	| otherwise = False
--subbag b1 b2 = _subbag (quicksort b1) (quicksort b2)--(quicksort b2)
--subbag ((x1,n1):xs1) ((x2, n2):xs2)
--	| x1 == x2 && n1 <= n2 = True
--subbag (

--quicksort to use in subbag and isbag
--gathered from stackoverflow. Code was used to help sort bags in the subbag and isbag functions. https://stackoverflow.com/questions/7717691/why-is-the-minimalist-example-haskell-quicksort-not-a-true-quicksort
quicksort :: Ord a => Bag a -> Bag a
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
	where
		lesser = filter (<p) xs
		greater = filter (>=p) xs

--helper function to subbag
_subbag :: (Eq a, Ord a) => Bag a -> Bag a -> Bool
_subbag [] [] = True
_subbag [] b1 = True
_subbag b1 [] = False
_subbag [(x,n)] ((x2, n2):xs1)
	| x == x2 && n <= n2 = True
	| x > x2 = _subbag [(x,n)] xs1
	| otherwise = False
_subbag ((x1,n1):xs) ((x2,n2):xs1)
	| x1 == x2 && n1 <= n2 = _subbag xs xs1
	| x1 == x2 && n1 > n2 = False
	| x1 > x2 = _subbag ((x1,n1):xs) xs1
--	| x2 > x1 = _subbag xs ((x2,n2):xs1)
	| otherwise = False

	

-- changed function definition to include Ord a so I could utilize sorting
-- compute the intersection of two multisets
isbag :: (Eq a, Ord a) => Bag a -> Bag a -> Bag a
isbag [] [] = []
isbag b1 [] = []
isbag [] b2 = []
isbag b1 b2 = _isbag (quicksort b1) (quicksort b2)
--isbag ((x,n):xs) ((x1,n1):xs1)
--	| x == x1 && n <= n1 = xs
--	| x == x1 && n1 <= n = 

--isbag helper function to compute the intersection
_isbag :: (Eq a, Ord a) => Bag a -> Bag a -> Bag a
_isbag [] [] = []
_isbag [] b1 = []
_isbag b1 [] = []
_isbag ((x1,n1):xs) ((x2,n2):xs2)
	| x1 == x2 && n1 <= n2 = [(x1, n1)] ++ _isbag xs xs2
	| x1 == x2 && n2 < n1 = [(x1, n2)] ++ _isbag xs xs2
	| x1 < x2 = _isbag xs ((x2,n2):xs2)
	| x2 < x1 = _isbag ((x1,n1):xs) xs2



-- compute number of elements in a bag
size :: Bag a -> Int
size [] = 0
size ((x,n):xs) = n + size xs


--problem 2 define data types
--
type Node = Int
type Edge = (Node, Node)
type Graph = [Edge]
type Path = [Node]

--comment out test vars
--g :: Graph
--g = [(1,2),(1,3),(2,3),(2,4),(3,4)]

--h :: Graph
--h = [(1,2), (1,3), (2,1), (3,2), (4,4)]

--isMember list [] = False
--isMember n (x:xs)
--	| n == x = True
--	| otherwise = isMember n xs

--rd :: [Node] -> [Node]
--rd [] = []
--rd (x:xs)
--	| x `elem` xs = rd xs
--	| otherwise = x : rd xs


--compute list of nodes in a graph
nodes :: Graph -> [Node]
nodes [] = []
nodes ((x,y):xs)
--	| x /= y = [x] ++ [y] ++ rd (nodes xs)
	| otherwise = norm (([x]++[y]) ++ norm (nodes xs)) 
--	++  norm (nodes xs)
--nodes ((x,y):xs)
--	| x /= y =  [x] ++ [y] ++ nodes xs
--	| otherwise = [x] ++ nodes xs

--computes list of successors for a node
suc :: Node -> Graph -> [Node]
suc a [] = []
suc a ((x,y):xs)
	| x == a = [y] ++ (suc a xs)
	| otherwise = suc a xs

--detaches a node and all of its edges from a graph
detach :: Node -> Graph -> Graph
detach a [] = []
detach a ((x,y):xs)
	| x == a || y == a = (detach a xs)
	| otherwise = [(x,y)] ++ (detach a xs)


--create a cycle of any given number
cyc :: Int -> Graph
--i :: Int
--cyc 0 = []
cyc 0 = []
--cyc 1 = [(1,2)]
cyc a
	| otherwise= rmOne( norm(cyc (a-1) ++ [(a-1, a)] )  ++ [(a,1)])
--	| a == 1 = [(1,2)]
--	| otherwise = cyc(a-1) ++ [((a-1), a)]
--cyc a = cyc (a-1)  ++ [(a, a-(a-1))]

--used in implementation of cyc function
rmOne :: Graph -> Graph
rmOne [x] = [x]
rmOne ((x,y):xs)
	| y == 1 = rmOne xs
	| otherwise = [(x,y)] ++ rmOne xs

--problem 3! define data types	

type Number = Int
type Point = (Number, Number)
type Length = Number

data Shape = Pt Point
	| Circle Point Length
	| Rect Point Length Length
	deriving Show

type Figure = [Shape]
type BBox = (Point, Point)

--f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]
--compute the width of a shape
width :: Shape -> Length
width (Circle (_,_) l) = 2 * l
width (Pt (_,_)) = 0
width (Rect (p1, p2) l1 l2)
	| l1 > l2 = l1
	| otherwise = l2

--computes the bounding box of a shape
bbox :: Shape -> BBox
bbox (Pt (p1, p2)) = ((p1,p2), (p1,p2))
bbox (Circle (p1, p2) r) = (((p1-r), (p2-r)), ((p1+r), (p2+r)))
bbox (Rect (p1,p2) l1 l2) = ((p1, p2), ((p1 + l1), (p2+l2)))

--computes minimum x coordinate of a shape
minX :: Shape -> Number
minX (Pt (p1,_)) = p1
minX ( Circle (p1, _) r) = p1 - r
minX (Rect (p1,p2) l1 l2) = p1 

--move the position of a shape by a vector
move :: Shape -> Point -> Shape
move (Pt (p1,p2)) (p3,p4) = Pt (p1+p3, p2 + p4)
move (Circle (p1, p2) r) (p3,p4) = Circle (p1+p3, p2+p4) r
move (Rect (p1,p2) l1 l2) (p3,p4) = Rect (p1+p3, p2+p4) l1 l2

--change shapes position so that its minX is equal to number given
moveToX :: Number -> [Shape] -> [Shape]
moveToX x [] = []
moveToX x [(Pt(p1,p2))] = [Pt (x,p2)]
moveToX x [(Circle (p1,p2) r)] = [Circle (x+r, p2) r]
moveToX x [(Rect (p1,p2) l1 l2)] = [Rect (x,p2) l1 l2]
moveToX x ((Pt (p1,p2)):xs) = [Pt (x, p2)] ++ moveToX x xs
moveToX x ((Circle (p1,p2) r):xs) = [Circle (x+r, p2) r] ++ moveToX x xs
moveToX x ((Rect (p1,p2) l1 l2):xs) = [Rect (x, p2) l1 l2] ++ moveToX x xs

--find min x of an array
findMin :: [Number] -> Number
findMin [] = 0
findMin x = minimum x

--find minimum x value of each shape in a figure
findMinimumValue :: Figure -> [Number]
findMinimumValue [] = []
findMinimumValue (shape) =  (map minX shape)


--TODO use moveToX and findMinimumValue
--transforms a figure into another where all shapes have same minX
alignLeft :: Figure -> Figure
alignLeft [] = []
alignLeft fig = moveToX (findMin (findMinimumValue fig)) fig
--alignLeft (sh:sh1:xs) = [(moveToX (findMin (findMinimumValue (sh:sh1:xs))) sh)] ++ [(moveToX (findMin (findMinimumValue (sh:sh1:xs))) sh1)] ++ alignLeft (sh1:xs) --[Pt(4,4), Pt(5,5)] ++ (alignLeft (sh1:xs))
--alignLeft (sh:xs) = [(moveToX (findMin (findMinimumValue (sh:xs))) sh)] ++ alignLeft xs

--	| otherwise = moveToX (minimum (map minX sh:xs)) sh -- : alignLeft xs
--	| otherwise = [moveToX 30 sh] ++ alignLeft xs
--alignLeft ((Pt (p1,p2)):xs)
--	| otherwise = map moveToX ((5) ((Pt (p1,p2)) : xs))
--	| otherwise = map (\moveToX -> p2 Pt(p1,p2)) ((Pt (p1,p2)) : xs)
--	| otherwise = [moveToX 5 (Pt (p1,p2))] ++ alignLeft xs
--alignLeft ((Circle (p1,p2) r):xs)
--	| otherwise = [moveToX 5 (Circle (p1,p2) r)] ++ alignLeft xs
--alignLeft ((Rect (p1,p2) l1 l2): xs)
--	| otherwise = [moveToX 5 (Rect (p1,p2) l1 l2)] ++ alignLeft xs

--map the moveToX function to all elements of the [Shape]
--
--
--check whether one shape is in another (contained within)
inside :: Shape -> Shape -> Bool

--point inside a point
inside (Pt (p1,p2)) (Pt (p3,p4))
	| p1 == p3 && p2 == p4 = True
	| otherwise = False

--point inside a circle
inside (Pt (p1, p2)) (Circle (p3,p4) r)
	| sqrt(fromIntegral((p3-p1)*(p3-p1) + (p4-p2) * (p4-2))) <= fromIntegral r = True	
--	| p1 <= p3 + r && p2 <= p4 + r = True
--	| p1 <= p3 + r && p2 >= p4 - r = True
--	| p1 >= p3 - r && p2 <= p4 + r = True
--	| p1 >= p3 - r && p2 >= p4 -r = True
	| otherwise = False

-- point inside a rectangle
--figure out what a rectangle is
inside (Pt (p1,p2)) (Rect (p3,p4) l1 l2)
	| p1 <= p3 + l1 && p1 >= p3 && p2 <= p4 + l2 && p2 >= p4 = True
	| otherwise = False

--circle inside a circle
inside (Circle (p1,p2) r1) (Circle (p3,p4) r2)
	| p1 + r1 <= p3 + r2 && p2 + r1 <= p4 + r2 = True
	| otherwise = False

--circle inside a pooint
inside (Circle (p1,p2) r) (Pt (p3,p4))
	| r == 0 && p1 == p3 && p2 == p4 = True
	| otherwise = False

--circle inside a rectangle
inside (Circle (p1,p2) r) (Rect (p3,p4) l1 l2)
	| p1 - r >= p3 && p2 - r >= p4 && p1 + r <= p3 + l1 && p2 + r <= p4 + l2 = True
	| otherwise = False

--rectangle inside a point
inside (Rect (p1,p2) l1 l2) (Pt (p3,p4))
	| p1 == p3 && p2 == p4 && l1 == 0 && l2 == 0 = True
	| otherwise = False

--rectangle inside a rectangle
inside (Rect (p1,p2) l1 l2) (Rect (p3,p4) l3 l4)
	| p1 + l1 <= p3 + l3 && p2 + l2 <= p4 + l4 && p1 >= p3 && p2 >= p4 = True
	| otherwise = False


--rectangle inside a circle
inside (Rect (p1,p2) l1 l2) (Circle (p3,p4) r)
	| sqrt(fromIntegral(l1*l1 + l2*l2)) / 2 <= (fromIntegral r) && p1> p3-r && p1 +l1 < p3 + r && p2 > p4 - r && p2 + l2 < p4 + r = True
--	| p3 - r <= p1 && p4 - r <= p2 && 
	| otherwise = False
