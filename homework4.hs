-- Christien Hotchkiss
-- CS 381
-- Homework 4
-- THe only part of this homework that I struggled with was the last part of exercise two. I had the reasoning down, but couldn't figure out how to get the x and y values when they were returned from rect but wrapped in Just. Once I figured out that a helper function do the trick, it was pretty straightforward.
--
-- exercise 1 complete done 100% working
type Prog = [Cmd]
data Cmd = LD Int | ADD | MULT | DUP | INC | SWAP | POP Int
type Rank = Int
type Stack = [Int] -- deriving Show
type CmdRank = (Int, Int)

--part a done
--find the number of elements needed on stack (fst) and number of elements pushed to stack (snd)
rankC :: Cmd -> CmdRank
rankC (LD _) = (0,1)
rankC ADD = (2,1)
rankC MULT = (2,1)
rankC DUP = (0,1)
rankC INC = (1,1)
rankC SWAP = (2,2)
rankC (POP n) = (n, 0)

--call rank helper function to start Prog with an empty stack
rankP :: Prog -> Maybe Rank
rankP [] = Just 0
rankP p = rank p 0
-- rankP (x:xs) = Just (snd(rankC x))
-- rankP [] = Just 0

--ensure that there are enough elements already on the stack. If the number of 
--required elements (fst(rankC x) is greater than current) then return nothing
rank :: Prog -> Rank -> Maybe Rank
rank [] r = Just r
rank (x:xs) r | fst(rankC x) > r = Nothing
	      | otherwise = rank xs (r + (snd(rankC x) - fst(rankC x))) 

--use for testing purposes
ptest::Prog
ptest = [LD 1, LD 2, ADD, INC, DUP, DUP, SWAP, MULT, POP 1, LD 4]

ptest1::Prog
ptest1 = [LD 1, ADD]

ptest2::Prog
ptest2 = [INC, POP 1]

ptest3::Prog
ptest3 = [LD 2, LD 2, ADD, LD 4, LD 5, LD 5, POP 4]

ptest4:: Prog
ptest4 = [LD 2, LD 2, ADD, LD 4, LD 5, LD 5, POP 5]

--part b done
--first call rankP to check whether type correct
--semStatTC calls function sem
semStatTC:: Prog -> Maybe Stack
semStatTC p | typeCorrect p = Just (sem p []) --here instead of doing rankP p use sem p
	    | otherwise = Nothing

--the function sem can be simplified to no longer include D and Maybe Stack. Instead, since we type checked prior to calling sem, we can guarantee that a Stack will be returned so the new sem definition can be Sem -> Stack.
--if rankP of the program is nothing then it is not type correct
typeCorrect :: Prog -> Bool
typeCorrect p | rankP p == Nothing = False
	      | otherwise = True

--recurse through the program and update the stack by calling semCmd function. Most of which was implemented in previous homework
sem :: Prog -> Stack -> Stack
sem [] x = x
sem [x] st = semCmd x st
sem (x:xs) st = sem xs ((semCmd x st))

--semCmd does something for every possible operation. This assumes that the
--stack will be in valid condition prior to each operation because we twill type
--check before calling this function
semCmd :: Cmd -> Stack -> Stack
semCmd (LD x) st = x:st
semCmd DUP (x:xs) = x:x:xs
semCmd ADD (x:x1:xs) = (x+x1):xs
semCmd MULT (x:x1:xs) = (x*x1):xs
semCmd SWAP (x:x1:xs) = x1:x:xs
semCmd INC (x:xs) = (x+1):xs
semCmd (POP x) (x1:xs) | x == 0 = x1:xs
		      | otherwise = semCmd (POP (x-1)) xs
semCmd _ st = st

--exercise 2 complete
--a
--is done, still need to provide test cases
data Shape = X | TD Shape Shape | LR Shape Shape deriving Show
type BBox = (Int, Int)

--when adding a shape on top of another one (TD): we must first check the widths of the two shapes which is the x value and hence the fst in the tuple. The resulting BBox wil ltake the width of the wider box and will take the heigh of the sum of the two shapes.
--when adding a shape to the left of another one (LR): we must first check the heights of the two shapes which is the y value and the 2nd element in tuple. The resulting BBox will take the height of the taller box and will take the width of the of the widths of the two shapes (width is x value and fst in tuple0
bbox :: Shape -> BBox
bbox X = (1,1)
bbox (TD s1 s2) | fst (bbox s1) >= fst (bbox s2) = (fst (bbox s1), snd (bbox s1) + snd (bbox s2))
		| otherwise = (fst (bbox s2), snd(bbox s1) + snd(bbox s2))
bbox (LR s1 s2) | snd (bbox s1) >= snd (bbox s2) = (fst(bbox s1) + fst(bbox s2), snd(bbox s1))
		| otherwise = (fst(bbox s1) + fst(bbox s2), snd (bbox s2))

--b done
--both s1 and s2 must be rectangles
rect :: Shape -> Maybe BBox
rect X = Just((1,1))
--when addinga shape on top on another. You muste snure that the x width of s1 and s2 are equal. Then the resultant rectangle is the sum of the heights along with the width
rect (TD s1 s2) | rect s1 /= Nothing && rect s2 /= Nothing && fst ( helper(rect s1)) == fst (helper(rect s2)) = Just (fst (helper (rect s1)), snd (helper(rect s1)) + snd (helper(rect s2)))
	        | otherwise = Nothing
--when adding a shape to the left of another. Must ensure that y heights of s1 and s2 are equal. Then the resultant rectangle is the sum of the widths and the height
rect (LR s1 s2) | rect s1 /= Nothing && rect s2 /= Nothing && snd (helper(rect s1)) == snd (helper(rect s2)) = Just (fst (helper(rect s1)) + fst (helper(rect s2)), snd (helper(rect s1)))
		| otherwise = Nothing

--helper function to unravel the Just. Allows me to use fst and snd to gather x and y coordinates in the function above. Otherwise using fst does not work when the BBox is surrounded by a Just constructor
helper :: Maybe BBox -> (Int, Int)
helper (Just (x,y)) = (x,y)

