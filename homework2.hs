--numUniques :: (Eq a) => [a] -> Int
--numUniques = length . nub
--Christien Hotchkiss
--CS381
--Assignment2
--Syntax
--This was easier than the first assignment for me. I got more used to writing recursive functions in Haskell. It was a bit hard at the beginning just to figure out how to use constructors within abstract syntax, but once part a of both 1 and 2 were complete, the rest of the assignment was pretty straitforward. 
--

--1a
--abstract syntax for mini logo
--type Na = Stringi--type Nu = Int

--cmd ::= pen mode
-- |moveto(pos,pos)
-- | def name (pars) cmd
-- | call name (vals)
-- | cmd ; cmd
-- mode ::= up | down
-- pos ::= num|name
-- pars ::= name, pars | name
-- vals ::= num,vals | name

data Cmd = Pen Mode
	| Moveto (Pos, Pos) 
	| Def String ( Pars ) Cmd 
	| Call String ( Vals ) 
	| Command Cmd Cmd -- | deriving (Show, Eq)
--	| NoOp
	deriving Show

data Mode = Up 
	| Down
	deriving Show -- | deriving (Show, Eq)

data Pos = I Int 
	| S String -- | deriving (Show, Eq)
	deriving Show

data Pars = StrPars String Pars 
	| Str String -- | deriving (Show, Eq)
	deriving Show

data Vals = InVals Int Vals 
	| In Int -- | deriving (Show, Eq)
	deriving Show

--1b
--macro vector to draw line
--from (x1,y1) to (x2,y2) (move pen to first point, put pen down, move pen to second point, pull the pen up
-- from CMD use Def String Pars Cmd where String = "vector" Pars (x1,y1, x2, y2)
-- and command = move to x1,y1 pen down, move to x2,y2 pen up
--first thing we must do is move pen up. This ensures that pen is not already down so we do not make unintended marks while moving from some random point to point x1, y1
--
vector = Def "vector" (StrPars "x1" ( StrPars "y1" (StrPars "x2" (Str "y2")))) (Command (Command (Pen Up) (Command (Moveto (S "x1", S "y1")) (Pen Down))) (Command (Moveto(S "x2", S "y2")) (Pen Up)))


--1c steps function
--create a stair like figure from Moveto and Pen Up Pen Down commands
steps :: Int -> Cmd
steps 0 = (Pen Up) --dont write anything for 0 steps
--move to the top of the staircase, then put the pen down. Call recursive function that will draw the staircase until in reaces 0
steps x = Command (Command (Command (Pen Up) (Moveto (I x, I x))) (Pen Down)) (steps1 x)

--pen is already down by time we reach this function so no need to pput it down.
steps1 :: Int -> Cmd
steps1 0 = (Pen Up) -- have reached bottom step
--continually write one unit to the left and then one unit down Until we reach 0 0 then bring the pen up and staircase is complete
steps1 x = Command (Command ((Moveto (I (x-1), I x))) (Moveto (I (x-1), I (x-1)))) (steps1 (x-1))


--2a
--define abstract syntax for
--circuit ::= gates links
--gates ::= num:gateFn ; gates | epsilon
--gateFn :: = and | or | xor | not
--links ::= from num.num to num.num; links | epsilon
--
--
--data Circuit = GL (Gates, Links) --deriving Show

--make INT because I is already used for problem 1
--data Gates = INT (Int, GateFn) Gates | NoOp --deriving Show

--data GateFn = AND | OR | XOR | NOT --deriving Show

--data Links = FR (Int, Int) (Int, Int) Links | NoOP --deriving Show



data Circuit = GL (Gates, Links) deriving Show

data Gates =  GTS [(Int, GateFn)] deriving Show -- | NoOp
data GateFn = AND| OR |XOR | NOT deriving Show
data Links = LK [Link] deriving Show -- | NoOP 
data Link = FROM (Int, Int) (Int, Int) deriving Show

--2b
--create the half adder as haskell data type
--halfAdder :: Circuit
--halfAdder = GL (( (INT (1, XOR) (INT (2, AND) NoOp))), (FR (1, 1) (2, 1) (FR (1,2) (2,2)  NoOP)))
--
--use the constructors and data types created in part a to define the half adder portrayed in homework document
halfAdder :: Circuit
halfAdder = GL (GTS([(1,XOR), (2,AND)]), LK([FROM(1,1) (2,1), FROM (1,2) (2,2)]))


--2c
--generate pretty printer for half adder and abstract syntax
--figure out how to print new line.
--	if i am in ghci and do ppCir halfAdder. it just prints \n as literals
--	but if i use putStr (ppCir halfAdder) it interprets \n and prints a new line
--	is there a way to accurately print a new line in ghci without using putStr



--helper function for ppC
--this takes the array of gates and prints them as according to homework file
ppG :: [(Int, GateFn)] -> String
ppG [] = ""
--use pattern matching (types can either be AND, OR, XOR, NOT). Then determine if they are 0, 1 or 2
ppG ((x,AND):xs) | x == 0 = "0:and;\n" ++ ppG xs
		| x == 1 = "1:and;\n" ++ ppG xs
		| x == 2 = "2:and;\n" ++ ppG xs

ppG ((x,OR):xs) | x == 0 = "0:or;\n" ++ ppG xs
		| x == 1 = "1:or;\n" ++ ppG xs
		| x == 2 = "2:or;\n" ++ ppG xs 

ppG ((x,XOR):xs) | x == 0 = "0:xor;\n" ++ ppG xs
		| x == 1 = "1:xor;\n" ++ ppG xs
		| x == 2 = "2:xor;\n" ++ ppG xs 

ppG ((x,NOT):xs) | x == 0 = "0:not;\n" ++ ppG xs
		| x == 1 = "1:not;\n" ++ ppG xs
		| x == 2 = "2:not;\n" ++ ppG xs 



--helper function for ppL. It will print a single link using the . format specified in homework. The format is x.y to x1.y1
takeLink :: Link -> String
takeLink (FROM (x,y) (x1,y1)) = show x ++ "." ++ show y ++ " to " ++ show x1 ++ "." ++ show y1


--helper function for ppC. It will take the array of Links and print out each link on a new line using the takeLink helper function. Then recursively go through the entire list, printing all links
ppL :: [Link] -> String
ppL [] = ""
ppL (x:xs) = "from " ++ takeLink x ++ ";\n" ++ ppL xs

--instance Show Circuit where Show = ppCir

--pretty printer for Circuit is called ppC


--not sure exactly how we are expected to format this. The uncommented version requires the user to do putStr (ppCir halfAdder) to show the correct string where \n are characterized as new line characters rather than \n.
--if using the commands ppC halfAdder in ghci is supposed to output the correct format, then the function to do this are the next two lines that are currently commented:
--ppC :: Circuit -> IO()
--ppC (GL (GTS x,LK y)) = putStr(ppG x ++ ppL y)
ppC :: Circuit -> String
ppC (GL (GTS x, LK y)) = (ppG x ++ ppL y)
--ppG calls function to take care of printing gates
--ppL calls function to take care of printing links


--ppCir :: Circuit -> String
--ppCir (GL (x,y)) = ppGs x  ++ ";\n"  ++ ppLs y

--ppG :: Gates -> Gates
--ppG g = g
--ppGs :: Gates -> String
--ppGs (NoOp) = "NoOp"
--ppG [] = ""
--ppG (x:xs) = "hi"
--ppGs g = "hi"

--ppLs :: Links -> String
--ppLs l = "hey"
