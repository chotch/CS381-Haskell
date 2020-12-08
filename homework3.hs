--Christien Hotchkiss
--Homework 3
--Semantics
--I found this to be the hardest homework assignment so far. I wish the documents were a little more specific, but it is also nice to be able to interpret errors as we wish. Like I dont know if i am able to change function definitions, if I am supposed to return Nothing or [4,3,Nothing,3,Nothing] for a stack. The hardest part for me was sem' in the second exrecise. I couldn't figure out how to preserve the state in the correct way while recursively going through all the commands if commands were conjoined using Seq. I think I have everything figured out, I pass all the test cases recomended in the document and some of my own.
--
--
--Exercise1
--abstract syntax definition.
--In order to properly run use sem [CmdS] (Just [])
type Prog = [CmdS]-- deriving Show
data CmdS = LD Int | ADD | MULT | DUP deriving Show
type Stack = [Int]
type D = Maybe Stack

--recursively call all the commands in the prog array and operate on stack. Use the stack return of the current command to be the stack input for the recursive call of sem function. The output is either a stack indicating all succesful opeations or Nothing, which means an operation in the prog array failed on the current stack.
--are we supposed to just return Nothing if any of the operations fails? or is an element of the final stack supposed to be nothing idicating that an operation fails there?
--the directions are not clear at all on this and I assume that my first guess is correct because the type of stack given to us in the problem is Stack = [Int]. If this definition were instead Stack = [Maybe Int] then I would go with my second guess on how to perfectly do this problem. 
--sem functions takes an array of commands as well as the current stack and outputs type D, which is Maybe Stack.
--done
sem :: Prog -> Maybe Stack ->  D
sem [] (Just x) = Just x
sem [] (Nothing) = Nothing
sem [x] (Just st) = semCmd x (Just st)
--sem (x:xs) st = sem xs case (semCmd x st) of
--	(Just x) -> x
--	(Nothing) -> []
sem (x:xs) (Just st) =  (sem xs ( (semCmd x (Just st))))
sem _ Nothing = Nothing
--done
--i assume the top of the stack (where things are added and popped) is the front of a list. Example the top of [4,5,2] is 4.
--helper function for semantics command
--takes a single command and the current contents of the stack and produces 
--a new stack that has the command processed
--if none of the pattern matching occurs. Then the contents of the stack are not valid for the current operation to take place so I return nothing
semCmd :: CmdS -> Maybe Stack -> D
semCmd (LD x) (Just st) = Just (x:st)
semCmd DUP (Just (x:xs)) = Just (x:x:xs)
semCmd ADD (Just (x:x1:xs)) = Just ((x+x1):xs)
semCmd MULT (Just (x:x1:xs)) = Just ((x*x1):xs)
semCmd _ (Just st) = Nothing
semCmd _ (Nothing) = Nothing


--programmings used for testing. All tests passed when used on an empty list or when used on a list that is already populated
test1 :: Prog
test1 = [LD 3, DUP, ADD, DUP, MULT]
test2 :: Prog
test2 = [LD 3, ADD]
test3 :: Prog
test3 = []
--
--Exercise2
--
--abstract syntax definition
data Cmd = Pen Mode | MoveTo Int Int | Seq Cmd Cmd deriving Show
data Mode = Up | Down deriving Show
type State = (Mode, Int, Int)
type Line = (Int, Int, Int, Int)
type Lines = [Line]


--semS :: Cmd -> State -> (State, Maybe Lines)
--semS (Pen Up) (_, i1, i2) = ((Up, i1, i2), Nothing)
--semS (Pen Down) (_, i1, i2) = ((Down, i1, i2), Nothing) 
--semS (MoveTo x1 x2) (Up, i1, i2) = ((Up, x1, x2), Nothing)
--semS (MoveTo x1 x2) (Down, i1, i2) = ((Down, x1, x2), Just [(i1,i2,x1,x2)])
--semS (Seq c1 c2) (m, i1, i2) = ((m, i1, i2), Nothing)
--semS (MoveTo x1 x2) (m, i1, i2) = ((m, x1, x2), [(i1,i2,x1,x2)])
--semS (Seq c1 c2) (st) =
--

--done
--this function takes in a command and a state and produces a new current state
--as well as what line that command produces
--this is correct and working
semS :: Cmd -> State -> (State, Lines)
semS (Pen Up) (_, i1, i2) = ((Up, i1, i2), [])
semS (Pen Down) (_, i1, i2) = ((Down, i1, i2), []) 
semS (MoveTo x1 x2) (Up, i1, i2) = ((Up, x1, x2), [])
semS (MoveTo x1 x2) (Down, i1, i2) = ((Down, x1, x2), [(i1,i2,x1,x2)])


--not done, this is all i need to do. How can i keep track of the current state
--which is essential when determing where a line goes to. If only input is Cmd and output is Lines
--this function takes in a command and produces Lines, which is a lis tof line
--call semS
--sem' :: Cmd -> State -> Lines
--sem' c startState = snd(semHelp c (startState, []))

--takes a command of the form Seq (Seq (Seq ....))
--and transofmrs it to an array of commands so it is easier to process by using helper function. Then it is sent to semTry2 with starting state of Up (0,0)
sem' :: Cmd -> Lines
sem' c = semTry2 (helper c) (Up, 0, 0)

--Function takes a state and list of commands and calls semS2 to find the new state and new lines drawn by this command. It stores the list of Lines and builds it recursevily by calling semTry2 on the rest of the array of commands
semTry2 :: [Cmd] -> State -> Lines
semTry2 (x:xs) st = (snd(semS x st)) ++ semTry2 xs (fst (semS x st))
semTry2 [] st = []

--takes a long command of the form Seq(Seq(Seq...)) and transofmrs it into a more readable and easier to operate on form of [C1, C2, C3]
helper :: Cmd -> [Cmd]
helper (Seq c1 c2) = (helper c1) ++ (helper c2)
helper c = [c]


--semHelp :: Cmd -> (State, Lines) -> (State, Lines)
--semHelp c (st, l) = case (semS c st) of
--	((m, x1, y1), Just [(x2,y2,x3,y3)]) -> ((m, x1, y1), [(x2,y2,x3,y3)])
--	((m, x1, y1), Nothing) -> (st, l)
--sem' c = case (semS (Seq c c) (Up, 0, 0)) of
--	((m, x1, y1), Just [(x2,y2,x3,y3)]) -> []
--	((m, x1, y1), Nothing) -> snd(semHelp c (Up, 0, 0))

--semHelp :: Cmd -> State -> (State, Lines)
--semHelp (Seq c c1) s = (semHelp c s) ++ (semHelp c1 (fst(semHelp c s)))
--semHelp c s = ((Up,1,2),[(100,100,100,100)])
--sem' c = snd(semS (Seq c c) (Up, 0, 0)) 
--sem' (Seq c1 c2) = sem' c1 ++ sem' c2
--sem' c = semS c (Up, 0 0) 
--sem' c = case (semS c (Up, 0, 0)) of
--	((mode, x1, y1), Just (x2,y2,x3,y3)) -> --append list and add lines together
--	((mode, x1, y1), Nothing) -> --append notothing

--semHelp:: Cmd -> State -> Lines
--semHelp (Seq c1 c2) (m, i1, i2) = semHelp c1 (m, i1, i2) ++ semHelp c2 (m, i1, i2)
--semHelp c (s) = case (semS c s) of 
--	((m, i1, i2), Just ([(x1, y1, x2, y2)])) -> [(x1,y1,x2,y2)]
--	((m, ii, iii), Nothing) -> []
--semHelp _ (s) = []
	

--semHelp :: State -> Maybe Lines -> Maybe Lines
--semHelp (mode, x1, x2) (Maybe xs) = Just xs
--semHelp (mode, x1, x2) (Nothing) = Nothing
--
--
-- the below are copied from last weeks homework assignment to test functionality of my mini logo code for this assignment

steps :: Int -> Cmd
steps 0 = (Pen Up) --dont write anything for 0 steps
--move to the top of the staircase, then put the pen down. Call recursive function that will draw the staircase until in reaces 0
steps x = Seq (Seq (Seq (Pen Up) (MoveTo x x)) (Pen Down)) (steps1 x)

--pen is already down by time we reach this function so no need to pput it down.
steps1 :: Int -> Cmd
steps1 0 = (Pen Up) -- have reached bottom step
--continually write one unit to the left and then one unit down Until we reach 0 0 then bring the pen up and staircase is complete
steps1 x = Seq (Seq ((MoveTo (x-1) x)) (MoveTo (x-1) (x-1))) (steps1 (x-1))

