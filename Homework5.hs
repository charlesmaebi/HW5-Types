-- Name : Charles Aebi
-- Date Due : 2/25/2026

module Homework5 where

import HW5Types

semCmd :: Cmd -> Stack -> Maybe Stack
-- load an integer onto the stack
semCmd (LDI i) s = Just((I i):s)

-- load a boolean onto the stack
semCmd (LDB b) s = Just((B b):s)

-- Add the top values on the stack and push result onto the stack
-- If there aren't two integers return Nothing
semCmd ADD ((I i):(I i'):s) = Just((I (i + i')) : s)
semCmd ADD _ = Nothing

-- Multiply the top values on the stack and push result onto stack
-- If there aren't two integers return Nothing
semCmd MULT ((I i):(I i'):s) = Just((I (i * i')) : s)
semCmd MULT _ = Nothing

-- If top value is less than next push True onto stack
-- otherwise push false
semCmd LEQ ((I i):(I i'):s) = Just ((B (i <= i')):s)
semCmd LEQ _ = Nothing

-- If top of stack is True execute first program
-- If False execute second program
-- If the stack is empty return nothing
semCmd (IFELSE [] _ ) ((B True):s) = Just s
semCmd (IFELSE p1 _ ) ((B True):s) = sem p1 s
semCmd (IFELSE _ [] ) ((B False):s) = Just s
semCmd (IFELSE _ p2 ) ((B False):s) = sem p2 s
-- Case - empty stack or top is not boolean
semCmd (IFELSE _ _) _ = Nothing

-- complete duplicate DUP to duplicate the top value on the
-- stack both integers and boolean values can be duplicated
-- if the stack is empty return Nothing
semCmd DUP ((I i):s) = Just ((I i) : (I i) : s)
semCmd DUP ((B b):s) = Just ((B b) : (B b) : s)
semCmd DUP _ = Nothing

-- If there is an integer on top of the stack, increment by 1
-- If the stack is empty, return nothing
semCmd INC ((I i):s) = Just ((I (i+1)):s)
semCmd INC _ = Nothing

-- Pull two elements from the top of the stack and swap postitions
-- If stack is empty, return nothing
semCmd SWAP (x:y:s) = Just (y:x:s)
semCmd SWAP _ = Nothing

-- Pop an element from the stack, returning a stack without element
-- If stack is empty, return nothing
semCmd (POP k) s =
    if length s >= k
        then Just (drop k s)
        else Nothing

-- Catch any undefined commands or errors
semCmd _ _ = Nothing

-- Implement rankC to map commands to rank
-- Format (n,m)
-- n - elements the command pops
-- m - how many elements the command pusheds
rankC :: Cmd -> CmdRank
rankC (LDI _) = (0, 1)
rankC (LDB _) = (0, 1)
rankC ADD = (2, 1)
rankC MULT = (2, 1)
rankC LEQ = (2, 1)
rankC DUP = (1, 2)
rankC INC = (1, 1)
rankC SWAP = (2, 2)
rankC (POP k) = (k, 0)
rankC (IFELSE _ _) = (1, 0)

-- rankP checks if a programs is rank safe recursively
-- Case 1: IFELSE block
-- Check both branches of condition are rank safe, continuing
-- recursively with the minimum rank from both branches
-- Case 2: Any other command
-- Check each command in a program to ensure rank safety until
-- base case is reached
rankP :: Prog -> Rank -> Maybe Rank
rankP [] r = Just r
rankP (c:cs) r = 
    case c of 
        IFELSE p1 p2 ->
            if r < 1
                then Nothing
                else 
                    let rAfterCond = r - 1
                        rank1 = rankP p1 rAfterCond
                        rank2 = rankP p2 rAfterCond
                    in case (rank1, rank2) of 
                        (Just r1, Just r2) -> rankP cs (min r1 r2)
                        _ -> Nothing
        _ -> 
            let (n, m) = rankC c
            in if r < n
                then Nothing
                else rankP cs (r - n + m)

--Runs the program by:
--Checking if the stack has enough elements to run safe
--If not enough elements, return nothing
--If program is rank safe, run using sem
--Return results of sem, either A stack or nothing
run :: Prog -> Stack -> Result
run p s = 
    case rankP p (length s) of
        Nothing -> RankError
        Just _ -> case sem p s of
            Just s' -> A s'
            Nothing -> TypeError


-- sem applies all the commands in the program to the stack
sem :: Prog -> Stack -> Maybe Stack
sem [] s = Just s       -- no program: succeed with current stack
sem (c:cs) s =
  case semCmd c s of
    Just s' -> sem cs s'
    Nothing -> Nothing  -- stop execution on Nothing

