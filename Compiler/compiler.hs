{-
 - Authors      : Chau Ying Kot & Doran Kayoumi
 - Date         : 22.01.2020
 - Description  :
 - All of the `compileExp` functions returns a tuple containing
 - (cmd, length of the comd, nb variable defined/used by the cmd)
 - cmd being the `assembly` instruction.
 -
 - The map argument used by all the function is a list containing all the `assembly` instructions
 - for the current program being compiled.
 -
 - The binary operation use the reverse Polish notation.
-}
module Lab13 where

import  Data.Char

type Op = Char
type Name = [Char]

data Exp =
  Cst Int
  | Bin Op Exp Exp
  | Un Op Exp
  | If Exp Exp Exp
  | Var Name
  | Let Name Exp Exp
  | Func Name [Exp]
  | Def Name [Name] Exp
  | App Name [Exp]
  deriving (Show)

data Instruction = ADD | DOT | HALT | INT Int | JMP Int | JZR Int | LINK Int | LOAD Int | MPY | STORE Int | SUB | UNLK | EXIT Int | CALL Int deriving Show

-- get the value correponding to the variable name
offset name ((key,val):map)
    | name == key = val
    | otherwise = offset name map


compileExp (Cst int) _ _ = ([INT int], 2, 0)

-- it's a binary operator
compileExp (Bin op xLeft xRight) map vars =
    (cLeft ++ cRight ++ [opCode], lLeft+lRight+1, max vLeft vRight)
    where
        opCode = case op of
            '+' -> ADD
            '-' -> SUB
            '*' -> MPY
        (cLeft, lLeft, vLeft ) = compileExp xLeft map vars
        (cRight, lRight, vRight) = compileExp xRight map vars

-- it's a comparator
compileExp (If xCond xThen xElse) map vars =
    -- plus 4 because we add JZR n and JMP m
    (code, lCond+lThen+lElse+4, max vCond $ max vThen vElse)
    where
        (cCond, lCond, vCond) = compileExp xCond map vars
        (cThen, lThen, vThen) = compileExp xThen map vars
        (cElse, lElse, vElse) = compileExp xElse map vars
        code = cCond ++ (JZR $ lThen + 2):cThen ++ (JMP $ lElse):cElse

-- it's a Let declaration
compileExp (Let name eqExp inExp) map vars =
    (code, eqLength+inLength+2, max eqVars inVars + 1)
    where
        (eqCode, eqLength, eqVars) = compileExp eqExp map vars
        (inCode, inLength, inVars) = compileExp inExp ((name,vars+1):map) (vars+1)
        code = eqCode ++ (STORE $ vars+1):inCode

-- If it a variable, load the value stocked
compileExp (Var name) map _ = ([LOAD $ offset name map], 2, 0)

-- Complie a definition
compileExp (Def func args body) map _ = (code, bLenght+7, 0)
    where
        nargs = length args
        (bCode, bLenght, bVars) =
            compileExp body (zip args [-nargs-1..]) 0
        code = LINK bVars:bCode ++ [STORE $ (-nargs)-1, UNLK, EXIT(nargs-1)]

-- Complie a function call
compileExp (App name seq) map vars =
    (sCode ++ [CALL $ offset name map], sLenght+2, sVars)
    where
        (sCode, sLenght, sVars) = compileSeq seq map vars

-- Compile a sequence of expression
compileSeq [] _ _ = ([], 0,0)
compileSeq (exp:exps) map vars =
    (hCode ++ tCode, hLength + tLength, max hVars tVars)
    where
        (hCode, hLength, hVars) = compileExp exp map vars
        (tCode, tLength, tVars) = compileSeq exps map vars

-- Complie a program
compileProg [exp] map _ =
    (LINK eVars:eCode,0,0)
    where
        (eCode, _ , eVars) = compileExp exp map 0

-- The program being compiled starts w/ a definition of a function.
-- map contains the address of all the functions and all the variables defined in the program to compile
compileProg (def@(Def func _ _):prog) map addr =
    (dCode ++ pCode, dLength+pLength, 0)
    where
        map2 = (func, addr):map
        (dCode, dLength, _) = compileExp def map2 0
        (pCode, pLength, pVars) = compileProg prog map2 (addr+dLength)

-- Compile a complete program, the code of function definitiona are after the jump
-- The numbre of jump to do depends the length of all the definition
-- prog is a table of function
compile prog =
    (JMP addr):pCode ++ [DOT,HALT]
    where
        (pCode, addr, _) = compileProg prog [] 2


-- compile a expression
-- compile xExp =
--     (LINK nExp) :cExp ++ [DOT,HALT]
--     where
--         (cExp, _, nExp) = compileExp xExp [] 0

list [] = return ()
list (i:is) =
    do {
        putStrLn $ show i ;
        list is
        }
