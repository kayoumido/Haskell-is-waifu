compile (Let "x" (Cst 2) (Let "y" (Cst 3) (Bin '+' (Var "x") (Var "y"))))

def Fact n = if n then n * Fact(n-1) else 1

def Fact n = if n then n * Fact(n-1) else 1
Def "Fact" ["n"] (If (Var "n") (Bin "*" (Var "n") (Func "Fact" [Bin "-" (Var "n") (Cst 1)])) (Cst 1))
Func "Fact" [Cst 4]

 compile [(Let "y" (Cst 4) (Let "x" (Cst 3) (Bin '*' (Var "x") (Var "y")))),(Let "x" (Cst 2) (Let "y" (Cst 3) (Bin '+' (Var "x") (Var "y"))))]

 compile [Def "Fact" ["n"] (If (Var "n") (Bin '*' (Var "n") (Func "Fact" [Bin '-' (Var "n") (Cst 1)])) (Cst 1)), Func "Fact" [Cst 4]]

 Def "Add" ["n","m"] (Bin "+" (Var "n") (Var "m"))

 compile [Def "Add" ["n","m"] (Bin '+' (Var "n") (Var "m")), Func "Add" [Cst 4, Cst 5]]