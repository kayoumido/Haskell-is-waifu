-- Q5
valeurs _ _ Vide = 0
valeurs min max (Noed t lhs rhs) =
    | t < min = valeurs min max rhs
    | t > max = valeurs min max lhs
    | otherwise = 1 + (valeurs min max lhs) + (valeurs min max rhs)