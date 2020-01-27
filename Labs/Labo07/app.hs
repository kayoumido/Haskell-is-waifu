import Data.Char
main = 
  do 
    text <- getLine
    if null text
      then return ()
    else do
      putStrLn $ transform text
      main

verityTable = let l = [False, True] in 
    [[x,y,z, x&&y || z] | x<-l, y<-l, z<-l]

transform s = map (toUpper) s

showline l@(x:xs)
  | l == []   = show ""
  | otherwise = do 
    show x
    showline xs 
-- showLine l@(x:xs) = do
--     if l == [] then 
--         return ()
--     else 
--         do
--             putStrLn $ show
--             showLine xs


-- showTruth table@(x:xs) = 
--     do 
--         if table == []
--             then return ()
--         else
--             do
--                 showLine