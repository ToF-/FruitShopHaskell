module FruitShop

where


process :: [String] -> [Int]
process ss = total  ss 

total :: [String] -> [Int]
total =  map fst . tail . scanl addProduct (0,[]) 
    where
    addProduct :: (Int,[String]) -> String -> (Int,[String]) 
    addProduct (t,ps) s = (t + findPrice s - reduction s , s:ps) 
        where

        findPrice s = case lookup s fruits of
            Just p -> p
            Nothing -> error $ s ++ " ???"

        reduction :: String -> Int
        reduction s = case lookup s specials of 
            Just r -> ((*r) . (`mod`2) . length . filter ((==) s)) ps
            Nothing -> 0

        fruits = [("Pommes",100),("Bananes",150),("Cerises",75)]

        specials = [("Cerises", 30), ("Bananes", 150)]
