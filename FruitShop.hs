module FruitShop

where

type Bill    = (Money, [Product])
type Money   = Int
type Product = String

process :: [Product] -> [Int]
process ss = total  ss 

total :: [Product] -> [Money]
total =  map fst . tail . scanl addProduct (0,[]) 
    where
    addProduct :: Bill -> String -> Bill 
    addProduct (t,ps) s = (t + findPrice s - reduction s , s:ps) 
        where

        findPrice s = case lookup s fruits of
            Just p -> p
            Nothing -> error $ s ++ " ???"

        reduction :: Product -> Money
        reduction s = case lookup s specials of 
            Just r -> ((*r) . (`mod`2) . length . filter ((==) s)) ps
            Nothing -> 0

        fruits = [("Pommes",100),("Bananes",150),("Cerises",75)]

        specials = [("Cerises", 30), ("Bananes", 150)]
