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

        findPrice s = case lookup (localization s) fruits of
            Just p -> p
            Nothing -> error $ s ++ " ???"

        reduction :: Product -> Money
        reduction s = case lookup s specials of 
            Just (r,m) -> ((applyReduction r) . (==0) . (`mod` m) . length . filter ((==) s)) (s:ps)
            Nothing -> 0

        applyReduction :: Int -> Bool -> Int
        applyReduction r True = r
        applyReduction _ _    = 0

        localization :: Product -> Product
        localization s = case lookup s local of
            Just p -> p
            Nothing -> s

        fruits = [("Pommes",100),("Bananes",150),("Cerises",75)]

        specials = [("Cerises", (20,2)), ("Bananes", (150,2)),("Mele",(50,2)),("Apples",(100,3))]

        local = [("Apples","Pommes"),("Mele","Pommes")]
