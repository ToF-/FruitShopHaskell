module FruitShop

where


process :: [String] -> [String]
process ss = map show $ total 0 ss 

total :: Int -> [String] -> [Int]
total n [] = []
total n (s:ss) = t : total t ss 
    where 
    t = n + price s
    price s = case lookup s fruits of
        Just p -> p
        Nothing -> error $ s ++ " ???"
    fruits = [("Pommes",100),("Cerises",75)]
