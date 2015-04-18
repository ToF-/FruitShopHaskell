module FruitShop

where


process :: [String] -> [String]
process ss = map show $ total  ss 

total :: [String] -> [Int]
total = tail . scanl addProduct 0
    where
    addProduct :: Int -> String -> Int 
    addProduct t = (+t) . price
    price s = case lookup s fruits of
        Just p -> p
        Nothing -> error $ s ++ " ???"
    fruits = [("Pommes",100),("Cerises",75)]
