module FruitShop

where


process :: [String] -> [Int]
process ss = total  ss 

total :: [String] -> [Int]
total =  map fst . tail . scanl addProduct (0,[]) 
    where
    addProduct :: (Int,[String]) -> String -> (Int,[String]) 
    addProduct (t,ps) s = (t + findPrice s - reduction s ps, s:ps) 
    findPrice s = case lookup s fruits of
        Just p -> p
        Nothing -> error $ s ++ " ???"
    reduction :: String -> [String] -> Int
    reduction "Cerises" ps  = ((*20) . (`mod`2) . length . filter ((==)"Cerises")) ps
    reduction _ _ = 0 

    fruits = [("Pommes",100),("Bananes",150),("Cerises",75)]
