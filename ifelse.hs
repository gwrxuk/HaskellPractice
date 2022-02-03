findInt :: [Int] -> Maybe Int
findInt [] = Nothing
findInt (x:xs) = if x == 1 then Just x else findInt xs
