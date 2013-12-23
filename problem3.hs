elementAtK :: [a]->Int->a
elementAtK (x:xs) 1 = x
elementAtK (x:xs) k = elementAtK xs (k-1)