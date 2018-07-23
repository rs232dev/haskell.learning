module GH_course where 

    
-- -------------------------------------------------------------------------- --
-- pag. 9 "a taste of haskell" 
-- *Main> :t fsum
-- fsum :: Num a => [a] -> a
-- -------------------------------------------------------------------------- --
fsum :: (Num a) =>  [a] -> a
fsum [] = 0
fsum (x:xs) = x + fsum xs

fsum':: (Num a) =>  [a] -> a
fsum' = foldr (+) 0

-- -------------------------------------------------------------------------- --
-- pag. 10 "a taste of haskell" 
-- *Main> :t fqsort
-- fqsort :: Ord a => [a] -> [a]
--
-- how it works:
--
-- fqsort[3,5,1,4,2]
-- 
-- fqsort [1,2] ++ [3] ++ fqsort [5,4]
--         \        \             \
--          \        \_ [x]        \_ larger
--           \
--            \_ smaller
--
--
-- processing fqsort smaller: fqsort [1,2] ++ ...
--
-- fqsort [] ++ [1] ++ [2] 
--         \     \      \_ larger
--          \     \
--           \     \_ [x]
--            \
--             \_ smaller
--        
--  next step fqsort smaller
--  [] = [] ..so end recursive call then the result is: [1,2]
--   
-- processing fqsort larger: ... ++ fqsort [5,4]
--
-- fqsort [4] ++ [5] ++ [] 
--         \      \      \_ larger
--          \      \
--           \      \_ [x]
--            \
--             \_ smaller
--        
--  next step fqsort larger
--  [] = [] ..so end recursive call then the result is: [4,5]
--
-- so the final result is: [1,2,3,4,5]
-- -------------------------------------------------------------------------- --
fqsort :: Ord a => [a] -> [a]
fqsort []      = []
fqsort (x:xs)  = fqsort smaller ++ [x] ++ fqsort larger
    where
        smaller = [a | a <- xs,a<=x]
        larger  = [a | a <- xs,a>x]





