module Cap_32.ListMonad where


-- ========================================================================== --
-- List Monad                                                                 -- 
-- ========================================================================== --

-- Main> ( [1,2,3] >>= (\x -> [[x+1]]) ) >>= ((\[x] -> [[x+1]]))
-- [[3],[4],[5]]

-- Main> concat(( [1,2,3] >>= (\x -> [[x+1]]) ) >>= ((\[x] -> [[x+1]])))
-- [3,4,5]


