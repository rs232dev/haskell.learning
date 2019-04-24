module Cap_16.Syntax_Tricks where

-- ========================================================================== --
-- As-patterns                                                                --
--                                                                            --
-- Sometimes, when matching a pattern with a value, it may be useful to bind  --
-- a name to the whole value being matched.                                   --
-- ========================================================================== --

-- As-patterns allow exactly this: they are of the form var@pattern and have 
-- the additional effect to bind the name var to the whole value being     
-- matched by
-- pattern. For instance, here is a toy variation on the map theme:

contrivedMap :: ([a] -> a -> b) -> [a] -> [b]
contrivedMap f [] = []
contrivedMap f list@(x:xs) = f list x : contrivedMap f xs




        