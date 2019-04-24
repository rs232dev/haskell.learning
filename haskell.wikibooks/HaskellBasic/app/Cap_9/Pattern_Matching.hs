module Cap_9.Pattern_Matching where


-- ========================================================================== --
-- Pattern Matching                                                           --
-- ========================================================================== --
{--
    Consider a program which tracks statistics from a racing competition in 
    which racers receive points based on their classification in each race, 
    the scoring rules being:

    * 10 points for the winner;
    * 6 for second-placed;
    * 4 for third-placed;
    * 3 for fourth-placed;
    * 2 for fifth-placed;
    * 1 for sixth-placed;        
    * no points for other racers.

We can write a simple function which takes a classification (represented by 
an integer number: 1 for first place, etc. 3 ) and returns how many points 
were earned. One possible solution uses if/then/else:
--}

pts,pts',pts'' :: Int -> Int
pts x =
    if x == 1
        then 10
        else if x == 2
            then 6
            else if x == 3
                then 4
                else if x == 4
                    then 3
                    else if x == 5
                        then 2
                        else if x == 6
                            then 1
                            else 0

-- We can do better!

{-- 
  This feature of Haskell is called pattern matching. When we call pts, 
  the argument is matched against the numbers on the left side of each of the 
  equations, which in turn are the patterns. 
  The matching is done in the order we wrote the equations. First, the 
  argument is matched against the 1 in the first equation. If the argument 
  is indeed 1, we have a match and the first equation is used; so pts 1 
  evaluates to 10 as expected. Otherwise, the other equations are tried in 
  order following the same procedure. The final one, though, is rather 
  different: the _ is a special pattern, often called a "wildcard", that 
  might be read as "whatever": it matches with anything; and therefore if the 
  argument doesn't match any of the previous patterns pts will return zero.
--}

pts' 1 = 10
pts' 2 = 6
pts' 3 = 4
pts' 4 = 3
pts' 5 = 2
pts' 6 = 1
pts' _ = 0  -- the _ is a special pattern, often called a "wildcard", that 
            -- might be read as "whatever": 
            -- it matches with anything; and therefore if the argument 
            -- doesn't match any of the previous patterns pts will return zero.


-- However, we could use a variable to make pts even more concise.
pts'' 1 = 10
pts'' 2 = 6
pts'' x
    | x <= 6 = 7 - x
    | otherwise = 0


-- ========================================================================== --
-- Tuples and Lists Pattern                                                   --
-- ========================================================================== --    

-- Main> :t fst
-- fst :: (a, b) -> a

list_length:: [a] -> Int
list_length []  = 0
list_length (x:xs) = 1 + (list_length xs)

-- ========================================================================== --
-- let bindings                                                               --
-- ========================================================================== --    
roots a b c =
    let sdisc = sqrt (b * b - 4 * a * c)
    in ((-b + sdisc) / (2 * a),
    (-b - sdisc) / (2 * a))