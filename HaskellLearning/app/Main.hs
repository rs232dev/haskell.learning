module Main where

import Lib
import Utils.Myutils
import Utils.Start
import GH_course
        
    {-- 
    
     git clone https://github.com/rs232dev/haskell.learning.git
    git remote add origin https://github.com/rs232dev/haskell.learning.git
    ...
    git add --all
    git commit -m "clean"
    git push --all

    git push origin master
    
    stack new MyProject
    cd MyProject
    stack setup    
    
    -- commons
    
    stack build intero
    cabal update
    cabal install hlint

    export PATH=/home/corrado/.cabal/bin/:/home/corrado/.local/bin:......


    
    --}

main :: IO()      
main = do
{
    putStrLn ("haskell main "++  show(square 3));
    putStrLn "haskell main";

    putStrLn ("gh sum list of integers:" ++  show (fsum  [1..5]) );
    putStrLn ("gh sum list of integers:" ++  show (fsum' [1..5]) );
    putStrLn ("gh sum list of floats  :" ++  show (fsum [1.0,2.1,3,4.7]) );

    putStrLn ("gh sort a list     :" ++  show (fqsort [3,5,1,4,2]) );
    putStrLn ("gh sort a list rev :" ++  show (reverse(fqsort [3,5,1,4,2]) ));

    putStrLn ("gh head of a list  :" ++  show (head [3,5,1,4,2]) );
    putStrLn ("gh tail of a list  :" ++  show (tail [3,5,1,4,2]) );
    putStrLn ("gh nth  of a list  :" ++  show ([3,5,1,4,2] !! 2 ) );
    
    putStrLn ("gh select the first n elements of a list  :" ++  show ( take 3 [3,5,1,4,2] ) );
    putStrLn ("gh remove the firts n elements of a list  :" ++  show ( drop 3 [3,5,1,4,2] ) );
    
    putStrLn ("gh sum elements of a list  :"     ++  show ( sum [3,5,1,4,2] ) );
    putStrLn ("gh product elements of a list  :" ++  show ( product [3,5,1,4,2] ) );

    putStrLn ("gh length of a list  :" ++  show ( length [3,5,1,4,2] ) );
    putStrLn ("gh append two lists  :" ++  show ( [1,2,3] ++ [4,5] ) );

    putStrLn ("gh nth  of a infinite list  :" ++  show (take 5 [1..]) );

    -- Tuples
    putStrLn ("add 2 tuple element :" ++  show (tuples_add (4,3)) );

    -- curried function
    putStrLn ("add 2 tuple element :" ++  show ( (add' 3)5) );

        
    -- defining function
    putStrLn ("abs :" ++  show ( GH_course.abs (-4)));


    -- lambda function
    putStrLn ("add'' :" ++  show ( GH_course.add'' 2 5));
    putStrLn ("odd'' :" ++  show ( GH_course.odd  10));

            {--
            putStrLn "haskell main";
            putStrLn ("square 5:" ++ show(square 5));
            putStrLn ("test 2:" ++ show(test2));
            putStrLn ("func_with_prefix_operator (+) 5 4   = " ++ show(func_with_prefix_operator 5 4));
            putStrLn ("func_with_infix_operator      5 + 4 = " ++ show(func_with_infix_operator 5 4));
            putStrLn ("guards:" ++ show(fn_abs(-9)));
            putStrLn ("tuple (pair) 1st and 2nd element: " ++ show(fst(pair)) ++ " - " ++show(snd(pair)));
            putStrLn ("tuple        fourth      element: " ++ show(get4th(mytuple)));
        
            putStrLn ("polymorpchic fun :" ++ show(plength ll2));
        
            putStrLn ("myfoldr_sum     :" ++ show(myfoldr (+) 0 [1,2,3,4,5]));
            putStrLn ("myfoldr_product :" ++ show(myfoldr (*) 1 [1,2,3,4,5]));
        
            putStrLn ("tree:" ++ show(treedata));
        
            putStrLn ("mySum:" ++ show(mySum [1,2,3,4,5]));
            putStrLn ("compose:" ++ show(comp2 [2,4]));
        
        --}
      
}
    