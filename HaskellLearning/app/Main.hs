module Main where

import Lib
import Utils.Myutils
import Utils.Start
        
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
    