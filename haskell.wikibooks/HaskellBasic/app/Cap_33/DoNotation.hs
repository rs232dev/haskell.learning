module Cap_33.DoNotation where
    
-- (>>) (then) operator works almost identically in do notation
io_then_operator =
        putStr "Hello"  >>
        putStr " "      >>
        putStr "world!" >>
        putStr "\n"

    
main2 :: IO ()
main2 = do 
        putStr "hello"
        putStr " "    
        putStr "world"
        putStr "\n"




        

nameDo :: IO ()
nameDo = do putStr "What is your first name? "
            first <- getLine
            putStr "And your last name? "
            last <- getLine
            let full = first ++ " " ++ last 
            putStrLn ("Pleased to meet you, " ++ full ++ "!")


            