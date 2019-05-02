module Cap_30.Monads where


-- ========================================================================== --
-- Motivation: Maybe                                                          --
-- ========================================================================== --    

data Person = Person String Int

father,mother :: Person -> Maybe Person 
father (Person name age) = Just (Person name age)
mother (Person name age) = Just (Person name age)


bothGrandfathers :: Person -> Maybe (Person, Person)
bothGrandfathers p =
    case father p of
        Nothing -> Nothing
        Just dad ->
            case father dad of
                Nothing -> Nothing
                Just gf1 ->                          -- found first grandfather
                    case mother p of
                        Nothing -> Nothing
                        Just mom ->
                            case father mom of
                                Nothing -> Nothing
                                Just gf2 ->          -- found second grandfather
                                    Just (gf1, gf2)



bothGrandfathers' p =
    father p >>=
        (\dad -> father dad >>=
            (\gf1 -> mother p >>=   -- gf1 is only used in the final return
                (\mom -> father mom >>=
                    (\gf2 -> return (gf1,gf2) ))))