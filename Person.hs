
data Person = Mother | Father
father :: Person -> Maybe Person
father p  = Just Father

mother :: Person -> Maybe Person
mother p =  Just Mother

maternalGrandfather :: Person -> Maybe Person
maternalGrandfather p = mother p >>=father

bothGrandfathers p = 
 father p >>=
  (\dad -> father dad >>=
    (\gf1 -> mother p >>=
      (\mom -> father mom >>=
        (\gf2 -> return (gf1, gf2) ))))


main=putStrLn "Yes"
