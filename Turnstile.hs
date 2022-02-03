
data Person = Mother | Father
father :: Person -> Maybe Person
father p  = Just Father

mother :: Person -> Maybe Person
mother p =  Just Mother

maternalGrandfather :: Person -> Maybe Person
maternalGrandfather p =
    case mother p of
        Nothing -> Nothing
        Just mom -> father mom



bothGrandfathers :: Person -> Maybe (Person, Person)
bothGrandfathers p =
 case father p of
  Nothing -> Nothing
  Just dad ->
   case father p of 
    Nothing -> Nothing
    Just gf1 ->
     case mother p of
      Nothing -> Nothing
      Just mom ->
       case father mom of 
        Nothing -> Nothing
        Just gf2 ->
         Just (gf1, gf2)

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin _ = (Thank, Unlocked)

push Locked   = (Tut , Locked)
push Unlocked = (Open, Locked)


monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
      (a3, s3) = push s2
      (a4, s4) = coin s3
      (a5, s5) = push s4
  in ([a1, a2, a3, a4, a5], s5)

newtype State s a = State { runState :: s -> (a, s) }

main=putStrLn "Yes"
