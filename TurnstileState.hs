import Control.Monad.Trans.State 
data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin _ = (Thank, Unlocked)

push Locked   = (Tut , Locked)
push Unlocked = (Open, Locked)


coinS, pushS :: State TurnstileState TurnstileOutput
coinS = state coin
pushS = do
 s <- get 
 put Locked
 case s of 
  Locked -> return Tut
  Unlocked -> return Open 

--put :: s -> State s ()
--put newState  = state $ \_ -> ((),newState)

--get :: State s s
--get = state $ \s -> (s,s)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
      (a3, s3) = push s2
      (a4, s4) = coin s3
      (a5, s5) = push s4
  in ([a1, a2, a3, a4, a5], s5)

mondayS :: State TurnstileState [TurnstileOutput]
mondayS = do
  a1 <- coinS
  a2 <- pushS
  a3 <- pushS
  a4 <- coinS
  a5 <- pushS
  return [a1, a2, a3, a4, a5]

main=do
 putStrLn "Yes"
