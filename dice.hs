import Control.Monad.Trans.State
import System.Random

rollPair :: StdGen -> ((Int, Int), StdGen)
rollPair s0 =
 let (r1, s1) = randomR (1,6) s0
     (r2, s2) = randomR (1,6) s1
 in ((r1,r2), s2)


rollPairS :: State StdGen (Int, Int)
rollPairS = do
  r1 <- rollDieS
  r2 <- rollDieS
  return (r1, r2)

rollDieS :: State StdGen Int
rollDieS = state $ randomR (1,6)
