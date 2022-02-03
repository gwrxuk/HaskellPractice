import Control.Monad.State

type MyState = Int
valFromState :: MyState -> Int
valFromState s = -s
nextState :: MyState -> MyState
nextState x = 1+x

type MyStateMonad = State MyState

getNext :: MyStateMonad Int
getNext = state (\st -> let st' = nextState(st) in (valFromState(st'),st'))

inc3:: MyStateMonad Int
inc3 = getNext >>= \x ->
       getNext >>= \y ->
       getNext >>= \z ->
       return z

inc3Sugared :: MyStateMonad Int
inc3Sugared = do x <- getNext
                 y <- getNext
                 z <- getNext
                 return z

inc3AlternateResult::MyStateMonad Int
inc3AlternateResult = do  getNext
                          getNext
                          getNext
                          s<-get
                          return (s*s)

inc3DiscardedValuesSugared::MyStateMonad Int
inc3DiscardedValuesSugared =    do 
                            getNext
                            getNext
                            getNext









