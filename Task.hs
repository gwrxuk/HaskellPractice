import Data.Char

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

readUntil :: String -> IO ()
readUntil ending = do
 input <- getLine
 if input == ending then return ()
 else readUntil ending
    

readUntilWithCount :: String -> IO Int
readUntilWithCount ending = aux ending 0
 where aux ending count = do
                        input <- getLine
                        let ncount = count + 1
                        if input == ending then return  ncount
                        else aux ending ncount 

type IOMaybe a = IO (Maybe a)
returnIOM :: a -> IOMaybe a
returnIOM = return . Just

bindIOM :: IOMaybe a -> (a -> IOMaybe b) -> IOMaybe b
bindIOM iom f = do
 maybe_val <- iom
 case maybe_val of
   Nothing -> return Nothing
   (Just v) -> f v

(>>>=) = bindIOM

liftIOM :: IO a -> IOMaybe a
liftIOM io = io >>= returnIOM

checkInput :: String -> Bool
checkInput [] = False
checkInput (x:_) = isUpper x

getName :: IOMaybe String
getName = do
 input <- getLine
 if checkInput input
 then returnIOM input
 else return Nothing

example = putStr "Please enter your name: "
          >> getName
          >>>=(\s -> liftIOM $ putStrLn $ "Yourname is "++s)

