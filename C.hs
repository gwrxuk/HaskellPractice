import Data.Either

data Either a b = Lef a | Right b
data Maybe a = Nothing | Just a

data MyError = EmptyLine

mightFail :: ExceptT MyError IO ()
mightFail = do
 l <- lift getLine
 when (null l) (throwError EmptyLine) 
