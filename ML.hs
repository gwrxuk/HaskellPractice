{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe

data Code =  Code {var :: Maybe Char, value ::  Int}
 deriving (Show)


data ByteCode = LOAD_VAL Int | WRITE_VAR Char | READ_VAR Char | ADD  | MULTIPLY | RETURN_VALUE

type Stack = [Code]
type Output = [Int]
type Program = [ByteCode]

type VM a = ReaderT Program (WriterT Output (State Stack)) a

newtype Comp a = Comp {unComp :: VM a}
 deriving (Functor, Applicative, Monad, MonadReader Program, MonadWriter Output, MonadState Stack)

runByteCode :: ByteCode -> Comp ()
runByteCode op = case op of
 LOAD_VAL n -> modify (loadVal(n):)
 READ_VAR c -> do
  code <- state (\st -> do
    case readVar st c of
     Just a -> (a,st)
     )
  modify(code:)
 WRITE_VAR c -> do
  code <- gets head
  modify (writeVar code c:)
 ADD -> do 
  a <- gets head
  b <- gets head
  modify((add a b):)
 MULTIPLY -> do
  a <- gets head
  b <- gets head
  modify((multiply a b):)
 RETURN_VALUE -> do
  code <- gets head
  tell [ readValue(code) ]

multiply :: Code -> Code -> Code
multiply a b = Code {var = Nothing, value = readValue(a) * readValue(b)}

add :: Code -> Code -> Code
add a b = Code {var=Nothing, value = readValue(a) + readValue(b)}

loadVal :: Int -> Code
loadVal n = Code {var=Nothing, value=n} 

readValue :: Code -> Int
readValue (Code {var = name, value = v}) = v

writeVar :: Code -> Char  -> Code
writeVar code name =  Code {var = Just name, value =  (readValue code) }

getVar :: Code -> Maybe Char
getVar (Code {var = name, value = v}) = name

readVar :: [Code] -> Char -> Maybe Code
readVar [] _ = Nothing
readVar (x:xs) c =  if (matchVar x c) then Just  x else readVar xs c

matchVar :: Code -> Char -> Bool
matchVar code c = if (getVar code) == Nothing then False else fromJust (getVar code) == c

runProgram :: Comp ()
runProgram = do
  program <- ask
  case program of
   [] -> return ()
   (x:xs) -> runByteCode x >> local (const xs) runProgram

execVM :: Program -> Output
execVM = flip evalState [] . execWriterT . runReaderT (unComp runProgram) 

program :: Program
program = [
 LOAD_VAL 1,
 WRITE_VAR 'x',
 LOAD_VAL 2,
 WRITE_VAR 'y',
 READ_VAR 'x',
 LOAD_VAL 1,
 ADD,
 READ_VAR 'y',
 MULTIPLY,
 RETURN_VALUE
 ]

main :: IO ()
main = mapM_ print $execVM program
