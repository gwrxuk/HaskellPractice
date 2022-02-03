import Data.Maybe
import System.Exit
import System.Environment

foldl' :: (a->b->a)->a->[b]->a
foldl' _ z [] = z
foldl' f z (x:xs) =
 let z' = f z x in
 z' `seq` foldl' f z' xs
printHelp = do
 progName <- getProgName
 putStrLn $ "Usage: "++ progName ++ "[-h | --help | -v || version] <greeting>"
printVersion = putStrLn "v1"

mainAct [] = do
 putStrLn "Needs a greeting!"
 printHelp
 exitFailure

mainAct args = do
 let greeting = unwords args
 name <- lookupEnv "USER"
 putStrLn $ maybe "No user to greet!" (\name -> greeting ++ " " ++ name) name

main = do
 args <- getArgs
 if "-h" `elem` args || "--help" `elem` args then
  printHelp >> exitSuccess
  else if "-v" `elem` args || "--version" `elem` args then
   printVersion >> exitSuccess
    else
     mainAct args
