import System.IO
import Data.Char

lower :: String -> String
lower = map toLower

findStrings :: [String] -> String -> [String]
findStrings sws text = [w | w<-sws, (lower w) `elem` txtwords]
 where
  ftext = filter (\x -> isLetter x || isSpace x) text
  txtwords = map lower $ words ftext

getSearchWords :: IO [String]
getSearchWords = do
 putStrLn "Specify the words to search: "
 aux
 where
  aux = do
   putStr "> "
   line <- getLine
   if line == "" then
    return []
   else do
    putStrLn aux
    xs <- aux
    return $ line:xs

main :: IO()
main = do
 hSetBuffering stdout NoBuffering
 sws <- getSearchWords
 putStr "File to search: "
 path <- getLine
 text <- readFile path
 let found = findStrings sws text
 let nfound = [w | w <-sws, not $ w`elem` found]
 mapM_ (\s -> putStrLn $ "\"" ++ s ++ "\" found ") found
 mapM_ (\s -> putStrLn $ "\"" ++ s ++ "\" Not found") nfound
