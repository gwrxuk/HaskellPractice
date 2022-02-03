import Data.Maybe 
fac 0 = 1
fac n = n * fac (n-1)

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs)
 | mod x 2 == 0 = x : evens xs
 | otherwise = evens xs


asc :: Int -> Int -> [Int]
asc n m 
 | m < n = []
 | m == n = [m]
 | m > n = n : asc (n+1) m

app :: (a->b) -> a ->b
app f x = f x
add1 :: Int -> Int
add1 x = x+1

data PeaNum = Succ PeaNum | Zero
incr :: PeaNum -> PeaNum
incr = Succ

data Person = Person { name :: String, age :: Int}

greet :: Person -> [Char]
greet (Person name _) = "Hi" ++ name

rev :: [a] -> [a]
rev = foldr (\x acc -> x : acc) []

prefixes :: [a] -> [[a]]
prefixes =
 foldr (\x acc -> [x] : (map ((:)x) acc ))[]

greet2 :: IO ()
greet2 = do
 putStrLn "What is your name?"
 name <- getLine
 putStrLn ("Hello" ++ name ++".")


lagrange :: [(Float, Float)] -> Float -> Float
lagrange xs x = foldl (\acc (xj, y) -> acc + (y * l xj)) 0 xs
 where
  l xj = foldl (
       \acc (xk, _)->
         if (xj == xk) then
           acc
         else
           acc * ((x-xk) / (xj -xk))
    ) 1 xs

data Trie a = Leaf a | Node a [Trie a]
foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf x ) = f acc x
foldtrie f acc (Node x xs) = foldl f' (f acc x) xs 
 where f' acc t = foldtrie f acc t

safediv :: Integral a => a -> a -> Maybe a
safediv a b =
 if b == 0 then Nothing else Just $ div a b

data Temperature = C Float | F Float
 deriving (Show, Eq)
isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:y:xs) = 
   (x <= y) && isAsc(y:xs)
main= do
       let f = lagrange [(1.2, 55.2),(1.2,5.5)]
       let t = f 5.0
       
       print(t)
       let person = Person{name = "a", age = 1}
       let a = greet person
       print (a) 
       greet2
       let b = safediv 10 2
       print(b)
