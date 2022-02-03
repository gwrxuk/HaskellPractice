maybeadd :: Num b => Maybe b -> b -> Maybe b
maybeadd mx y = mx >>= (\x -> Just $ x+y)


maybeadd2 :: Num b => Maybe b -> Maybe b -> Maybe b
maybeadd2 mx my = 
 mx >>= (\x -> my >>= (\y -> Just $ x+y))


maybeadd3 :: (Monad m, Num b) => m b -> m b -> m b
maybeadd3 mx my =
 mx >>= (\x -> my >>= (\y -> return $ x+y))

rev xs = rev_aux [] xs
 where 
  rev_aux acc [] = acc
  rev_aux acc (x:xs) = rev_aux (x:acc) xs


data Tree a = Leaf |  Node (Tree a) a (Tree a)
cut :: Integer -> Tree a -> Tree a
cut 0 _ = Leaf



insert :: (Ord a)=> a -> Tree a -> Tree a
insert v Leaf = Node Leaf v Leaf
insert v (Node l vt r)
 | v <= vt = Node (insert v l) vt r
 | v > vt = Node l vt (insert v r)


inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node l v r ) =
 (inorder l) ++ [v] ++ (inorder r)
inv_tup_tree :: Tree (Integer, Integer)
inv_tup_tree = aux (0,0)
 where 
  aux (l,r) = Node (aux $ (l+1, r)) (l,r) (aux $ (l,r+1))


