data DList a = DLNode (DList a) a (DList a)
 
rot              :: Integer -> [a] -> [a]
rot n xs | n < 0  = rot (n+1) ((last xs):(init xs))
         | n == 0 = xs
         | n > 0  = rot (n-1) (tail xs ++ [head xs])
 
mkDList   :: [a] -> DList a
mkDList [] = error "Must have at least one element."
mkDList xs = DLNode (mkDList $ rot (-1) xs) (head xs) (mkDList $ rot 1 xs)
