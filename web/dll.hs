data DList a = DLNode (DList a) a (DList a)
 
mkDList :: [a] -> DList a
 
mkDList [] = error "must have at least one element"
mkDList xs = let (first,last) = go last xs first
             in  first
 
  where go :: DList a -> [a] -> DList a -> (DList a, DList a)
        go prev []     next = (next,prev)
        go prev (x:xs) next = let this        = DLNode prev x rest
                                  (rest,last) = go this xs next
                              in  (this,last)
 
takeF :: Integer -> DList a -> [a]
takeF 0     _                 = []
takeF n (DLNode _ x next) = x : (takeF (n-1) next)
 
takeR :: Show a => Integer -> DList a -> [a]
takeR 0     _                 = []
takeR n (DLNode prev x _) = x : (takeR (n-1) prev)
