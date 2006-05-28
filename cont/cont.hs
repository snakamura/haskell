mul1 :: Num a => [a] -> a
mul1 = foldr (*) 1

mul2 :: Num a => [a] -> a
mul2 []     = 1
mul2 (x:xs) = x * mul2 xs

mul3 :: Num a => [a] -> a
mul3 []     = 1
mul3 (0:xs) = 0
mul3 (x:xs) = x * mul3 xs

mul4 :: Num a => [a] -> a
mul4 = mulCps id
 where
     mulCps :: Num a => (a -> b) -> [a] -> b
     mulCps f []     = f 1
     mulCps f (x:xs) = mulCps (\r -> f (x * r)) xs

mul5 :: Num a => [a] -> a
mul5 = mulCps id
 where
     mulCps :: Num a => (a -> b) -> [a] -> b
     mulCps f []     = f 1
     mulCps f (0:xs) = f 0
     mulCps f (x:xs) = mulCps (\r -> f (x * r)) xs

mul6 :: Num a => [a] -> a
mul6 = mulCps id
 where
     mulCps :: Num a => (a -> b) -> [a] -> b
     mulCps f = mulCps' f
      where
          mulCps' g []     = g 1
          mulCps' _ (0:xs) = f 0
          mulCps' g (x:xs) = mulCps' (\r -> g (x * r)) xs

mul7 :: Num a => [a] -> a
mul7 x = runCont (mulCont x) id
 where
     mulCont :: Num a => [a] -> Cont a a
     mulCont []     = return 1
     mulCont (x:xs) = do r <- mulCont xs
                         return $ x * r

mul8 :: Num a => [a] -> a
mul8 x = runCont (mulCont x) id
 where
     mulCont :: Num a => [a] -> Cont a a
     mulCont []     = return 1
     mulCont (0:xs) = return 0
     mulCont (x:xs) = do r <- mulCont xs
                         return $ x * r

mul9 :: Num a => [a] -> a
mul9 x = runCont (callCC $ mulCont x) id
 where
     mulCont :: Num a => [a] -> (a -> Cont a a) -> Cont a a
     mulCont x c = mulCont' x
      where
          mulCont' []     = return 1
          mulCont' (0:xs) = c 0
          mulCont' (x:xs) = do r <- mulCont' xs
                               return $ x * r


data Tree a = Branch a (Tree a) (Tree a)
            | Node a

mt1 :: Num a => Tree a -> a
mt1 (Branch n l r) = n * (mt1 l) * (mt1 r)
mt1 (Node n)       = n

mt2 :: Num a => Tree a -> a
mt2 = mtCps id
 where
     mtCps :: Num a => (a -> b) -> Tree a -> b
     mtCps f (Branch n l r) = mtCps (\x ->
                              mtCps (\y ->
                              f (n * x * y)) r) l
     mtCps f (Node n)       = f n

mt3 :: Num a => Tree a -> a
mt3 t = runCont (mtCont t) id
 where
     mtCont :: Num a => Tree a -> Cont a a
     mtCont (Branch n l r) = do x <- mtCont l
                                y <- mtCont r
                                return $ n * x * y
     mtCont (Node n)       = return n


newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Monad (Cont r) where
    return x = Cont $ \f -> f x
    Cont c >>= f = Cont $ \g -> c (\x -> runCont (f x) g)

class Monad m => MonadCont m where
    callCC :: ((a -> m b) -> m a) -> m a

instance MonadCont (Cont r) where
    callCC f = Cont $ \g -> runCont (f (\x -> Cont $ \_ -> g x)) g


mul4' :: Num a => [a] -> a
mul4' xs = mulCps xs id
 where
     mulCps :: Num a => [a] -> (a -> b) -> b
--     mulCps []     = \f -> f 1
     mulCps []     = r 1
--     mulCps (x:xs) = \f -> mulCps xs (\y -> f (x * y))
--     mulCps (x:xs) = mulCps xs |>>= (\y -> \f -> f $ x * y)
     mulCps (x:xs) = mulCps xs |>>= (\y -> r $ x * y)

type C r a = (a -> r) -> r

r :: a -> C r a
r x = \f -> f x

(|>>=) :: C r a -> (a -> C r b) -> C r b
cont |>>= f = \k -> cont (\x -> f x k)

callcc :: ((a -> C r b) -> C r a) -> C r a
callcc f = \g -> (f (\x -> \_ -> g x)) g
