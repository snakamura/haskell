module Writer where

import Data.Bifunctor
import Control.Comonad

newtype Writer w a = Writer (w, a)

instance Functor (Writer w) where
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap a2b (Writer (w, a)) = Writer (w, a2b a)

instance Monoid w => Applicative (Writer w) where
    pure :: a -> Writer w a
    pure a = Writer (mempty, a)

    (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    Writer (wa2b, a2b) <*> Writer (wa, a) = Writer (wa <> wa2b, a2b a)

instance Monoid w => Monad (Writer w) where
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    Writer (wa, a) >>= a2wb = let Writer (wb, b) = a2wb a in Writer (wa <> wb, b)

withWriter :: (String, Int)
withWriter = let w1, w2, w3 :: Int -> Writer String Int
                 w1 n = Writer ("1st\n", n + 1)
                 w2 n = Writer ("2nd\n", n * 10)
                 w3 = pure
                 Writer (w, a) = return 100 >>= w1 >>= w2 >>= w3
              in (w, a)


newtype Traced w a = Traced (w -> a)

instance Functor (Traced w) where
    fmap :: (a -> b) -> Traced w a -> Traced w b
    fmap a2b (Traced w2a) = Traced (a2b . w2a)

instance Monoid w => Comonad (Traced w) where
    extract :: (Traced w a) -> a
    extract (Traced w2a) = w2a mempty

    extend :: (Traced w a -> b) -> Traced w a -> Traced w b
    extend ta2b (Traced w2a) = Traced $ \wb -> ta2b (Traced $ \wa -> w2a (wa <> wb))

withTraced :: (String, Int)
withTraced = let t1, t2, t3 :: Traced String (String, Int) -> (String, Int)
                 t1 (Traced w2a) = second (+ 1) $ w2a "1st\n"
                 t2 (Traced w2a) = second (* 10) $ w2a "2nd\n"
                 t3 = extract
                 Traced w2a' = Traced (, 100) =>> t1 =>> t2 =>> t3
              in w2a' ""
