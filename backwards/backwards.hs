import Control.Applicative
import Control.Applicative.Backwards
import Data.Foldable

main :: IO ()
main = do
    putStrLn "a" *> putStrLn "b"
    forwards $ Backwards (putStrLn "a") *> Backwards (putStrLn "b")
    forwards $ Backwards (pure (const (const ()))) <*> Backwards (putStrLn "a") <*> Backwards (putStrLn "b")
    forwards $ Backwards (putStrLn "a" <**> pure (const (const ()))) <*> Backwards (putStrLn "b")
    forwards $ Backwards (putStrLn "b" <**> (putStrLn "a" <**> pure (const (const ()))))
    forwards $ Backwards (putStrLn "b" <**> liftA2 (\a f -> f a) (putStrLn "a") (pure (const (const ()))))
    forwards $ Backwards (liftA2 (\a f -> f a) (putStrLn "b") (liftA2 (\a f -> f a) (putStrLn "a") (pure (const (const ())))))

    traverse_ print [1 .. 10 :: Int]
    forwards $ traverse_ (Backwards . print) [1 .. 10 :: Int]
