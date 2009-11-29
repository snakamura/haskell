module Main where

import Control.Applicative
import Control.Arrow (first)
import Prelude hiding (seq)


newtype Parser input output = Parser { runParser :: input -> Maybe (output, input) }

instance Functor (Parser input) where
  fmap f parser = Parser $ \i -> first f <$> runParser parser i

instance Applicative (Parser input) where
  pure v = Parser $ \i -> pure (v, i)
  f <*> v = Parser $ \i -> do (g, r) <- runParser f i 
                              (o, r') <- runParser v r 
                              return (g o, r')

instance Alternative (Parser input) where
  empty = Parser $ const empty
  l <|> r = Parser $ \i -> runParser l i <|> runParser r i


token :: Eq a => a -> Parser [a] a
token c = tokenOf (== c)

tokenOf :: (a -> Bool) -> Parser [a] a
tokenOf p = Parser tokenOf'
  where
    tokenOf' (c:cs) | p c = pure (c, cs)
    tokenOf' _            = empty

sepBy1 :: Eq a => Parser [a] b -> a -> Parser [a] [b]
sepBy1 parser sep = some
  where
    some = many <|> pure <$> parser
    many = (:) <$> (parser <* token sep) <*> some

end :: Parser [a] ()
end = Parser end'
  where
    end' [] = pure ((), [])
    end' _  = empty



newtype Regex = Regex Branch
    deriving Show

type Branch = [Seq]

type Seq = [Piece]

type Piece = (Atom, Quantifier)

data Atom = CharAtom Char
          | Group Branch
    deriving Show

data Quantifier = None
                | Optional
                | Repeat
    deriving Show


parse :: String -> Maybe Regex
parse s = (runParser parser) s >>= return . Regex . fst
  where
    parser = branch <* end

branch :: Parser String Branch
branch = sepBy1 seq '|'

seq :: Parser String Seq
seq = many piece

piece :: Parser String Piece
piece = (,) <$> atom <*> quantifier

atom :: Parser String Atom
atom = charAtom <|> group

charAtom :: Parser String Atom
charAtom = CharAtom <$> charOf isAtomChar

group :: Parser String Atom
group = Group <$> (char '(' *> branch <* char ')')

quantifier :: Parser String Quantifier
quantifier = Repeat <$ char '*' <|> Optional <$ char '?' <|> pure None

char :: Char -> Parser String Char
char = token

charOf :: (Char -> Bool) -> Parser String Char
charOf = tokenOf

isAtomChar :: Char -> Bool
isAtomChar = flip notElem "|()*?"



data NFAState a = NFAState { runNFA :: [a] -> [(NFAState a, [a])] }
                | NFAEndState


compile :: Regex -> NFAState Char
compile (Regex branch) = compileBranch NFAEndState branch

compileBranch :: NFAState Char -> Branch -> NFAState Char
compileBranch next branch = NFAState $ newState
  where
    newState s = zip (compileBranch' branch) (cycle [s])
    compileBranch' []   = [next]
    compileBranch' seqs = map (compileSeq next) seqs

compileSeq ::NFAState Char -> Seq -> NFAState Char
compileSeq next (piece:seq) = compilePiece (compileSeq next seq) piece
compileSeq next []          = next

compilePiece :: NFAState Char -> Piece -> NFAState Char
compilePiece next (a, None)     = compileAtom next a
compilePiece next (a, Optional) = NFAState $ newState
  where
    newState s = [(compileAtom next a, s), (next, s)]
compilePiece next (a, Repeat)   = NFAState $ newState
  where
    newState s = [(compileAtom (NFAState newState) a, s), (next, s)]

compileAtom :: NFAState Char -> Atom -> NFAState Char
compileAtom next (CharAtom char) = NFAState $ newState
  where
    newState (c:cs) | c == char = [(next, cs)]
    newState _                  = []
compileAtom next (Group branch) = compileBranch next branch


match :: String -> String -> Maybe Bool
match regex s = parse regex >>= Just . flip matchRegex s

matchRegex :: Regex -> String -> Bool
matchRegex = matchNFA . compile

matchNFA :: NFAState Char -> String -> Bool
matchNFA nfa = or . matchNFA' nfa
  where
    matchNFA' :: NFAState Char -> String -> [Bool]
    matchNFA' NFAEndState [] = [True]
    matchNFA' NFAEndState _  = [False]
    matchNFA' nfa         s  = let next = runNFA nfa s
                               in concatMap (uncurry matchNFA') next

searchNFA :: NFAState Char -> String -> Bool
searchNFA nfa = or . concatMap (searchNFA' nfa) . takeWhile (not . null) . iterate tail
  where
    searchNFA' :: NFAState Char -> String -> [Bool]
    searchNFA' NFAEndState _ = [True]
    searchNFA' nfa         s = let next = runNFA nfa s
                               in concatMap (uncurry searchNFA') next


grep :: String -> [FilePath] -> IO ()
grep regex paths = case parse regex of
                     Just regex -> mapM_ (flip grepNFA $ compile regex) paths
                     Nothing    -> putStrLn $ "Invalid pattern: `" ++ regex ++ "'"

grepNFA :: FilePath -> NFAState Char -> IO ()
grepNFA path nfa = catch (readFile path >>= mapM_ (uncurry printLine) . matchedLines . lines)
                         (\e -> putStrLn $ "File not found: " ++ path ++ " (" ++ show e ++ ")")
  where
    matchedLines :: (Enum a, Num a) => [String] -> [(String, a)]
    matchedLines l = [ (s, n) | (s, n) <- zip l [1..], searchNFA nfa s ]
    printLine :: Show a => String -> a -> IO ()
    printLine s n = putStrLn (path ++ ":" ++ show n ++ ":" ++ s)
