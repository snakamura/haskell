module Main where

import Control.Monad
import List hiding (group)
import Maybe
import Prelude hiding (seq)
import System

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
        parser = do b <- branch
                    empty
                    return b

branch :: Parser String Branch
branch = sepBy1 seq '|'

seq :: Parser String Seq
seq = many piece

piece :: Parser String Piece
piece = do a <- atom
           q <- quantifier
           return (a, q)

atom :: Parser String Atom
atom =     charAtom
       <|> group

charAtom :: Parser String Atom
charAtom = do c <- charOf isAtomChar
              return $ CharAtom c

group :: Parser String Atom
group = do char '('
           b <- branch
           char ')'
           return $ Group b

quantifier :: Parser String Quantifier
quantifier =     do char '*'
                    return Repeat
             <|> do char '?'
                    return Optional
             <|> return None

empty :: Parser [a] ()
empty = Parser empty'
    where
        empty' [] = Just ((), [])
        empty' _  = Nothing

always :: b -> Parser a b
always x = Parser $ \s -> Just (x, s)

char :: Char -> Parser String Char
char = token

charOf :: (Char -> Bool) -> Parser String Char
charOf = tokenOf

token :: Eq a => a -> Parser [a] a
token c = tokenOf (==c)

tokenOf :: (a -> Bool) -> Parser [a] a
tokenOf = Parser . token'
    where
        token' f []                = Nothing
        token' f (c:s) | f c       = Just (c, s)
                       | otherwise = Nothing

many :: Parser a b -> Parser a [b]
many parser =     do r <- parser
                     m <- many parser
                     return $ r:m
              <|> do r <- parser
                     return [r]
              <|> do return []

sepBy1 :: Eq a => Parser [a] b -> a -> Parser [a] [b]
sepBy1 parser sep =     do r <- parser
                           token sep
                           m <- sepBy1 parser sep
                           return $ r:m
                    <|> do r <- parser
                           return [r]

isAtomChar :: Char -> Bool
isAtomChar = flip notElem "|()*?"


newtype Parser a b = Parser { runParser :: a -> Maybe (b, a) }

instance Monad (Parser a)
    where
        (>>=)  = (|>>=)
        return = always
        fail _ = Parser $ \_ -> Nothing

instance MonadPlus (Parser a)
    where
        mzero = Parser $ \_ -> Nothing
        mplus = (<|>)

infixl 1 <|>
(<|>) :: Parser a b -> Parser a b -> Parser a b
(<|>) (Parser p1) (Parser p2) = Parser $ \s -> case p1 s of
                                                   Nothing -> p2 s
                                                   r       -> r

infixl 1 |>>=
(|>>=) :: Parser a b -> (b -> Parser a c) -> Parser a c
(|>>=) (Parser p) f = Parser $ \s -> case p s of
                                         Nothing     -> Nothing
                                         Just (v, s) -> runParser (f v) $ s

infixl 1 |>>
(|>>) :: Parser a b -> Parser a c -> Parser a c
(|>>) p1 p2 = p1 |>>= \_ -> p2


data NFAState a = NFAState { runNFA :: [a] -> [(NFAState a, [a])] }
                | NFAEndState

instance Eq (NFAState a)
    where
        NFAEndState == NFAEndState = True
        _           == _           = False

compile :: Regex -> NFAState Char
compile (Regex branch) = compileBranch NFAEndState branch

compileBranch :: NFAState Char -> Branch -> NFAState Char
compileBranch next branch = NFAState $ newState
    where
        newState s = zip (compileBranch' branch) (cycle [s])
        compileBranch' (seq:[])     = [compileSeq next seq]
        compileBranch' (seq:branch) = compileSeq next seq:compileBranch' branch
        compileBranch' []           = [next]

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
        newState (c:s) | c == char = [(next, s)]
                       | otherwise = []
        newState _                 = []
compileAtom next (Group branch) = compileBranch next branch


match :: String -> String -> Maybe Bool
match regex s = parse regex >>= Just . flip matchRegex s

matchRegex :: Regex -> String -> Bool
matchRegex regex = or . matchNFA (compile regex)

matchNFA :: NFAState Char -> String -> [Bool]
matchNFA NFAEndState [] = [True]
matchNFA NFAEndState _  = [False]
matchNFA nfa         s  = let next = runNFA nfa $ s
                          in concatMap (uncurry matchNFA) next


main :: IO()
main = do args <- getArgs
          let pattern = args !! 0
              s = args !! 1
          case parse pattern of
              Just regex -> case matchRegex regex s of
                                True  -> putStrLn "Match"
                                False -> putStrLn "Not Match"
              Nothing    -> putStrLn $ "Invalid pattern: `" ++ pattern ++ "'"
