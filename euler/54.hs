import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord

value =
  do hands <- liftM parseHands $ readFile "54.txt"
     print $ length $ filter (== GT) $ map (uncurry compareHand) hands

data Suit = Spade | Heart | Diamond | Club deriving (Eq, Show)

data Card = Card {
  suit   :: Suit,
  number :: Int
} deriving (Eq, Show)

parseCard :: String -> Card
parseCard [v, s] = Card (suit s) (value v)
  where
    suit c = fromJust $ lookup c suits
    suits = [('S', Spade), ('H', Heart), ('D', Diamond), ('C', Club)]
    value c = fromMaybe (ord c - ord '0') $ lookup c values
    values = [('T', 10), ('J', 11), ('Q', 12), ('K', 13), ('A', 14)]


type Hand = [Card]

parseHands :: String -> [(Hand, Hand)]
parseHands = map (splitAt 5 . map parseCard . words) . lines

compareHand :: Hand -> Hand -> Ordering
compareHand l r = compare (eval l) (eval r)



data Rank = RoyalFlush
          | StraightFlush Int
          | FourKing Int
          | FullHouse Int Int
          | Flush
          | Straight Int
          | ThreeKing Int
          | TwoPairs Int Int
          | OnePair Int
          | HighCard
 deriving (Eq, Show)

instance Ord Rank where
  compare = compareRank

compareRank RoyalFlush        RoyalFlush        = EQ
compareRank RoyalFlush        _                 = GT
compareRank _                 RoyalFlush        = LT
compareRank (StraightFlush l) (StraightFlush r) = compare l r
compareRank (StraightFlush _) _                 = GT
compareRank _                 (StraightFlush _) = LT
compareRank (FourKing l)      (FourKing r)      = compare l r
compareRank (FourKing _)      _                 = GT
compareRank _                 (FourKing _)      = LT
compareRank (FullHouse l1 l2) (FullHouse r1 r2) = compare l1 r1 `mappend` compare l2 r2
compareRank (FullHouse _  _ ) _                 = GT
compareRank _                 (FullHouse _  _ ) = LT
compareRank Flush             Flush             = EQ
compareRank Flush             _                 = GT
compareRank _                 Flush             = LT
compareRank (Straight l)      (Straight r)      = compare l r
compareRank (Straight _)      _                 = GT
compareRank _                 (Straight _)      = LT
compareRank (ThreeKing l)     (ThreeKing r)     = compare l r
compareRank (ThreeKing _)     _                 = GT
compareRank _                 (ThreeKing _)     = LT
compareRank (TwoPairs l1 l2)  (TwoPairs r1 r2)  = compare l1 r1 `mappend` compare l2 r2
compareRank (TwoPairs _  _ )  _                 = GT
compareRank _                 (TwoPairs _  _ )  = LT
compareRank (OnePair l)       (OnePair r)       = compare l r
compareRank (OnePair _)       _                 = GT
compareRank _                 (OnePair _)       = LT
compareRank _                 _                 = EQ

data Result = Result Rank Hand deriving (Eq, Show)

instance Ord Result where
  compare = compareResult

compareResult :: Result -> Result -> Ordering
compareResult (Result lr lh) (Result rr rh) =
  compare lr rr `mappend` compare (reverse $ sort $ map number lh) (reverse $ sort $ map number rh)

eval :: Hand -> Result
eval hand = Result (rank h) h
  where
    h = sortBy (comparing number) hand

rank :: Hand -> Rank
rank hand = maybe HighCard fromJust $ find isJust $ map (($ hand)) ranks
  where
    ranks = [royalFlush, straightFlush, fourKing, fullHouse, flush, straight, threeKing, twoPairs, onePair]

royalFlush :: Hand -> Maybe Rank
royalFlush hand | map number hand == [10..14] = Just RoyalFlush
                | otherwise                    = Nothing

straightFlush :: Hand -> Maybe Rank
straightFlush hand = case straight hand of
                       Just (Straight n) | isJust (flush hand) -> Just $ StraightFlush n
                       _                                       -> Nothing

fourKing :: Hand -> Maybe Rank
fourKing hand = let g = reverse $ sortBy (comparing length) $ group $ map number hand
                in if length (head g) == 4 then
                     Just $ FourKing (head (head g))
                   else
                     Nothing

fullHouse :: Hand -> Maybe Rank
fullHouse hand = let g = reverse $ sortBy (comparing length) $ group $ map number hand
                 in if length g == 2 && length (g !! 0) == 3 && length (g !! 1) == 2 then
                      Just $ FullHouse (head (g !! 0)) (head (g !! 1))
                    else
                      Nothing

flush :: Hand -> Maybe Rank
flush hand | length (nub (map suit hand)) == 1 = Just Flush
           | otherwise                         = Nothing

straight :: Hand -> Maybe Rank
straight hand | map number hand `elem` candidates = Just $ Straight $ sum $ map number hand
              | otherwise                         = Nothing
  where
    candidates = map sort $ nub $ takeWhile (\x -> length x == 5) $ map (take 5) $ tails $ concat $ replicate 2 $ [2..14]

threeKing :: Hand -> Maybe Rank
threeKing hand = let g = reverse $ sortBy (comparing length) $ group $ map number hand
                 in if length (head g) == 3 then
                      Just $ ThreeKing (head (head g))
                    else
                      Nothing

twoPairs :: Hand -> Maybe Rank
twoPairs hand = let g = reverse $ sortBy (comparing length) $ group $ map number hand
                in if length g == 3 && length (g !! 0) == 2 && length (g !! 1) == 2 then
                     Just $ TwoPairs (head (g !! 1)) (head (g !! 0))
                   else
                     Nothing

onePair :: Hand -> Maybe Rank
onePair hand = let g = reverse $ sortBy (comparing length) $ group $ map number hand
               in if length (head g) == 2 then
                    Just $ OnePair (head (head g))
                  else
                    Nothing
