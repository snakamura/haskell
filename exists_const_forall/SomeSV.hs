module SomeSV where

import Data.Kind

type data V = V1 | V2

type SV :: V -> Type
data SV v where
  MkSV1 :: Int -> SV V1
  MkSV2 :: String -> SV V2

type SomeSV :: Type
data SomeSV where
  MkSomeSV :: SV v -> SomeSV

someSV1, someSV2 :: SomeSV
someSV1 = MkSomeSV (MkSV1 1)
someSV2 = MkSomeSV (MkSV2 "x")

fromSomeSV :: SomeSV -> String
fromSomeSV (MkSomeSV sv) =
  case sv of
    MkSV1 n -> show n
    MkSV2 s -> s

toSomeSV :: Int -> SomeSV
toSomeSV n = MkSomeSV (MkSV1 n)
