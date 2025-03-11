module AnySV where

import Data.Kind

type data V = V1 | V2

type SV :: V -> Type
data SV v where
  MkSV1 :: Int -> SV V1
  MkSV2 :: String -> SV V2

type AnySV :: Type
newtype AnySV = MkAnySV (forall v. SV v)

anySVShow :: AnySV -> String
anySVShow (MkAnySV sv) =
  case sv of
    MkSV1 n -> show n
    MkSV2 s -> s

anySV :: AnySV
anySV = MkAnySV undefined
