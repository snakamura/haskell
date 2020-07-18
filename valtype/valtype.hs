{-# LANGUAGE AllowAmbiguousTypes,
             DataKinds,
             GADTs,
             KindSignatures,
             ScopedTypeVariables,
             StandaloneDeriving,
             TypeApplications
#-}

import Data.Text (Text)

data ValType = Text | Bool

data Value (tag :: ValType) where
    T :: Text -> Value 'Text
    B :: Bool -> Value 'Bool
deriving instance Show (Value tag)

data SomeValue = forall tag. SomeValue (Value tag)

data SValType (tag :: ValType) where
    SText :: SValType 'Text
    SBool :: SValType 'Bool

class SValTypeI (tag :: ValType) where sing :: SValType tag
instance SValTypeI 'Text where sing = SText
instance SValTypeI 'Bool where sing = SBool

unwrap :: forall tag. SValTypeI tag => SomeValue -> Maybe (Value tag)
unwrap (SomeValue v) =
    case sing @tag of
        SBool | B _ <- v -> Just v
        SText | T _ <- v -> Just v
        _ -> Nothing

unwrapExplicit :: SValType tag -> SomeValue -> Maybe (Value tag)
unwrapExplicit sValType (SomeValue v) =
    case sValType of
        SBool | B _ <- v -> Just v
        SText | T _ <- v -> Just v
        _ -> Nothing

class IsType (a :: ValType) where typeOf :: ValType
instance IsType 'Text where typeOf = Text
instance IsType 'Bool where typeOf = Bool

{-
unwarp' :: forall tag. IsType tag => SomeValue -> Maybe (Value tag)
unwarp' (SomeValue v) =
    case typeOf @tag of
        Bool | B _ <- v -> Just v
        Text | T _ <- v -> Just v
        _ -> Nothing

unwarpExplicit' :: ValType -> SomeValue -> Maybe (Value tag)
unwarpExplicit' valType (SomeValue v) =
    case valType of
        Bool | B _ <- v -> Just v
        Text | T _ <- v -> Just v
        _ -> Nothing
-}
