{-# LANGUAGE DataKinds,
             ExistentialQuantification,
             GADTs,
             KindSignatures,
             OverloadedStrings,
             ScopedTypeVariables,
             StandaloneDeriving,
             TypeApplications
#-}

import Data.Text (Text)

data ValueType = Text | Bool

data Value (valueType :: ValueType) where
    T :: Text -> Value 'Text
    B :: Bool -> Value 'Bool

deriving instance Show (Value valueType)

data SomeValue = forall valueType. SomeValue (Value valueType)

textValue :: Value 'Text
textValue = T "ABC"

boolValue :: Value 'Bool
boolValue = B True

someTextValue, someBoolValue :: SomeValue
someTextValue = SomeValue textValue
someBoolValue = SomeValue boolValue


data SValueType (valueType :: ValueType) where
    SText :: SValueType 'Text
    SBool :: SValueType 'Bool

class KnownValueType (valueType :: ValueType) where
    knownValueType :: SValueType valueType
instance KnownValueType 'Text where
    knownValueType = SText
instance KnownValueType 'Bool where
    knownValueType = SBool

unwrap :: forall valueType. KnownValueType valueType => SomeValue -> Maybe (Value valueType)
unwrap (SomeValue value) =
    case knownValueType @valueType of
        SText | T _ <- value -> Just value
              | otherwise -> Nothing
        SBool | B _ <- value -> Just value
              | otherwise -> Nothing

v1 = unwrap someTextValue :: Maybe (Value 'Text)
v2 = unwrap someTextValue :: Maybe (Value 'Bool)
v3 = unwrap someBoolValue :: Maybe (Value 'Text)
v4 = unwrap someBoolValue :: Maybe (Value 'Bool)
