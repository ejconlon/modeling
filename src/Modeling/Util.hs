module Modeling.Util where

import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)

-- TODO these are probably better expressed with prisms

data Injection e a b = Injection { apply :: a -> b, invert :: b -> Either e a }

-- Category without the id conflict
idInjection :: Injection e a a
idInjection = Injection id Right

composeInjection :: Injection e b c -> Injection e a b -> Injection e a c
composeInjection (Injection apl1 inv1) (Injection apl2 inv2) = Injection (apl1 . apl2) (inv1 >=> inv2)

injectionMapError :: (e -> e') -> Injection e a b -> Injection e' a b
injectionMapError f (Injection apl oldInv) = Injection apl newInv where
    newInv b =
        case oldInv b of
            Left e -> Left (f e)
            Right a -> Right a

-- Aeson errors are stringy
newtype ErrorMsg = ErrorMsg { unErrorMsg :: Text } deriving (Show, Eq, IsString)

jsonInjection :: (ToJSON a, FromJSON a, IsString e) => Injection e a Value
jsonInjection = Injection toJSON inv where
    inv v =
        case (parseEither parseJSON) v of
            Left e -> Left (fromString e)
            Right a -> Right a

injectionToJSON :: ToJSON b => Injection e a b -> a -> Value
injectionToJSON (Injection { apply }) a = toJSON (apply a)

injectionParseJSON :: FromJSON b => (e -> String) -> Injection e a b -> Value -> Parser a
injectionParseJSON render (Injection { invert }) v = do
    b <- parseJSON v
    case invert b of
        Right a -> pure a
        Left e -> fail (render e)

data Sum a = Sum Text (Maybe a) deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (Sum a) where
    toJSON (Sum n ma) = object (("name" .= n):(maybe [] (\a -> ["attributes" .= object [n .= a]]) ma))

instance FromJSON a => FromJSON (Sum a) where
    parseJSON = withObject "Sum" $ \v -> do
        n <- v .: "name"
        mo <- v .:? "attributes"
        case mo of
            Nothing -> pure (Sum n Nothing)
            Just o ->
                if HM.size o /= 1
                    then fail ("Too many branches in sum for " <> T.unpack n)
                    else Sum n <$> o .: n

data SumInjectionError ne ae = SumNameError ne | SumAttributesError ae deriving (Generic, Eq, Show)

sumInjection :: Injection ne n Text -> Injection ae a (n, Maybe b) -> Injection (SumInjectionError ne ae) a (Sum b)
sumInjection ninj ainj = Injection apl inv where
    apl a =
        let (n, mv) = (apply ainj) a
            t = (apply ninj) n
        in Sum t mv
    inv (Sum t mv) =
        case (invert ninj) t of
            Left ne -> Left (SumNameError ne)
            Right n ->
                case (invert ainj) (n, mv) of
                    Left ae -> Left (SumAttributesError ae)
                    Right a -> Right a

simpleSumInjection :: Injection ae a (Text, Maybe b) -> Injection (SumInjectionError Void ae) a (Sum b)
simpleSumInjection = sumInjection idInjection
