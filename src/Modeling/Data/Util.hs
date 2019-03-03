module Modeling.Data.Util where

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

-- TODO these are probably better expressed with prisms and isos

data Bijection a b = Bijection { biApply :: a -> b, biInvert :: b -> a }

idBijection :: Bijection a a
idBijection = Bijection id id

composeBijection :: Bijection b c -> Bijection a b -> Bijection a c
composeBijection (Bijection apl1 inv1) (Bijection apl2 inv2) = Bijection (apl1 . apl2) (inv2 . inv1)

flipBijection :: Bijection a b -> Bijection b a
flipBijection (Bijection apl inv) = Bijection inv apl

lowerBijection :: Bijection a b -> Injection Void a b
lowerBijection (Bijection apl inv) = Injection apl (Right . inv)

data Injection e a b = Injection { injApply :: a -> b, injInvert :: b -> Either e a }

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

renderErrorMsg :: ErrorMsg -> String
renderErrorMsg = T.unpack . unErrorMsg

jsonInjection :: (ToJSON a, FromJSON a, IsString e) => Injection e a Value
jsonInjection = Injection toJSON inv where
    inv v =
        case (parseEither parseJSON) v of
            Left e -> Left (fromString e)
            Right a -> Right a

injectionToJSON :: ToJSON b => Injection e a b -> a -> Value
injectionToJSON (Injection { injApply }) a = toJSON (injApply a)

injectionParseJSON :: FromJSON b => (e -> String) -> Injection e a b -> Value -> Parser a
injectionParseJSON render (Injection { injInvert }) v = do
    b <- parseJSON v
    case injInvert b of
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

onlyNameError :: SumInjectionError ne Void -> ne
onlyNameError (SumNameError ne) = ne

sumInjection :: Injection ne n Text -> Injection ae a (n, Maybe b) -> Injection (SumInjectionError ne ae) a (Sum b)
sumInjection ninj ainj = Injection apl inv where
    apl a =
        let (n, mv) = (injApply ainj) a
            t = (injApply ninj) n
        in Sum t mv
    inv (Sum t mv) =
        case (injInvert ninj) t of
            Left ne -> Left (SumNameError ne)
            Right n ->
                case (injInvert ainj) (n, mv) of
                    Left ae -> Left (SumAttributesError ae)
                    Right a -> Right a

sumInjection' :: Injection ne n Text -> Bijection a (n, Maybe b) -> Injection ne a (Sum b)
sumInjection' ninj abij = injectionMapError onlyNameError (sumInjection ninj (lowerBijection abij))

simpleSumInjection :: Injection ae a (Text, Maybe b) -> Injection (SumInjectionError Void ae) a (Sum b)
simpleSumInjection = sumInjection idInjection

type SumErrorMsg = SumInjectionError ErrorMsg ErrorMsg

missingAttrs, unexpectedAttrs :: ErrorMsg
missingAttrs = ErrorMsg "Missing attrs"
unexpectedAttrs = ErrorMsg "Unexpected attrs"

simpleWithoutAttrs :: Maybe a -> Either ErrorMsg (Maybe a)
simpleWithoutAttrs ma = case ma of { Nothing -> Right Nothing; Just _ -> Left unexpectedAttrs }

simpleWithAttrs :: (a -> Maybe b) -> Maybe a -> Either ErrorMsg (Maybe a)
simpleWithAttrs s ma = case (ma >>= s) of { Nothing -> Left missingAttrs; Just _ -> Right ma }

withoutAttrs :: b -> Maybe a -> Either ErrorMsg b
withoutAttrs y ma = case ma of { Nothing -> pure y; Just _ -> Left unexpectedAttrs }

withAttrs :: (a -> Maybe v) -> (v -> Either ErrorMsg b) -> Maybe a -> Either ErrorMsg b
withAttrs s f ma = case (ma >>= s) of { Nothing -> Left missingAttrs; Just x -> f x }
