module Modeling.Data.Util where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Modeling.Data.Bidi

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
injectionToJSON (Injection { injApply }) = toJSON . injApply

injectionToEncoding :: ToJSON b => Injection e a b -> a -> Encoding
injectionToEncoding (Injection { injApply }) = toEncoding . injApply

injectionParseJSON :: FromJSON b => (e -> String) -> Injection e a b -> Value -> Parser a
injectionParseJSON render (Injection { injInvert }) v = do
    b <- parseJSON v
    case injInvert b of
        Right a -> pure a
        Left e -> fail (render e)

data Sum a = Sum Text (Maybe a) deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

-- TODO Pull Sum instance code into this and use this in them
-- sumInjection :: Injection e a Value -> Injection e (Sum a) Value
-- sumInjection (Injection innerApl innerInj) = Injection outerApl outerInj where
--     outerApl = undefined
--     innerApl = undefined

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

data DomainInjectionError ne ae = DomainNameError ne | DomainAttributesError ae deriving (Generic, Eq, Show)

onlyNameError :: DomainInjectionError ne Void -> ne
onlyNameError (DomainNameError ne) = ne

onlyAttributesError :: DomainInjectionError Void ae -> ae
onlyAttributesError (DomainAttributesError ae) = ae

domainInjection :: Injection ne n Text -> Injection ae a (n, Maybe b) -> Injection (DomainInjectionError ne ae) a (Sum b)
domainInjection ninj ainj = Injection apl inv where
    apl a =
        let (n, mv) = (injApply ainj) a
            t = (injApply ninj) n
        in Sum t mv
    inv (Sum t mv) =
        case (injInvert ninj) t of
            Left ne -> Left (DomainNameError ne)
            Right n ->
                case (injInvert ainj) (n, mv) of
                    Left ae -> Left (DomainAttributesError ae)
                    Right a -> Right a

domainInjection' :: Injection ne n Text -> Bijection a (n, Maybe b) -> Injection ne a (Sum b)
domainInjection' ninj abij = injectionMapError onlyNameError (domainInjection ninj (voidBijection abij))

simpleDomainInjection :: Injection ae a (Text, Maybe b) -> Injection ae a (Sum b)
simpleDomainInjection = injectionMapError onlyAttributesError . domainInjection idInjection

-- type DomainErrorMsg = DomainInjectionError ErrorMsg ErrorMsg

missingAttrs, unexpectedAttrs :: ErrorMsg
missingAttrs = ErrorMsg "Missing attrs"
unexpectedAttrs = ErrorMsg "Unexpected attrs"

simpleWithoutAttrs :: Maybe a -> Either ErrorMsg (Maybe a)
simpleWithoutAttrs ma = case ma of { Nothing -> Right Nothing; Just _ -> Left unexpectedAttrs }

simpleWithAttrs :: (a -> Maybe b) -> Maybe a -> Either ErrorMsg (Maybe a)
simpleWithAttrs s ma = case (ma >>= s) of { Nothing -> Left missingAttrs; Just _ -> Right ma }

withoutAttrs :: b -> Maybe a -> Either ErrorMsg b
withoutAttrs y ma = case ma of { Nothing -> pure y; Just _ -> Left unexpectedAttrs }

withAttrs :: (a -> Maybe v) -> (v -> b) -> Maybe a -> Either ErrorMsg b
withAttrs s f ma = case (ma >>= s) of { Nothing -> Left missingAttrs; Just x -> Right (f x) }

sumInjection :: Injection e a Value -> Injection e (Sum a) (Sum Value)
sumInjection (Injection vapl vinv) = Injection apl inv where
    apl (Sum n ma) = Sum n (vapl <$> ma)
    inv (Sum n mv) = Sum n <$> ema where
        ema = case mv of
            Nothing -> Right Nothing
            Just v -> Just <$> vinv v
