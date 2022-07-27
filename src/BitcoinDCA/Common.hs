{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BitcoinDCA.Common where

import Control.Monad.Except
import Data.Aeson (FromJSON (parseJSON), Options (omitNothingFields, rejectUnknownFields, sumEncoding), SumEncoding (tagFieldName), ToJSON (toJSON), Value (Number, Object, String), defaultOptions, defaultTaggedObject, withText)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.ByteString (ByteString)
import Data.Coerce (Coercible, coerce)
import qualified Data.HashMap.Lazy as HashMap
import Data.Ratio (Ratio, denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as Text
import Money (Dense, SomeDense, dense', mkSomeDense, someDenseAmount, someDenseCurrency)
import Network.HTTP.Types (notFound404)
import Servant.Client
import Text.Pretty.Simple (pShow, pString)

isMultipleOf :: RealFrac a => a -> a -> Bool
isMultipleOf _ 0 = False
isMultipleOf x y = isInt $ toRational x / toRational y

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

lengthAtMost :: (Ord t, Num t) => [a] -> t -> Bool
lengthAtMost _ n | n < 0 = False
lengthAtMost [] _ = True
lengthAtMost (_ : xs) n = xs `lengthAtMost` (n -1)

expectNotFound :: MonadError ClientError m => m a -> m (Maybe a)
expectNotFound m = catchError (Just <$> m) $ \case
  FailureResponse _ Response {..} | responseStatusCode == notFound404 -> return Nothing
  err -> throwError err

decimalParseJSON :: Coercible a (Dense asset) => Value -> Parser a
decimalParseJSON (Number decimal) = return . coerce . dense' . toRational $ decimal
decimalParseJSON (String decimal) = do
  case Text.rational @Rational decimal of
    Left err -> fail err
    Right (n, rest)
      | Text.null rest -> return . coerce . dense' $ n
      | otherwise -> fail $ "unexpected input: " <> Text.unpack rest
decimalParseJSON value = typeMismatch "Decimal" value

decimalToJSON :: forall asset a. Coercible a (Dense asset) => a -> Value
decimalToJSON = toJSON . Text.pack . showDecimal . toRational @(Dense asset) . coerce

someDenseParseJSON :: Coercible a SomeDense => String -> Value -> Parser a
someDenseParseJSON name = withText name $ \decimal ->
  case Text.rational @Rational decimal of
    Left err -> fail err
    Right (n, rest)
      | Just (' ', asset) <- Text.uncons rest,
        not (Text.null asset),
        Just someDense <- mkSomeDense asset n ->
        return . coerce $ someDense
      | otherwise -> fail $ "unexpected input: " <> Text.unpack rest

someDenseToJSON :: Coercible a SomeDense => a -> Value
someDenseToJSON x =
  let someDense = coerce x
   in String $ Text.concat [Text.pack . showDecimal $ someDenseAmount someDense, " ", someDenseCurrency someDense]

byteStringParseJSON :: Coercible a ByteString => Value -> Parser a
byteStringParseJSON x = coerce . encodeUtf8 <$> parseJSON x

byteStringToJSON :: Coercible a ByteString => a -> Value
byteStringToJSON = toJSON . decodeUtf8 . coerce

-- | From https://stackoverflow.com/a/30938328
showDecimal :: (Integral a, Show a) => Ratio a -> String
showDecimal rat = signedPart ++ shows d (dot next ++ go next)
  where
    signedPart = if num < 0 then "-" else ""
    dot n | n > 0 = "."
    dot _ = ""
    (d, next) = abs num `quotRem` den
    num = numerator rat
    den = denominator rat
    go 0 = ""
    go x =
      let (d, next) = (10 * x) `quotRem` den
       in shows d (go next)

jsonConfigOptions :: Options
jsonConfigOptions =
  defaultOptions
    { omitNothingFields = True,
      sumEncoding = defaultTaggedObject {tagFieldName = "tag"},
      rejectUnknownFields = True
    }

(?:.) :: Text -> Value -> Value -> Value
field ?:. value = \case
  Object o ->
    Object $
      HashMap.alter
        ( Just . \case
            Just x -> x
            _ -> value
        )
        field
        o
  x -> x

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust x f = case x of
  Just x -> f x
  Nothing -> pure ()

whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing x m = maybe m pure x

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb m = do
  b <- mb
  when b m

type family IsText a where
  IsText String = 'True
  IsText Text = 'True
  IsText TL.Text = 'True
  IsText _ = 'False

class LogShow (isText :: Bool) a where
  logShow :: IsText a ~ isText => a -> Text

instance LogShow 'True String where
  logShow = TL.toStrict . pString

instance LogShow 'True Text where
  logShow = id

instance LogShow 'True TL.Text where
  logShow = TL.toStrict

instance (Show a) => LogShow 'False a where
  logShow = TL.toStrict . pShow

(<+) :: LogShow (IsText a) a => a -> Text -> Text
x <+ y = logShow x <> y

(+>) :: LogShow (IsText a) a => Text -> a -> Text
x +> y = x <> logShow y

newtype Secret = Secret ByteString

instance Show Secret where
  show _ = "***"
