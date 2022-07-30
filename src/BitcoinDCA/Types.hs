{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BitcoinDCA.Types where

import BitcoinDCA.Common
import Control.Monad (guard)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String), withText)
import Data.Aeson.Types (typeMismatch)
import Data.ByteString (ByteString)
import Data.Coerce (Coercible, coerce)
import Data.Data (Proxy (Proxy))
import Data.Maybe (fromJust)
import Data.String (IsString)
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Haskoin as Bitcoin
import qualified Language.Bitcoin.Script.Descriptors as Bitcoin
import Money (Dense, ExchangeRate, SomeDense, dense, denseCurrency, exchange, someDenseCurrency, withSomeDense)
import System.Cron (CronSchedule, nextMatch, parseCronSchedule, serializeCronSchedule)

newtype AssetId = AssetId ByteString
  deriving (Generic, Show, IsString, Eq, Ord)

instance FromJSON AssetId where
  parseJSON = byteStringParseJSON

instance ToJSON AssetId where
  toJSON = byteStringToJSON

newtype OrderId = OrderId ByteString
  deriving (Generic, Show)

newtype Address = Address ByteString
  deriving (Generic, Show)

instance FromJSON Address where
  parseJSON = byteStringParseJSON

instance ToJSON Address where
  toJSON = byteStringToJSON

newtype OutputDescriptor = OutputDescriptor Bitcoin.OutputDescriptor
  deriving (Generic, Show)

instance FromJSON OutputDescriptor where
  parseJSON = withText "Output descriptor" $ \desc ->
    case Bitcoin.parseDescriptor Bitcoin.btc desc of
      Left err -> fail err
      Right x -> return $ OutputDescriptor x

instance ToJSON OutputDescriptor where
  toJSON (OutputDescriptor desc) = String $ Bitcoin.descriptorToText Bitcoin.btc desc

newtype WithdrawId = WithdrawId ByteString
  deriving (Generic, Show)

newtype Size = Size SomeDense
  deriving newtype (Show, Eq, Ord)

instance FromJSON Size where
  parseJSON = someDenseParseJSON "Size"

instance ToJSON Size where
  toJSON = someDenseToJSON

newtype Funds = Funds SomeDense
  deriving newtype (Show, Eq, Ord)

instance FromJSON Funds where
  parseJSON = someDenseParseJSON "Funds"

instance ToJSON Funds where
  toJSON = someDenseToJSON

-- | A non-negative amount of money
newtype Money asset = Money (Dense asset)
  deriving newtype (Eq, Ord, Num, Real)

instance forall asset. KnownSymbol asset => Show (Money asset) where
  showsPrec n (Money d) =
    let c = symbolVal (Proxy :: Proxy asset)
     in showParen (n > 10) $
          showString (showDecimal $ toRational d)
            . showChar ' '
            . (c ++)

data OrderStatus
  = OrderFailed
  | OrderPending
  | OrderComplete
  deriving (Generic, Show)

data Asset (asset :: Symbol) = Asset
  { id :: AssetId,
    maxPrecision, minWithdrawalAmount :: Money asset,
    maxWithdrawalAmount :: Maybe (Money asset)
  }
  deriving (Generic, Show, Eq)

data AssetPair (base :: Symbol) (quote :: Symbol) = AssetPair
  { base :: AssetId,
    quote :: AssetId,
    minMarketFunds :: Money quote,
    maxMarketFunds :: Maybe (Money quote),
    quoteIncrement :: Money quote
  }
  deriving (Generic, Show, Eq)

data Order base = Order {id :: OrderId, status :: OrderStatus, filledSize :: Money base}
  deriving (Generic, Show)

data Withdraw = Withdraw
  { id :: WithdrawId,
    status :: WithdrawStatus
  }
  deriving (Generic, Show)

data WithdrawStatus
  = WithdrawFailed
  | WithdrawPending
  | WithdrawComplete
  deriving (Generic, Show)

class Target a where
  type TargetQuote a :: Symbol
  fundsAt :: a -> UTCTime -> Money (TargetQuote a)
  earliestTimeAt :: a -> Money (TargetQuote a) -> UTCTime

data CronTarget quote = CronTarget
  { startTime :: UTCTime,
    everyTick :: Money quote,
    cronSchedule :: CronSchedule
  }
  deriving (Show)

instance Target (CronTarget quote) where
  type TargetQuote (CronTarget quote) = quote
  fundsAt t@CronTarget {..} currentTime
    | Just previousTime <- previousMatch cronSchedule currentTime = (everyTick *) . money' . toRational $ fundsAt' t {startTime = previousTime} currentTime
    | otherwise = 0
    where
      fundsAt' CronTarget {..} currentTime
        | Just nextTime <- nextMatch cronSchedule startTime =
          case currentTime `compare` nextTime of
            LT ->
              currentTime `diffUTCTime` startTime
                / nextTime `diffUTCTime` startTime
            GT ->
              1
                + fundsAt' CronTarget {startTime = nextTime, ..} currentTime
            EQ -> 1
        | otherwise = 0

  earliestTimeAt t@CronTarget {..} funds
    | Just previousTime <- previousMatch cronSchedule startTime =
      flip addUTCTime previousTime $ earliestTimeAt' t {startTime = previousTime} funds
    | otherwise = startTime
    where
      earliestTimeAt' CronTarget {..} totalFunds
        | Just nextTime <-
            nextMatch cronSchedule startTime,
          everyTick > 0,
          totalFunds > 0 = case everyTick `compare` totalFunds of
          LT ->
            nextTime `diffUTCTime` startTime
              + earliestTimeAt'
                CronTarget {startTime = nextTime, ..}
                (totalFunds - everyTick)
          GT ->
            (nextTime `diffUTCTime` startTime)
              * fromRational (toRational totalFunds / toRational everyTick)
          EQ -> nextTime `diffUTCTime` startTime
        | otherwise = 0

-- | Will return the previous time from the given starting point where
-- this schedule would have matched. Returns Nothing if the schedule will
-- never match. Note that this function is inclusive of the given
-- time.
previousMatch :: CronSchedule -> UTCTime -> Maybe UTCTime
previousMatch cs now =
  -- This uses 'one-sided' binary search, making O(lg n) calls to nextMatch
  do
    next <- nextMatch cs now
    let beforeNext time = do
          current <- nextMatch cs time
          if current == next
            then return time
            else beforeNext current
    let searchPrevious lookBehind = do
          current <- nextMatch cs (addUTCTime (- lookBehind) now)
          if current < next
            then beforeNext current
            else searchPrevious $ 2 * lookBehind
    searchPrevious 60

newtype Frequency = CronSchedule CronSchedule
  deriving (Generic, Show)

instance FromJSON Frequency where
  parseJSON (String cronExpression) =
    either fail return $ CronSchedule <$> parseCronSchedule cronExpression
  parseJSON value = typeMismatch "String" value

instance ToJSON Frequency where
  toJSON (CronSchedule x) = toJSON $ serializeCronSchedule x

money :: Rational -> Maybe (Money asset)
money x = do
  d <- dense x
  guard (d >= 0)
  return $ Money d

money' :: Rational -> Money asset
money' = fromJust . money

exchangeMoney :: ExchangeRate src dest -> Money src -> Money dest
exchangeMoney rate (Money dense) = Money $ exchange rate dense

withSomeMoney :: Coercible a SomeDense => a -> (forall asset. KnownSymbol asset => Money asset -> r) -> r
withSomeMoney x f = withSomeDense (coerce x) (f . Money)

assetId :: forall asset. KnownSymbol asset => AssetId
assetId = AssetId $ Text.encodeUtf8 $ denseCurrency @asset 0

sizeAssetId :: Size -> AssetId
sizeAssetId (Size someDense) = AssetId $ Text.encodeUtf8 $ someDenseCurrency someDense

fundsAssetId :: Funds -> AssetId
fundsAssetId (Funds someDense) = AssetId $ Text.encodeUtf8 $ someDenseCurrency someDense

isShitcoin :: forall asset. KnownSymbol asset => Bool
isShitcoin = assetId @asset /= "BTC"
