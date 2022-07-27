{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module BitcoinDCA.Test.Strategy.ScheduledBuys where

import           BitcoinDCA.Test.Types
import BitcoinDCA.Common
import           BitcoinDCA.Strategy.ScheduledBuys
import Data.Function
import           Data.Monoid
import           Test.Tasty
import           Test.Tasty.QuickCheck

test_minimumBuys :: [TestTree]
test_minimumBuys =
  [ testProperty "Conserves funds"
      $ \(assetPair :: TestAssetPair) funds -> let (Sum remainder, buys) = minimumBuys assetPair funds
                     in remainder + sum buys == funds
  , testProperty "Minimum market funds is an lower bound of all buys"
      $ \(assetPair@AssetPair { minMarketFunds } :: TestAssetPair) funds
      -> let (_, buys) = minimumBuys assetPair funds
         in all (>= minMarketFunds) buys
  , testProperty "Maximum market funds is an upper bound of all buys"
      $ \(assetPair@AssetPair { maxMarketFunds } :: TestAssetPair) funds
      -> let (_, buys) = minimumBuys assetPair funds
         in all (\x -> maybe True (x <=) maxMarketFunds) buys
  , testProperty "Quote increment is a divisor of all buys"
      $ \(assetPair@AssetPair { quoteIncrement } :: TestAssetPair) funds
      -> let (_, buys) = minimumBuys assetPair funds
         in all ((isMultipleOf `on` toRational) quoteIncrement) buys
  , testProperty "At most one buy is not maximum market funds"
      $ \(assetPair@AssetPair { maxMarketFunds } :: TestAssetPair) funds
      -> let (_, buys) = minimumBuys assetPair funds
         in (`lengthAtMost` (1 :: Integer)) . filter (\x -> Just x /= maxMarketFunds) $ buys
         ]
