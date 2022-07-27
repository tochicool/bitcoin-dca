{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}

module BitcoinDCA.Test.Types (
    module BitcoinDCA.Types,
    TestAssetPair
) where

import BitcoinDCA.Types
import Generic.Random
import Test.QuickCheck

import Data.ByteString as BS

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary

deriving instance Arbitrary AssetId

instance Arbitrary (AssetPair base quote) where
    arbitrary = genericArbitraryU

instance Arbitrary (Money asset) where
    arbitrary = Money <$> arbitrary

type TestAssetPair = AssetPair "BTC" "USD"
