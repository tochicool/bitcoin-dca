{-# LANGUAGE NumericUnderscores #-}

module CBPDCA.DCA
    ( dollarCostAveraging
    )
where

import           CBPDCA.Strategy                ( Strategy )
import           CoinbasePro.Authenticated      ( placeMarketOrder )
import           CoinbasePro.Authenticated.Orders
                                                ( Order )
import           CoinbasePro.MarketData.Types   ( Product(..) )
import           CoinbasePro.Types              ( ProductId
                                                , unProductId
                                                , Side(Buy)
                                                , Funds(..)
                                                )
import           CoinbasePro.Unauthenticated    ( products )
import           Control.Concurrent.Forkable    ( threadDelay
                                                , forkIO
                                                )
import           Control.Exception              ( SomeException
                                                , throw
                                                )
import           Control.Monad                  ( forM_
                                                , void
                                                )
import           Control.Monad.Catch            ( Handler(..) )
import           Control.Monad.Cont             ( liftIO
                                                , when
                                                , forever
                                                )
import           Control.Monad.Except.Extended  ( throwIfNothing
                                                , catchError
                                                )
import           Control.Monad.Trans            ( MonadTrans(lift) )
import           Control.Retry                  ( fullJitterBackoff
                                                , recovering
                                                )
import           Data.List                      ( find )
import           Data.Time                      ( getCurrentTime
                                                , diffUTCTime
                                                )
import           Katip                          ( Severity(..)
                                                , logTM
                                                , logStr
                                                )
import           System.Cron                    ( describe
                                                , serializeCronSchedule
                                                )
import           System.Cron.Describe           ( defaultOpts )
import           System.Cron.Extended           ( CronSchedule
                                                , previousMatch
                                                , nextMatch
                                                )
import           Text.Printf                    ( printf )


dollarCostAveraging :: Double -> ProductId -> CronSchedule -> Strategy ()
dollarCostAveraging amount productToBuyId frequency = do

    let frequencyCron        = serializeCronSchedule frequency
    let frequencyDescription = describe defaultOpts frequency
    currentTime  <- liftIO getCurrentTime
    previousTime <-
        previousMatch frequency currentTime
            `throwIfNothing` printf
                                 "The cron schedule '%s' (%s) will never match"
                                 frequencyCron
                                 frequencyDescription
    nextTime <-
        nextMatch frequency currentTime
            `throwIfNothing` printf
                                 "The cron schedule '%s' (%s) will never match"
                                 frequencyCron
                                 frequencyDescription
    void . forkIO . forever $ do
        liftIO . threadDelay $ 1_000_000 * 60
        $(logTM) InfoS "Heartbeat"
    dollarCostAveraging' previousTime nextTime
  where
    dollarCostAveraging' previousTime nextTime = do
        let dv   = amount
            dt   = nextTime `diffUTCTime` previousTime
            rate = dv / realToFrac dt
        availableProducts         <- lift . lift $ products
        productToBuy@Product {..} <-
            find ((==) productToBuyId . productId) availableProducts
                `throwIfNothing` printf
                                     "The product '%s' is not in the list of\
                                      \ available products"
                                     (unProductId productToBuyId)
        let funds = rate
            clamp mn x mx = max mn (min x mx)
            adjustedFunds = quoteIncrement * fromIntegral
                (clamp (ceiling $ minMarketFunds / quoteIncrement)
                       (round $ funds / quoteIncrement)
                       (floor $ maxMarketFunds / quoteIncrement)
                )
            tickMS = fromIntegral . round $ 1_000_000 * adjustedFunds / funds
            tick = tickMS / 1_000_000
            ordersPerSecond = 1 / tick
        $(logTM) InfoS . logStr @String $ printf
            "DCAing into %s at a rate of %f %s per second"
            baseCurrency
            rate
            quoteCurrency
        $(logTM) InfoS . logStr @String $ printf
            "Placing order %f times per second"
            (show ordersPerSecond)
        when (tick < 0.2) $ $(logTM) WarningS . logStr @String $ printf
            "Need to hit private API %.3f times per second in order to meet\
            \ investment target. This is above the rate limit of 5 times per\
            \ second and so results may be unstable"
            (show ordersPerSecond)
        currentTime <- liftIO getCurrentTime
        let timeUntilNext   = nextTime `diffUTCTime` currentTime
            timeUntilNextMS = 1_000_000 * timeUntilNext
        forM_ (takeWhile (< timeUntilNextMS) [0, tickMS ..]) $ \s -> forkIO $ do
            liftIO . threadDelay . round $ s
            buy adjustedFunds productToBuy
        liftIO . threadDelay . round $ timeUntilNextMS
        timeAfterNext <-
            nextMatch frequency nextTime `throwIfNothing` "Impossible"
        dollarCostAveraging' nextTime timeAfterNext


buy :: Double -> Product -> Strategy Order
buy funds Product { productId, baseCurrency, quoteCurrency } =
    recovering
            (fullJitterBackoff 1_000_000)
            [ const . Handler $ \(exception :: SomeException) -> do
                  $(logTM) ErrorS . logStr @String $ printf
                      "An error occured whilst placing an order. Will retry: %s"
                      (show exception)
                  return True
            ]
        $ \_ -> do
              order <- lift $ catchError
                  (placeMarketOrder Buy
                                    productId
                                    Nothing
                                    Nothing
                                    Nothing
                                    Nothing
                                    Nothing
                                    (Just (Funds funds))
                  )
                  throw
              $(logTM) NoticeS . logStr @String $ printf
                  "Placed market order for %f %s worth of %s"
                  funds
                  quoteCurrency
                  baseCurrency
              return order
