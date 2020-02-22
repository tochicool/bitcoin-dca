module Main where

import           Configuration                  ( Configuration(..)
                                                , commandLine
                                                , Settings(..)
                                                )
import           CBPDCA.DCA                     ( dollarCostAveraging )
import           CBPDCA.Strategy                ( runStrategy
                                                , Context(..)
                                                )
import           CoinbasePro.Authenticated.Request.Extended
                                                ( CoinbaseProCredentials(..) )
import           Control.Exception              ( bracket )
import           Control.Monad.Cont             ( when
                                                , void
                                                )
import           Control.Monad.Error.Class      ( catchError )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Validate         ( runValidate
                                                , refute
                                                )
import qualified Data.DList                    as DList
import           Data.DList                     ( DList )
import           Data.Monoid.Same               ( Same(..) )
import           Data.Semigroup                 ( Last(..)
                                                , Any(..)
                                                , All(..)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Yaml                     as Yaml
import           Katip                          ( logTM
                                                , logStr
                                                , Severity(..)
                                                , permitItem
                                                , Verbosity(V3)
                                                , registerScribe
                                                , closeScribes
                                                , defaultScribeSettings
                                                , initLogEnv
                                                , ColorStrategy(ColorIfTerminal)
                                                , mkHandleScribe
                                                , katipAddNamespace
                                                )
import           Options.Applicative            ( customExecParser
                                                , disambiguate
                                                , prefs
                                                )
import           System.Cron                    ( serializeCronSchedule )
import           System.Exit                    ( exitWith
                                                , ExitCode(ExitFailure)
                                                )
import           System.IO                      ( stdout )
import           Text.Printf                    ( printf )


main :: IO ()
main = do

    commandLineConfig@Configuration { configFile } <-
        liftIO . customExecParser (prefs disambiguate) $ commandLine

    yamlFileConfig <- case configFile of
        Just (Last configFile) -> Yaml.decodeFileEither configFile >>= \case
            Left parseException -> do
                putStrLn "Error encountered whilst parsing config file: "
                putStrLn . Yaml.prettyPrintParseException $ parseException
                exitWith . ExitFailure $ 2
            Right configuration -> return configuration
        _ -> do
            putStrLn "Config file path must be provided"
            exitWith . ExitFailure $ 2

    Settings {..} <- case getSettings (yamlFileConfig <> commandLineConfig) of
        Left errors -> do
            putStrLn "The following errors where found in the configuration:"
            traverse (printf "  - %s\n") (DList.toList errors)
            exitWith . ExitFailure $ 2
        Right settings -> return settings

    let credentials = CoinbaseProCredentials accessKey secretKey passphrase

    -- Handle quiet flag
    let priority    = if quiet then ErrorS else InfoS

    -- Setup logger
    handleScribe <- mkHandleScribe ColorIfTerminal
                                   stdout
                                   (permitItem priority)
                                   V3
    let environment = if sandbox then "development" else "production"
    let mkLogEnv =
            registerScribe "stdout" handleScribe defaultScribeSettings
                =<< initLogEnv "CBP-DCA" environment
    bracket mkLogEnv closeScribes $ \le ->
        void $ runStrategy sandbox credentials (Context mempty mempty le) $ do

            $(logTM) InfoS . logStr $ "Log Level: " ++ show priority
            $(logTM) InfoS . logStr @Text $ "Starting strategy"

            -- Run strategy
            katipAddNamespace "dollar_cost_averaging"
                $            dollarCostAveraging amount productId frequency
                `catchError` \errorMessage -> do
                                 $(logTM) CriticalS
                                     .  logStr
                                     $  "Strategy failed. "
                                     ++ errorMessage
                                 liftIO . exitWith $ ExitFailure 3

-- | Relies on ApplicativeDo for multiple error reporting
getSettings :: Configuration -> Either (DList String) Settings
getSettings Configuration {..} = runValidate $ do
    accessKey <- case accessKey of
        Just (Last accessKey) -> return accessKey
        _ -> refute . pure $ "The API access key is not provided"
    secretKey <- case secretKey of
        Just (Last secretKey) -> return secretKey
        _ -> refute . pure $ "The API secret key is not provided"
    passphrase <- case passphrase of
        Just (Last passphrase) -> return passphrase
        _ -> refute . pure $ "The API access passphrase is not provided"
    productId <- case productId of
        Just (Last productId) -> return productId
        _ -> refute . pure $ "The product id is not provided"
    amount <- case amount of
        Just (Same amount                 ) -> return amount
        Just (NotSame amount anotherAmount) -> refute . pure $ printf
            "Different values for amount: %f and %f"
            amount
            anotherAmount
        _ -> refute . pure $ "The amount is not provided"
    frequency <- case frequency of
        Just (Same frequency                    ) -> return frequency
        Just (NotSame frequency anotherFrequency) -> refute . pure $ printf
            "Different values for frequency: %s and %s"
            (serializeCronSchedule frequency)
            (serializeCronSchedule anotherFrequency)
        _ -> refute . pure $ "The frequency is not provided"
    sandbox <- case sandbox of
        Nothing            -> return False
        Just (Any sandbox) -> return sandbox
    quiet <- case quiet of
        Nothing          -> return False
        Just (All quiet) -> return quiet
    when (amount <= 0) $ refute . pure $ "The amount must be positive"
    return Settings { accessKey
                    , secretKey
                    , passphrase
                    , productId
                    , amount
                    , frequency
                    , sandbox
                    , quiet
                    }
