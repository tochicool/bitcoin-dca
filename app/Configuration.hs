module Configuration
    ( Configuration(..)
    , Settings(..)
    , commandLine
    )
where

import           CoinbasePro.Authenticated.Headers
                                                ( CBAccessKey(..)
                                                , CBAccessPassphrase(..)
                                                )
import           CoinbasePro.Authenticated.Request.Extended
                                                ( CBSecretKey(..) )
import           CoinbasePro.Types              ( ProductId(..) )
import           Control.Monad                  ( mzero )
import           Data.Maybe                     ( fromJust )
import           Data.Monoid.Same               ( Same(..) )
import           Data.Semigroup                 ( All(..)
                                                , Any(..)
                                                , Last(..)
                                                )
import qualified Data.Text                     as Text
import qualified Data.Yaml                     as Yaml
import           GHC.Generics                   ( Generic )
import           Generics.Deriving.Semigroup    ( gsappenddefault
                                                , GSemigroup(..)
                                                )
import           Options.Applicative
import           System.Cron                    ( CronSchedule
                                                , parseCronSchedule
                                                )
import           Text.Read                      ( readEither )

data Configuration = Configuration
    { configFile :: Maybe (Last FilePath)
    , accessKey  :: Maybe (Last CBAccessKey)
    , secretKey  :: Maybe (Last CBSecretKey)
    , passphrase :: Maybe (Last CBAccessPassphrase)
    , productId  :: Maybe (Last ProductId)
    , amount     :: Maybe (Same Double)
    , frequency  :: Maybe (Same CronSchedule)
    , sandbox    :: Maybe Any
    , quiet      :: Maybe All
    } deriving (Generic, Show, Eq)

instance Yaml.FromJSON Configuration

instance (Eq a) => GSemigroup (Same a) where
    gsappend = (<>)
instance Semigroup Configuration where
    (<>) = gsappenddefault

instance Yaml.FromJSON CronSchedule where
    parseJSON (Yaml.String cronExpression) =
        either (const mzero) return $ parseCronSchedule cronExpression
    parseJSON _ = mzero

deriving instance Generic a => Generic (Same a)
instance Yaml.FromJSON a => Yaml.FromJSON (Same a) where
    parseJSON x = (fmap Same . Yaml.parseJSON $ x) <|> pure DegenerateSame

deriving instance Yaml.FromJSON Any

deriving instance Yaml.FromJSON All

deriving instance Yaml.FromJSON CBAccessKey

deriving instance Yaml.FromJSON CBSecretKey
deriving instance Show CBSecretKey

deriving instance Yaml.FromJSON CBAccessPassphrase

data Settings = Settings
    { accessKey  :: CBAccessKey
    , secretKey  :: CBSecretKey
    , passphrase :: CBAccessPassphrase
    , productId  :: ProductId
    , amount     :: Double
    , frequency  :: CronSchedule
    , sandbox    :: Bool
    , quiet      :: Bool
    } deriving (Show)

commandLine :: ParserInfo Configuration
commandLine = info
    (commandLineArguments <**> helper)
    (  fullDesc
    <> progDesc "Automate dollar cost averaging on coinbase pro"
    <> header "CBP-DCA v0.1.0.0, (C) Tochukwu Obudulu"
    )

commandLineArguments :: Parser Configuration
commandLineArguments =
    Configuration
        <$> option
                (maybeReader (pure . pure . pure))
                (  long "config-file"
                <> short 'c'
                <> metavar "CONFIG"
                <> showDefaultWith (show . getLast . fromJust)
                <> value (pure . pure $ "config.yaml")
                <> action "file"
                <> help
                       "The path to the configuration file. For security reasons,\
            \ the API secret key must be set here."
                )
        <*> option
                (maybeReader (pure . pure . pure . CBAccessKey))
                (  long "access-key"
                <> short 'k'
                <> metavar "ACCESS_KEY"
                <> value Nothing
                <> help "The API access key"
                )
        <*> pure Nothing
        <*> option
                (maybeReader (pure . pure . pure . CBAccessPassphrase))
                (  long "passphrase"
                <> long "access-passphrase"
                <> short 'p'
                <> metavar "PASSPHASE"
                <> value Nothing
                <> help "The API access key"
                )
        <*> option
                (maybeReader (pure . pure . pure . ProductId . Text.pack))
                (  long "product-id"
                <> short 'i'
                <> metavar "PRODUCT_ID"
                <> value Nothing
                <> help "The ID of the product to DCA into"
                )
        <*> option
                (eitherReader (fmap (pure . Same) . readEither))
                (  long "amount"
                <> short 'a'
                <> metavar "AMOUNT"
                <> value (pure DegenerateSame)
                <> help
                       "The target amount of the quote currency to spend on the\
                \ product every trade"
                )
        <*> option
                (eitherReader
                    (fmap (pure . Same) . parseCronSchedule . Text.pack)
                )
                (  long "frequency"
                <> short 'f'
                <> metavar "FREQUENCY"
                <> value (pure DegenerateSame)
                <> help
                       "How often to DCA into the product. Accepts a cron\
                \ expression or one of @yearly, @monthly, @weekly, @daily or\
                \ @hourly"
                )
        <*> fmap
                (Just . Any)
                (switch
                    (long "sandboxed" <> short 's' <> help
                        "Use the sandboxed development API environment"
                    )
                )
        <*> fmap
                (Just . All)
                (switch
                    (long "quiet" <> short 'q' <> help
                        "Sets the log level to NOTICE and above"
                    )
                )
