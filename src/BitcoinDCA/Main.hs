{-# LANGUAGE NamedFieldPuns #-}
module BitcoinDCA.Main where

import BitcoinDCA.Config
import UnliftIO.IO (hSetBuffering, stdout, BufferMode (NoBuffering))
import qualified BitcoinDCA.CLI as CLI
import BitcoinDCA.CLI (CLI(..))
import Options.Applicative (customExecParser)
import Data.Yaml.Config (useEnv, loadYamlSettings)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  CLI {configFile} <- customExecParser CLI.parserPrefs CLI.parserInfo
  fromConfig =<< loadYamlSettings [configFile] [] useEnv
