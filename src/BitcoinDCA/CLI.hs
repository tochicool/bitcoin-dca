module BitcoinDCA.CLI where

import Options.Applicative (Parser, ParserInfo, fullDesc, header, help, helper, info, long, metavar, progDesc, short, strOption, value, (<**>), ParserPrefs, disambiguate, prefs, showHelpOnError, showDefault)

newtype CLI = CLI
  { configFile :: FilePath
  }

parserInfo :: ParserInfo CLI
parserInfo =
  info
    ( parser <**> helper )
    ( fullDesc
        <> header "bitcoin-dca - Automate dollar cost averaging on exchanges"
        <> progDesc "Validate and run the strategies as configured from CONFIG"
    )

parserPrefs :: ParserPrefs
parserPrefs = prefs $ disambiguate <> showHelpOnError

parser :: Parser CLI
parser =
  CLI
    <$> strOption
      ( long "config-file"
          <> short 'f'
          <> metavar "CONFIG"
          <> value "config/config.yaml"
          <> showDefault
          <> help "The path to the config yaml file to load"
      )
