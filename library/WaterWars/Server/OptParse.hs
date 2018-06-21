module WaterWars.Server.OptParse where


import ClassyPrelude
import Options.Applicative

data Arguments =
    Arguments
        { hostname :: Text
        , port :: Int
        } deriving (Show, Eq, Ord, Read)

argumentsParser :: Parser Arguments
argumentsParser = Arguments <$> hostnameParser <*> portParser

hostnameParser :: Parser Text
hostnameParser = strOption
    (  long "hostname"
    ++ short 'h'
    ++ metavar "Hostname"
    ++ help "Hostname where to run"
    ++ value "localhost"
    )

portParser :: Parser Int
portParser = option
    auto
    (  long "port"
    ++ short 'p'
    ++ metavar "Port"
    ++ help "Port forthe Server"
    ++ value 1234
    )