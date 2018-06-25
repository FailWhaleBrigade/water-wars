module WaterWars.Client.OptParse where


import ClassyPrelude
import Options.Applicative

data Arguments =
    Arguments
        { hostname :: Text
        , port :: Int
        , quiet :: Bool
        } deriving (Show, Eq, Ord, Read)

argumentsParser :: Parser Arguments
argumentsParser = Arguments <$> hostnameParser <*> portParser <*> quietParser

hostnameParser :: Parser Text
hostnameParser = strOption
    (  long "hostname"
    ++ metavar "Hostname"
    ++ help "Hostname of the Server Instance"
    ++ value "localhost"
    )

portParser :: Parser Int
portParser = option
    auto
    (  long "port"
    ++ short 'p'
    ++ metavar "Port"
    ++ help "Port to connect to"
    ++ value 1234
    )

quietParser :: Parser Bool
quietParser = switch
    (  long "quiet"
    ++ short 'q'
    ++ help "Silences the music of the game"
    )
