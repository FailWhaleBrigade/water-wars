module WaterWars.Server.OptParse where


import ClassyPrelude
import Options.Applicative

data Arguments =
    Arguments
        { hostname :: Text
        , port :: Int
        , fps :: Float
        } deriving (Show, Eq, Ord, Read)

argumentsParser :: Parser Arguments
argumentsParser = Arguments <$> hostnameParser <*> portParser <*> fpsParser

hostnameParser :: Parser Text
hostnameParser = strOption
    (  long "hostname"
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
    ++ help "Port for the Server to listen to"
    ++ value 1234
    )

fpsParser :: Parser Float
fpsParser = option
    auto
    (  long "fps"
    ++ short 's'
    ++ metavar "FPS"
    ++ help "Number of frames per second that the server shall perform"
    ++ value 60
    )