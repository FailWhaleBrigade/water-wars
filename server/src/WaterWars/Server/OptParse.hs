module WaterWars.Server.OptParse where


import ClassyPrelude
import Options.Applicative

data Arguments =
    Arguments
        { hostname :: Text
        , port :: Int
        , fps :: Float
        , monitorPort :: Int
        , gameMapFiles :: [String]
        } deriving (Show, Eq, Ord, Read)

argumentsParser :: Parser Arguments
argumentsParser =
    Arguments
        <$> hostnameParser
        <*> portParser
        <*> fpsParser
        <*> performanceMonitorParser
        <*> gameMapParser

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

performanceMonitorParser :: Parser Int
performanceMonitorParser = option
    auto
    (  long "monitor"
    ++ metavar "Port"
    ++ help "Port for the performance monitor server"
    ++ value 12001
    )

gameMapParser :: Parser [String]
gameMapParser = many
    (argument
        str
        (  metavar "Game Map ..."
        ++ help
               (  "List of play fields that the server should "
               ++ "serve in a cycle everytime a game has been won"
               )
        )
    )
