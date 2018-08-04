module WaterWars.Client.OptParse where


import ClassyPrelude
import Options.Applicative

data Arguments =
    Arguments
        { hostname :: Text
        , port :: Int
        , quiet :: Bool
        , performanceMonitoring :: Maybe Int
        , fullScreen :: Bool
        } deriving (Show, Eq, Ord, Read)

argumentsParser :: Parser Arguments
argumentsParser =
    Arguments
        <$> hostnameParser
        <*> portParser
        <*> quietParser
        <*> performanceMonitorParser
        <*> fullScreenParser

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
quietParser =
    switch (long "quiet" ++ short 'q' ++ help "Silences the music of the game")

performanceMonitorParser :: Parser (Maybe Int)
performanceMonitorParser = optional $ option
    auto
    (long "monitor" ++ metavar "Port" ++ help
        "Port for the performance monitor server"
    )

fullScreenParser :: Parser Bool
fullScreenParser = switch
    (  long "fullscreen"
    ++ help "Starts the game in fullscreen mode"
    )

