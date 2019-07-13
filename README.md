# Water-Wars

[![Build Status](https://travis-ci.org/FailWhaleBrigade/water-wars.svg?branch=frontend)](https://travis-ci.org/FailWhaleBrigade/water-wars)

## Build project with stack

Start the server.

```bash
> stack build
> stack exec water-wars-server -- --help
Fail Whale Brigade presents Water Wars.

Usage: water-wars-server [--hostname Hostname] [-p|--port Port] [-s|--fps FPS]
                         [--monitor Port] [Game Map ...]
  Start an instance of the water-wars server.

Available options:
  --hostname Hostname      Hostname where to run
  -p,--port Port           Port for the Server to listen to
  -s,--fps FPS             Number of frames per second that the server shall
                           perform
  --monitor Port           Port for the performance monitor server
  Game Map ...             List of play fields that the server should serve in a
                           cycle everytime a game has been won
  -h,--help                Show this help text
> stack exec water-wars-server -- -p 9999 --hostname "0.0.0.0" resources/game1.txt
```

Start the client:

```bash
> stack build
> stack exec water-wars-client -- --help
Fail Whale Brigade presents Water Wars.

Usage: water-wars-client [--hostname Hostname] [-p|--port Port] [-q|--quiet]
                         [--monitor Port] [--fullscreen]
  Start an instance of the water-wars client.

Available options:
  --hostname Hostname      Hostname of the Server Instance
  -p,--port Port           Port to connect to
  -q,--quiet               Silences the music of the game
  --monitor Port           Port for the performance monitor server
  --fullscreen             Starts the game in fullscreen mode
  -h,--help                Show this help text
> stack exec water-wars-client -- -p 9999 --hostname "0.0.0.0" --fullscreen
```

Exit the game with alt+f4.

### Build project with cabal


Start the server.

```bash
> cabal v2-build all
> cabal v2-run exe:water-wars-server -- --help
Fail Whale Brigade presents Water Wars.

Usage: water-wars-server [--hostname Hostname] [-p|--port Port] [-s|--fps FPS]
                         [--monitor Port] [Game Map ...]
  Start an instance of the water-wars server.

Available options:
  --hostname Hostname      Hostname where to run
  -p,--port Port           Port for the Server to listen to
  -s,--fps FPS             Number of frames per second that the server shall
                           perform
  --monitor Port           Port for the performance monitor server
  Game Map ...             List of play fields that the server should serve in a
                           cycle everytime a game has been won
  -h,--help                Show this help text
> cabal v2-run exe:water-wars-server -- -p 9999 --hostname "0.0.0.0" resources/game1.txt
```

Start the client:

```bash
> cabal v2-build all
> cabal v2-run exe:water-wars-client -- --help
Fail Whale Brigade presents Water Wars.

Usage: water-wars-client [--hostname Hostname] [-p|--port Port] [-q|--quiet]
                         [--monitor Port] [--fullscreen]
  Start an instance of the water-wars client.

Available options:
  --hostname Hostname      Hostname of the Server Instance
  -p,--port Port           Port to connect to
  -q,--quiet               Silences the music of the game
  --monitor Port           Port for the performance monitor server
  --fullscreen             Starts the game in fullscreen mode
  -h,--help                Show this help text
> cabal v2-run exe:water-wars-client -- -p 9999 --hostname "0.0.0.0" --fullscreen
```

Exit the game with alt+f4.
