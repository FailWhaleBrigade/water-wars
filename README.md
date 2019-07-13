# Water-Wars

[![Build Status](https://travis-ci.org/FailWhaleBrigade/water-wars.svg?branch=frontend)](https://travis-ci.org/FailWhaleBrigade/water-wars)

The game idea was to clone "Towerfall", a fast paced multiplayer shooting-platforming game. For a more creative touch, this game was supossed to feature under-water levels where your movement is drastically different. Mermaids fight to become the princess and are shooting with bubbles at each other.

However, since this course should take about 75 hours per person, you can imagine that we had to reduce our core product. Unfortunately, we had to throw away the underwater idea, since it would have required a lot of additional animations and effort regarding the physics.

## How To Play

You have to host a server and connect with a client. In the beginning, the game is in a warm-up phase, e.g. you cant kill other players on the server.
When all players are ready, the game starts. The players signal that they are ready by pressing `enter` on the keyboard. You can use `w`, `a`, and `d` to move, while `s` is the jump button. You can aim with the mouse where you want to shoot the bubble. Be careful, it takes a second or two to shoot again, so if you are close to the enemy and you miss, it might cost you your crown!
When only one player is alive, they are the victor! After a short amount of time, the next map is chosen, which may be just the same again, if you didn't create another one. The game never ends, so you have to eventually stop.

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

### Dependencies

Some c lib dependencies:

* zlib
* pulseaudio
* freeglut
* mesa
