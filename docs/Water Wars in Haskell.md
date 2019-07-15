# Water Wars in Haskell

## Game Design

Water Wars is a 2D platform shooter game, which is played in short, fast-paced rounds. In order for the game to be fair, all players should see the same state at all times, this is handled using a server that repeatedly informs all clients about the current game state.

The original game idea consisted of the map having two areas with different physical properties, underwater and above water.

### Terrain

The terrain is represented by a grid in which solid blocks can be placed. The solid blocks serve as impassable borders for players and projectiles. Blocks can have different textures, which define their look.
The Terrain does not change its shape or state during the game, but the server host has option to design their own map using a textfile.

### Player

Players move using the arrow keys in order to navigate the ground and platforms. Movement in the air is also possible but controls react more slowly.

Players have a rectangular hitbox. A player's position is determined by the bottom-center point of its hitbox.

### Projectiles

Projectiles are shot by players during the game. Projectiles are destroyed when they collide with a block. The hitbox of a projectile is a single point.

When colliding with a player, the projectile kills the player and disappears.

## System Architecture

The game is implemented using a server-client architecture. Communication between these two components is conducted via networking. Clients can connect to the server via websockets. The Server receives actions from the players, calculates the next state and propagates it to all clients.

One server process can only manage one game.

### Server Concurrency Model

The server performs multiple actions concurrently as follows. 
Handling client connections and calculating the next game state based on user input. The user input is handled in the event loop.
Thus, we can identify the following threading architecture.

* Game Loop: Calculates new game ticks regularly depending on the current player actions.
* Event Loop: Processes messages and can create messages for clients and the game loop.
* Websocket Application: A thread that listens for incoming connections.
* Per Client: Send Loop.
* Per Client: Receive Loop.

![Server Thread Architecture](https://github.com/FailWhaleBrigade/water-wars/raw/master/docs/water-wars-server.png)


#### The Event Loop

The event loop is responsible for handling messages from the various clients, the game loop and the Websocket Application.
To receive various kind of events, we create an event queue.
Different actors can send messages to the event loop using the event queue.
Clients send messages such as `Login`, `Logout` and `PlayerAction`, which are handled by the event loop accordingly, e.g. registering a player as an entity in the game loop, removing them from the game loop or saving their game input for processing.
The game loop regularly grabs the player actions so far and uses them for the next game state.
The Websocket Application informs the event loop whenever a connection opens or closes.

Thus, the event loop is the coordinator of the server architecture.

#### The Game Loop

The task of the Game Loop is calculating the next state of the game by calling the `gameLoop` function in regular intervals.

It receives player actions from the Event Loop thread by using a shared, synchronized variable. However, game state changes are propagated using the event queue which the event loop propagates to the registered clients.

#### Websocket Application

The websocket library starts a own thread that listens for incoming requests, which then result in the creation of client threads.

#### Client Connection

A connection to a client is handled using two threads.

The reader thread listens to the connection and deserializes the received data. The read message is sent to the event loop's event queue to be processed there.

The sender thread waits for messages from the event loop, serializes them and sends them to the client.

Both threads stay alive until the connection is terminated. If one of the threads is terminated for any reason, both threads will be restarted.

### Client Concurrency Model

The client's concurrecy model is limited by the restrictions from the gloss framework:

* gloss must run in the main thread.
* The window is open as long the gloss-thread runs.
* The gloss thread can only be stopped by terminating the whole program.

Thus, we need different threads for updates that are received from the server and to send the user input to the server.
These threads communicate over a single shared, synchronized variable.

## Used technologies

For this project, multiple technologies are included to help development. 

### Client

* gloss
* JuicyPixels
* proteaaudio
* mtl

`gloss` is the main windowing and graphics library. It is used for rendering the actual state and receiving user input. 

`JuicyPixels` is used for loading pixel art images, which are encoded as png, and transforming them into bitmaps. This step takes place during runtime and overwrites old bitmaps. The library `proteaaudio` is used for playing sound and is based on the `pulseaudio` library. Sound files are provided in the Ogg format and loaded at runtime. Hence, the files can be replaced to change the sounds.

Error handling in resource loading (e.g. images) is performed with  the `ExceptionT` monad transformer from the `mtl` library. This provides an easy to use interface for resource loading and meaningful error messages.

### Server

* extensible-effects
* monad-logger

The game logic implementation is based on the concept of effects from the `extensible-effects` library, which enables an alternative approach to effect handling compared to the `mtl` library. It promises improved composability, which is why we decided to give it a try.

The server uses a logger framework. There are multiple libraries that support mature logging mechanisms. After some experimenting, we settled on the `monad-logger` library. All messages are logged, however, only messages that are not regurarly sent, e.g. game state updates or player actions, are shown in `stdout` using the default logger mode.

### Communication

For communication between the server and client we use Websockets. 
This may not be the best solution for our application, due to the properties of TCP connections. However, it was the most simple to integrate into our project and never caused any problems and has therefore not been changed yet.

In the early stages of the project, we used the built-in serialization technique of GHC. This had a severe impact on performance, thus, we switched the serialization technique to `cereal`. 

### General

Client and server share some dependencies. One of these dependencies is `classy-prelude`. It provides a prelude based on typeclasses and replaces unsafe functions such as `head` and `tail` with safer variants.

Command-line arguments are parsed with the option parser combinator library `optparse-applicative`. It facilitates the declarative parsing of arguments for the application and generates a fitting parser function and a usage message for users.


For shared, synchronized variables the `stm` library is used. It provides the `TVar` that client and server use for communication across multiple threads.

Additionally, arrays are used for the game map.

### Build Environment

The project uses `stack` for dependency management. 
For package declarations, we also use the `hpack` tool which generates the configuration file for `cabal`.
The project is based on the `haskeleton` project template that adds folders for tests, executables, benchmarks and library code.
Moreover, a file for the continous integration tool [Travis CI](https://travis-ci.org/) is created.
For writing and running unit tests, the frameworks `tasty` and `hspec` are used.

### Language extensions

A number of language extensions is used in this project. 
Some of them provide syntactic sugar for existing functionality such as:

- NamedFieldPuns
- OverloadedStrings
- RecordWildCards
- LambdaCase

The `DeriveGeneric` extension is required for autogenerating implementations for serialization.

`FlexibleContexts`, `MonoLocalBinds` and `ScopedTypeVariables` are requirements for using the `extensible-effects` library. 

`NoImplicitPrelude` is used to replace the standard `Prelude` module with `ClassyPrelude`. We could have used the package `base-noprelude` instead of this language extension, but did not know that in the beginning of the project.


## Further Planned Improvements

Unfortunately, the project is far from being finished.

There are numerous improvements for gameplay and visuals.
For example, player models should be identifiable by avatar coloring.
Also the map visuals are not finished yet, multiple decorations have been designed and need to be included.

The initial idea of water physics has not been implemented, yet, due to time constraints. Hence, implementing these is still a planned improvement.

To increase usability, a launcher for starting the server and client without using a command line terminal would be a good addition.

A technical enhancement would be changing from Websocket communication to udp sockets. 
