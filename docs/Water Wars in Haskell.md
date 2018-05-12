# Water Wars in Haskell

## Pitch

2D-shooter, side-view, quick and easy gameplay

Map contains water

## Software Architecture

The game is implemented via a server-client archtecture.

### Used technologies

#### Frontend

* Gloss
* JuicyPixels

#### Communication

* Haskell read/show, maybe: cereal
* websockets

#### Server

* async
* stm
* extensible-effects

### Architecture of the Server

The work on the server is split among several concurrent working threads.

* The Game Loop
* The Event Loop
* Client Connection
    * Sending thread
    * receiving thread

#### The Game Loop

The task of the Game Loop is to calculate the next states of the game by calling the `gameLoop` function in regular intervals.

It communicates with the Event Loop thread by using stm's `TVar` variables.

#### The Event Loop

The event loop thread processes the messages sent from the clients. The messages get aggregated for the server 

#### Client Connection

A connection to a client is handled using 2 threads.

The reader-thrad listens on the connection and deserialzes the received data. The read message is sent to the Event Loop's input channel for it to be processed.

The sender thread is given a reader-end of a `TChan`, from which it waits for messages, serializing them and sending them to the client.

Both treads stay alive until the connection is terminated.

## Game Mechanics

During a game, the server and the clinets need to agree on a state of the map. The map consists of following things:

* Terraint
* Players
* Projectiles

All players should see the same state all the time during the game.

### Terrain

The terrain is a grid in which solid blocks can be placed. The solid blocks serve as walls, floors and ceilings for players and projectiles. Blocks can have different textures, which defined their look.
The Terrain does not change its chape or state during the game.

### Player

Players can move with arrow-keys and walk on the ground. Movement in-air is also possible but far slower.

Players have a rectangular hitbox. The players position is determined by the bottom-center point of its hitbox.

### Projectiles

Projectiles can get shot by players during the game.

Projectiles are effected by gravity and get slowed down by air drag. Projectiles stick in the surface of blocks.

When hitting a player, the projectile disappears and damages the player.

The hitbox of a projectile is a single point.


# Old Stuff

needs to be included in top part

## Game Engine Decision

### Gloss

Advantages:
- Simple Game Loop design
- Follows roughly the ELM architecture with the possibility to break out if necessary
- Loads of potentially useful libraries
- Can include sound if required
- Provides facilities to play a series of images

Disadvantages:
- Might not be abstract enough, e.g. batteries not included
- Might require us to write a lot of boilerplate code
- Error handling not visible, e.g. program crash
- No custom fonts

### Helm (using SDL2)

Advantages:
- Follows strictly ELM Architecture
- Break Out only via `Cmd`
- Easy to read
- Standard facilities for handling events via `Sub`'s
- Nice Examples

Disadvantages:
- No Sound
- Only .png files are possible
- It is impossible to provide a loading screen for loading resources
- Error handling not visible, e.g. program crash
- Websocket communication will be very complicated

### SDL2

Advantages:
- Can implement everything we need
- Medium-Level Abstraction
- Basic Error Handling documented

Disadvantages:
- Requires to write a lot of boilerplate

### OpenGL with GLUT

Advantages:
- Can implement everything we need
- Medium-Level Abstraction

Disadvantages:
- Requires to write a lot of boilerplate

### Elm

Advantage:
- no install necessary
- browser gives good UI-primitives
- maximum cross-platform

Main Disadvantage:
- no typeclasses and monads

## Components to do

- server game engine
- server event loop
- server connection management
- gameState serialization (binary, cereal, protobuf, custom...)
- client connection
- client event loop
- client textures
- client gameState rendering

## Game Design

Requirements:

* There is a map (small-ish)
* There are players on the map
* The player can jump & move
* The players can shoot
* Missiles have ballistic curve
* Hits kill a player instantly

Extend possibilities for:

* map editor
* items
* power-ups
* abilities
* different characters
* HP

## Gameplay

* Playable via Keyboard
    - "WASD" for movement
    - Space bar for jumping
    - some key for shooting (only forward)
* Multiplayer via Network

