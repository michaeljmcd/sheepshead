@doc Sheepshead Server [out=docs/01-server.md]
## The Server ##

One of the goals for this system is for it to allow multiple different
clients to connect to a singular server. So far, most of the effort
has been in describing the game core. Now it is time to take a look at
the server. 

The server is:

* TCP/IP based.
* Multiuser.

@code Server [out=src/server.lisp,lang=commonlisp]
(in-package :sheepshead-server)

@<User Declarations>

@<Listening>
@=

### Protocol ###

Each request is, in effect, a transaction with the game server.
Sheepshead need not be considered a real-time game. A command is made
and the results of that command are returned.

Each command has the following components:

* *Command name* indicating what operation is to be performed.
* *Authentication information* (a username with an optional hashed
  password) to determine if the client has authorization to perform
  the operations being requested.
* *Command arguments* which the server can pass along to the core.
  These will obviously vary depending on the operation being
  performed.

This information will be encoded as JSON. My first instinct would
normally be to simply pass S-expressions back and forth which is, in
principle, very similar to using JSON. After all, JSON is little more
than an S-expression for JavaScript. The difference is that JSON has
become sort of a *lingua franca* of object interchange between many
different programming languages and systems.

If someone wanted to write a client to the Sheepshead server in C++ and I
used S-expressions, they would likely have to write their own parser
or else head into some obscure backwater. JSON, on the other hand, is
very well supported.

Each return will have the following components:

* A *game ID* representing the game against which the transaction was
  implemented. This may be empty when administrative operations are
  called.
* Zero or more *return values*. These are dependent upon the operation
  being invoked. If the operation was, for example, play a card, the
  return value may be the new game state.
* A *transaction ID*. This book keeping value is a diagnostic tool.
* Zero or more *conditions*. This will be a list, usually empty, of objects
  that indicate either fatal errors or minor warnings.

#### Command Listing ####

`list-games`

:   Lists all active games that the current user has access to.

    **Additional Parameters**

    *None*.

    **Return Format**

    (#)  `games`--a list of game descriptors for the user to choose
    between.

`create-game`

:   Creates a new game on the server.

    **Additional Parameters**

    (#) `name`--an optional user-visible name.
    (#) `seat-count`--the number of slots available.
    (#) `goal-points`--the number of points to play to. If not specified,
    this number will be defaulted to 100.
    (#) `allow-bots-to-be-bumped`--a boolean value 

    If the game seats do not get filled (either through the addition of
    bots or through the addition of human players), the game will close in
    a length of time specified by the server.

`get-game-state`

:   Queries the server for the state of a specific game.

    **Additional Parameters**

    (#) `game-id`--the ID of the game whose state is to be queried.

`register-user`

:   Requests the creation of a new user on the game server. The server may
    permit open authentication. In this case, the password may be empty, but 
    the server will still check for a unique username.

    **Additional Parameters**

    (#) `username`--a required field indicating the user's handle.
    (#) `email`--a field with an email address. The server may select to make this optional.
    (#) `password`--the plain password for the user.

    The result will include either a user ID if the creation was successful
    or an error message if it failed.

`join-game`

:   Requests that a given player be allowed to join a specific game.

    **Additional Parameters**

    (#) `game-id`--the unique identifier associated with a game. Multiple
    requests of this type have no extra impact (e.g. no error is to be
    signalled if the user is already a part of the game that the request is
    for).

`resign-game`

:   Resigns from a game.

    **Additional Parameters**

    (#) `game-id`--the unique identifier associated with the game. If the
    user is not a part of the game that he is attempting to resign from, no
    error will be returned (he is, after all, already resigned).

`answer-blind`

:    Indicates whether or not a player wishes to take the blind.

    **Additional Parameters**

    (#) `game-id`
    (#) `answer`--a string that must be either "ACCEPT" or "DECLINE" (case
    insensitive).

`bury`

:    Buries cards after having taken the blind. If the player has not
     taken, or cannot take the blind because another player has taken it, a
     `CANNOT-BURY` error is returned.

    **Additional Parameters**

    (#) `game-id`
    (#) `cards`--a list of cards to be buried. 

`play-card`

:   Selects a card to play from the user's current hand. This request may
    be submitted prior to the player's actual turn in the game. In either
    event, the card will be validated. If the card is invalid, an
    `INVALID-CARD` error will be included in the response. If it is valid,
    the card will be played whenever the player's turn comes up.

    If this is sent after the player's card has been selected, but before
    it has been played in the current trick, the selection will be changed
    and the latter card played when the player's turn comes up.

    Finally, if a player submits a new `player-card` request after that
    player's card has been played but before the start of the new trick, it
    will be ignored entirely and an error signalled to the client.

    **Additional Parameters**

    (#) `game-id`--The game that this card will be played into.
    (#) `card`--An object indicating the full card to be played. 

#### Common Datatypes ####

Several data types in the command listing recur and, therefore, bear
description in their own right.

Card

:   This object must have two properties: `rank` indicating the numerical or
    English label of the card to be played ("K", "Q", "10", "9", etc.) and
    `suit` the suit of the card to be played, spelled out in full (e.g.
    "CLUBS", "HEARTS", "SPADE"). Both of these properties are case
    insensitive.

Game ID

:   An alphanumeric string that uniquely identifies a game. What
    constitutes a valid game ID is left entirely to the server
    implementation, the only requirement being that the strings are
    alphanumeric.

Game Descriptor

:   An object describing a game, having the following properties:

    (#) `game-id`--as discussed above.
    (#) `name`--a creator-specified name.
    (#) `seat-count`--the number of players in the game.
    (#) `open-seat-count`--the number of remaining seats to be filled.
    (#) `goal-points`--the number of points to play to.
    (#) `expiration-time`--the time at which this game will be closed if
    insufficient players are added to the game.

#### Examples ####

In order to make the protocol more clear, let's look at some examples.
Examples are more concrete when they are put in the form of a story. So, we
will consider a hypothetical user, Jane, who downloads a client to
Sheepshead. Jane has no comprehension of what her client is doing under the
hood and blissfully plays the game without giving it a second thought. We
will also assume, for simplicity's sake, that this is an open server,
without any need for registration, just the inputting of a username
(IRC-style).

Our examples, then, will have two parts: Jane's action and the
request-response cycle that results from it. The main underlying assumption
will be that Jane's client defaults to the server that we are examining.
So, once again, she does not really need to be thinking in client-server
terms or really in technological terms of any kind.

When Jane first begins a session with her client, she is shown a list of
games on the server. This results in the request:

    {
        command : "list-games",
        username : "jane",
        password : "",
    }

The server then returns the following response:

    {
        transaction-id : "G8732",
        game-id : "",
        return-values : {
            games : [{"gameid" : "G8123"}]
        }
        conditions : []
    }   

A request to create a new game should look like this:

    {
        command : "create-game",
        username : "johnsmith",
        password : "a2e34fab",
        arguments : {
            name: "my-game",
            seat-count : 4,
            goal-points : 25
        }
    }

The return value will then look like:

    {
        transaction-id : "G8732",
        game-id : "G8733",
        return-values : {
        }
        conditions : []
    }

### Multiuser ###

All users will be managed by the server.

@code User Declarations [lang=commonlisp]
(defclass sheepshead-user ()
  )
@=

### Handling Requests ###

@code Listening [lang=commonlisp]
(defparameter *server-host* "localhost")
(defparameter *server-port* 7777)

(defun sheepshead-tcp-request-handler (stream)
    (declare (type stream stream))
    (write-char #\H stream)
    (terpri stream))

(usocket:socket-server *server-host* 
    *server-port*
    #'sheepshead-tcp-request-handler)
@=

### Packaging ###

@code Server Package [out=src/server-package.lisp,lang=commonlisp]
(defpackage :sheepshead-server 
 (:export)
 (:use :common-lisp))
@=

This package then needs to be loaded by the ASDF system.

@code Server ASDF Package [out=sheepshead-server.asd,lang=commonlisp]
(defsystem)
@=

<!--- vim: set tw=75 ai: --->
