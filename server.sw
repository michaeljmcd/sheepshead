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

If we use RFC 3117 [^1] as our starting point, we see that we need to
define the following items as part of our protocol:

* Framing (or message delimiting).

* Message Encoding (or message representation).

* Error Reporting

* Asynchrony

* Authentication

* Privacy

We will specify each of these things in order.

1. Our messages will be delimited in the following manner:
    1. Receiving data marks the beginning of a message.
    2. A message is complete when two consecutive newlines (CR-LF, in this
    case) are received.

2. Each message shall be comprised of a series commands (a complete listing
   of which is below). Each command has two parts: a command name and
   any arguments that are necessary for that command. The end of a command
   is indicated by a single colon (`:`). The precise format of the
   arguments may depend upon the command, though typically they will be
   space separated tokens.

3. Errors are reported in each response. 

4. Communications performed under this protocol will be synchronous. No
support for asynchronous communications will be included in this protocol.
For the given application domain, there is little point. Each and every
request can be thought of as one transaction. There is not necessarily any
reason that a client could not open two transactions, though if there are
multiple requests that would alter the game state, the latter will take
effect.

5. Authentication will be required for all transactions with the server.
A username will be provided with one command and a password provided with
another. See both below for details. A server may decline to make passwords
required (e.g. an open server). A username will still be needed to
correlate the request to a specific player. However, the password command
will not need to be sent.

6. Due to the non-vital nature of game communications, no support for
encryption or other privacy protections will be included in this version of
the protocol.

#### Command Listing ####

`USER: <username>`

:   Specifies the username with which to authenticate this request.

`PASSWORD: <password>`

:   Specifies the password to be used to authenticate the user. All
    passwords must be SHA256 hashed and salted with the username. A
    psuedocode equation would look like:

    `hashed-password = sha256(plaintext-password + username)`

`LIST-GAMES`

:   Lists all active games that the current user has access to.

`JOIN-GAME`

:   Indicates that the current player wishes to join a game. The precise
    game must be indicated with a `GAME` command. Optionally, a seat may be
    specified with the `SEAT` command.

`GAME: <gameid>`

:   Specifies the game that any further operations in this transaction
    operate on.

    Indicating a game that the player has not joined or does not have
    access to will result in an error.

`SEAT: <seat>`

:   The 1-based index of the seat that the user is occupying. This
    instruction is not required (though it may still be sent) if the given
    player occupies only a single seat in the current game.

    Indicating a seat other than one that the player is responsible for
    will result in an error.

`BURY: [<card>]+`

:   Instructs the server to bury one or more cards separated by spaces.

`PLAY-CARD: <card>`

:   Indicates the card to be played, using the notation specified below. A
    `GAME` command is required as part of the transaction. If the user in
    question is playing multiple seats, a `SEAT` command will also be
    required.

`RESIGN`

:   Resigns the player from the game. This command may be combined with the
    `GAME` and `SEAT` commands.

`TAKE-BLIND: <boolean>`

:   Indicates that the player wishes to take the blind. This command may be
    issued before it is the player's turn to decide. If the bid never
    reaches the given player, the command will be disregarded without error
    or warning.

`REGISTER-USER`

:   Begins the process of registering a user. If sent, the `USERNAME` and
    `PASSWORD` commands are treated as though they are registration
    information, not authentication information.

`CREATE-GAME: <name>`

:   Creates a new game having the human-readable name *name*.
    If the game seats do not get filled (either through the addition of
    bots or through the addition of human players), the game will close in
    a length of time specified by the server.

`SEAT-COUNT: <count>`

:   When creating a game, this command indicates the seat count desired.
    Valid values are 3, 4 and 5.

`GOAL-POINTS: <count>`

:   This command indicates the goal to play to when creating a game. A
    value of -1 indicates that play will continue until there are no longer
    a sufficient number of players at which point the remaining player with
    the highest point count will be declared the winner.

`REPLACE-BOTS: <bool>`

:   This command indicates whether bots can be replaced by interested human
    players when playing a game.

`QUERY-GAME-STATE`

:   A no-op command that prompts the server to return game state. Must be
    combined with the `GAME` command and may be combined with the `SEAT`
    command.

#### Response Listing ####

`HAND: [<card>]+`

:   This line contains all of the cards currently in the player's hand.

`CARDS-TAKEN: [<card>]+`

:   A complete list of all cards taken by the current player, including
    cards buried in the blind.

`HAND-POINTS: <count>`

:   The number of points the player has in the current hand. A client may
    also calculate this value from the `CARDS-TAKEN` command--the results
    are required to be in sync.

`GAME-POINTS: <count>`

:   The number of points the player has in the current game.

`SEAT-COUNT: <count>`

:   The number of seats in the current game.

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

Booleans

:   Booleans are case insensitive, non-delimited tokens reading either
    `TRUE` or `FALSE`.

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

[^1]: [RFC 3117](http://www.faqs.org/rfcs/rfc3117.html)

<!--- vim: set tw=75 ai: --->
