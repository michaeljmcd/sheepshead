@doc Sheepshead Server [out=docs/02-server.md]
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
    2. A message is complete when the command `STOP` is encountered.

2. Each message shall be comprised of a series commands (a complete listing
   of which is below). Each command has two parts: a command name and
   any arguments that are necessary for that command. The end of a command
   is indicated by a single colon (`:`). The precise format of the
   arguments may depend upon the command, though typically they will be
   space separated tokens.

   Commands for multiple games may be sent or returned. In this case, a
   `GAME` command will indicate the current command. So, in overview, a
   multi game message would look like:

        GAME: g1
        [commands....]
        GAME: g2
        [commands....]

   This allows some efficiency in terms of server requests and responses.

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

`IS-BOT: <bool>`

:   Allows a client to indicate that it represents a bot. It is not really
    expected that all bots will implement this. The main reason for its
    inclusion is so that two people can begin a game with 3 bots (making 5
    total seats) and allow the bots to be replaced with humans wishing to
    join.

`TRANSACTION: <transaction-id>`

:   This optional command allows a client to specify multiple transactions
    without waiting for a server response. When provided, the server will
    echo this command back out when returning a response. This is a
    client-generated ID. It is, therefore, up to the client to ensure that
    the IDs do not clash.

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

`STOP`

:   A no-op command that indicates the end of a batch of commands.

#### Response Listing ####

`TRANSACTION: <transaction-id>`

:   The client-specified transaction ID. 

`SEAT: <number>`

:   Indicates which seat will be described in the following lines.

`PLAYER: <username>`

:   Indicates the player controlling the mentioned seat.

`HAND: [<card>]+`

:   This line contains all of the cards currently in the player's hand.
    This line will only be returned for the player(s) controlled by the
    requestor.

`GAME-STATUS: <game-status>`

:   Indicates the status of the game as a whole.

`HAND-STATUS: <hand-status>`

:   Indicates the status of the specific hand.

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

`SEATS-AVAILABLE: <count>`

:   The number of seats open in the current game.

`TIME-LEFT: <time>`

:   The amount of time remaining to fill the seats of a game before it is
    closed.

`STOP`

:   A no-op command that indicates the end of a batch of commands.

#### Errors and Warnings ####

Each response will include an error/warning code to indicate the general
health of the transaction. Each code is comprised of 4 digits, which take
on the following significances:

1. The severity digit. This is the highest level overview of what happened.
   A "1" indicates that everything returned without error or warning, a "2"
   indicates a non-fatal warning, and a "3" indicates a fatal error.
2. The second digit indicates which high-level area generated the error.
3. The remaining two digits are used to specify the exact error returned. 

For areas 2 and 3, we specify tables here that give an exact meaning to the
possible codes. Any value not listed is unused at this time.

Area #      Name            Description
------      ----            -----------------------------------------------
0           N/A             For queries and miscellaneous requests.
1           Syntax          Non-area specific syntax errors. 
2           Authentication  Username/password errors.
3           Authorization   Permissions errors.
4           Rule Violation  Attempts to perform actions that violate game
                            rules.
5           Registration    Errors attempting to register a new user.

With the general areas specified, we can begin a complete listing of errors
and warnings issued by the system.

Code        Title                           Description
----        ------------------------        ---------------------------------------------
1000        OK                              No errors or warnings to report.
1500        REGISTRATION ACCEPTED           No errors with a registration
                                            request.        
3201        BAD USERNAME OR PASSWORD        Unrecognized username or
                                            password.
3301        ACCESS DENIED                   An attempt was made to join a
                                            game or perform an action in a game that the 
                                            user does not have permissions to.
3302        BAD SEAT                        The seat specified is not one
                                            played by the user.
3303        BAD GAME                        The game specified is not one
                                            the user is in.
3401        WRONG SUIT                      An attempt was made to play a
                                            wrong-suited card.

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

Game Status

:   The valid

Hand Status

:   The valid

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

    USER: Jane
    TRANSACTION: 1
    PASSWORD: 2efb3
    LIST-GAMES
    STOP

The server can then respond with:

    1000 OK
    TRANSACTION: 1
    GAME: g232a
    GAME-NAME: Johann's game
    SEAT-COUNT: 5
    SEATS-AVAILABLE: 1
    GOAL-POINTS: 100
    GAME: g232c
    GAME-NAME: dragon-quest 17!!!
    SEAT-COUNT: 3
    SEATS-AVAILABLE: 0
    GOAL-POINTS: -1
    STOP

This indicates that there are two open games available. Jane was invited by
email to join Johann's game, so she instructs her client to join the game.
This results in the following server call:

    USER: Jane
    PASSWORD: 2efb3
    JOIN-GAME
    GAME: g232a
    TRANSACTION: 2
    STOP

Since this is an open game, the server returns:

    1000 OK
    TRANSACTION: 2
    STOP

Since Jane's client now knows she has successfully joined the game, it
queries the initial game state to display a screen to her.

    USER: Jane
    PASSWORD: 2efb3
    TRANSACTION: 3
    QUERY-GAME-STATE
    STOP

Since the game begins immediately once the last player has joined,
the server then responds:

    1000 OK
    TRANSACTION: 3
    GAME-STATUS: BEGINNING
    HAND-STATUS: DEALING
    SEAT: 1
    GAME-POINTS: 0
    PLAYER: Johann
    SEAT: 2
    GAME-POINTS: 0
    PLAYER: Bob
    SEAT: 3
    GAME-POINTS: 0
    PLAYER: Hans
    SEAT: 4
    GAME-POINTS: 0
    PLAYER: Jacob
    SEAT: 5
    GAME-POINTS: 0
    PLAYER: Jane
    STOP

### User Management ###

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
