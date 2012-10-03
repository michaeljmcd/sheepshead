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

#### Command Listing ####

`list-games`

:   Lists all active games that the current user has access to.

    **Additional Parameters**

    *None*.

`create-game`

:   Creates a new game on the server.

    **Additional Parameters**

    (#) `name`--an optional user-visible name.
    (#) `seat-count`--the number of slots available.
    (#) `goal-points`--the number of points to play to.

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

    The result will include either a user ID if the creation was successful or a

#### Examples ####

In order to make the protocol more clear, let's look at some examples.

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
    }

### Multiuser ###

All users will be managed by the server.

@code User Declarations [lang=commonlisp]
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
