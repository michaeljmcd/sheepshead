@doc Command Line Client[out=docs/02-cli-client.md]
# Command-Line Client #

The first client will be a simple one, primarily designed to test the
server's capabilities. In this iteration, the client will accept no
commandline parameters and launch the user directly into an interactive
shell.

The interactive shell will allow the user to interact with a single server
at a time via a number of commands. Before launching into the details about
how we will implement this system, let us list out the commands that we
will need to connect to a server, join, and play games.

-----------------------------------------------------------------------
**Command Syntax**                  **Description**                
----------------                    -----------------------------------
`help` [*command*]                  Without an argument, `help` prints out
                                    a list of available commands. With an 
                                    argument, it provides an overview of
                                    the syntax.

`connect` *server*[:*port*]         Connects to the server indicated by the
                                    argument. A username and password, if
                                    required, will be prompted for.

`ls`                                Lists all games that the user owns, has
                                    been invited to, or are public.

`join-game` *game-id*               Attempts to join the game.

`take-blind`                        Takes the blind in the current game.

`pass-blind`                        Passes on taking the blind in the
                                    current game.

`play-card` *card*                  Plays the selected card. If the card is
                                    a simple number, it is assumed to be 
                                    the one-based index of the card as
                                    displayed. If it is not, it is assumed
                                    to be a text-based description or
                                    abbreviation. This will be discussed in
                                    more depth later.

`resign-game` *game-id*             Leaves the listed game early.

`score`                             Lists the score in the current game.

`begin-game` *game-name*            Starts a new game. The CLI may prompt
                                    for additional information.

`quit`                              Resigns any open games and terminates
                                    the command line shell.
-----------------------------------------------------------------------

## Startup ##

We sketch out our code as follows:

@code Command Line Client [out=src/cli-client.lisp,lang=commonlisp]
(in-package :sheepshead-cli-client)

@<Command Lookup Table>
@<REPL>
@=

## REPL ##

Our interface will be fairly standard: 

(#) Display a prompt.
(#) Accept and parse a command.
(#) Execute the proper action for the said command. In this case, it means
wrapping it up as a JSON request and shipping it off to the current server.
(#) If the command did not quit the program, go to #1.

The first and lasts of these tasks are easy. It is the middle two that can
lead us to some interesting places.

@code REPL
(defun sheepshead-repl (
@=

## The Commands ##

After parsing, we will need to execute the function that makes sense for
our particular state. In order to simplify this process, we will associate
each command with a function and store the result in a lookup table.

Each command's name shall be indicated as a string which shall be the key
to the function in a hash table.

@code Command Lookup Table
@=

## Packaging ##

As is our wont, we will provide separate package and ASDF system files.

@code Command Line Client Package [out=src/cli-client-package.lisp]
(defpackage #:sheepshead-consoleui
    (:use :cl :asdf))
@=

@code Command Line Client ASDF System [out=sheepshead-cli-client.asd]
(defsystem sheepshead-cli-client
    :version 0.1
    :author "Michael McDermott"
    :license "BSD"
    :description "A Console UI for Sheepshead"
    :pathname "src/"
    :components ((:file "cli-client-package")
        (:file "cli-client-package" :depends-on
            ("cli-client-package"))))
@=

<!--- vim: set tw=75 ai: --->
