@doc Command Line Client[out=docs/03-cli-client.md]
## Command-Line Client ##

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

### Startup ###

We sketch out our code as follows:

@code Command Line Client [out=src/cli-client.lisp,lang=commonlisp]
(in-package :sheepshead-cli-client)

@<Command Lookup Table>
@<Print banner>
@<REPL>
@=

To identify ourselves, we will print a nice little welcome banner. As
gaudy as you can get in plain text.

@code Print banner [lang=commonlisp]
(format *standard-output*
    "Sheepshead version ~A~%Copyright 2010 by Michael McDermott~%~%"
    sheepshead:+VERSION+)
@=

### REPL ###

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

#### Prompt Utilities ####

The function `get-integer` is separated out to keep the code nice and neat.
It doesn't really do anything surprising, just print out a prompt, read in
the data, and verify that it is valid. If the input read in is not a valid
integer, the prompt will be repeated until a valid answer is given.

The remaining parameters are key-based, the first one being `test-fn`. This
is a function to determine if the number is valid, for whatever purpose is
intended. This is as opposed to if it is an integer. If `test-fn` is
provided and returns `nil`, the prompt will be made again.

@code utility functions [lang=commonlisp]
(defun get-integer (prompt &key (test-fn nil))
    (flet ((valid-input (input) 
                (let ((parsed-output (parse-integer input :junk-allowed t)))
                    (and parsed-output
                        (or (null test-fn) 
                            (funcall test-fn parsed-output))))

                ))
    (let ((input ""))
        (loop while (not (valid-input input))
            do
            (format *standard-output* prompt)
            (force-output *standard-output*)
            (setf input (read-line *standard-input*)))
        (parse-integer input :junk-allowed t))
    ))
@=

Sometimes, we will want to get a simple yes or no. Along those lines,
we will build a simple function to prompt for it.

@code utility functions [lang=commonlisp]
(defun get-boolean (prompt)
    (flet ((valid-input (input)
             (member-if #'(lambda (x) (equal x (string-downcase input))) 
                        '("y" "yes" "n" "no"))))
        (let ((input ""))
                (loop while (not (valid-input input))
                    do
                    (format *standard-output* prompt)
                    (force-output *standard-output*)
                    (setf input (string-downcase (read-line *standard-input*))))
                (if (or (equal input "y")
                        (equal input "yes"))
                    t
                    nil))
    ))
@=

The next function is a convenience, to keep the initial set up code
clean. It displays a prompt, reads a string and returns it.

@code utility functions [lang=commonlisp]
(defun get-player-name (player-number)
    (get-string 
        (format nil "~%Enter a name for player #~A " (1+ player-number))
        :default-value (format nil "Player ~A" player-number)))
@=

The heart of the function is yet another one that we will be using in
future areas to prompt for input, is a simple little function to get a
string named `get-string`.

@code utility functions [lang=commonlisp]
(defun get-string (prompt &key (default-value nil))
    (format *standard-output* prompt)
    (let ((input (read-line)))
        (if (equalp input "")
            default-value
            input)
        ))
@=

### The Commands ###

After parsing, we will need to execute the function that makes sense for
our particular state. In order to simplify this process, we will associate
each command with a function and store the result in a lookup table.

Each command's name shall be indicated as a string which shall be the key
to the function in a hash table.

@code Command Lookup Table
@=

### Packaging ###

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
