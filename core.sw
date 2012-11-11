@doc Sheepshead Server [out=docs/01-core.md]
## Core ##

This is where we model the basic elements of the game, including the
game itself. The interface should do little more than advance and
display the game in its own fashion. 

### A Nice Game of Cards ###

We will start at the bottom and build up. What is the most fundamental
thing in a card game? The cards. You cannot play without them, either
digitally or physically. The deck in Sheepshead is A-7 (i.e. 2-6s are
discarded) of each suit (assuming a French deck: clubs, spades,
hearts, and diamonds).

To ensure that the macro definitions predate any usage for them, we
will place them immediately after the the `in-package` statement.

@code Data Structures [out=src/core.lisp,lang=commonlisp]
(in-package :sheepshead)

@<core macros>

(defconstant +LOWCARD+ 7)
(defconstant +HIGHCARD+ 14)
(defconstant +VERSION+ 0.1)
(defconstant +MINPLAYERS+ 3)
(defconstant +MAXPLAYERS+ 5)
(defconstant +DECKSIZE+ 32)

(deftype suit
    '(member :clubs :spades :hearts :diamonds))

(defclass card ()
    ((rank :accessor rank :initarg :rank)
     (suit :accessor suit :initarg :suit :type suit)))
@=

Suits are assumed to be members of `'(:clubs :spades :hearts
:diamonds)` as indicated by the type `suit`.
Rank must be between 7 and 14, inclusive. The card
corresponding to each number will start at 7 and follow the Germanic
ordering that Sheepshead uses. So, the following numbers would
correspond to the following cards:

**Card constants**

Constant    Card
--------    ------
7           7
8           8
9           9
10          King
11          10
12          Ace
13          Jack
14          Queen

The ordering may seem a little strange, but it should make sense with
knowledge of the rules because it allows rank comparison to be done
purely as `rank1 > rank2`. 

We will add an implementation of `print-object` (See section 28.2 of
Common Lisp the Language; the equivalent of overriding C#'s ToString()
method). Clearly, this will not suffice for the UI, but it will make
things much, much easier when debugging interactively.

@code Data Structures [lang=commonlisp]
(defmethod print-object ((card card) stream)
    (let ((name (case (rank card)
                    (7 "7")
                    (8 "8")
                    (9 "9")
                    (10 "KING")
                    (11 "10")
                    (12 "ACE")
                    (13 "JACK")
                    (14 "QUEEN"))))
    (format stream "[~A OF ~A]" name (suit card))))
@=

This is little better than a C-struct in its current form. We have not
even bound the members. That is all right though. We will write a
function to generate a Sheepshead deck. The method used will be
extremely straightforward. For each suit, we will generate each of the
cards in the range 7-14. 

@code Data Structures [lang=commonlisp]
(defmethod generate-deck ()
    (let ((deck (make-list +DECKSIZE+ :initial-element nil))
          (index 0))
        (dolist (suit '(:clubs :spades :hearts :diamonds))
            (loop for rank from +LOWCARD+ upto +HIGHCARD+
                do 
                (setf (nth index deck) (make-instance 'card :rank rank :suit suit))
                (setf index (1+ index))
                ))
         deck
    ))
@=

We will provide another function, this one to shuffle the deck. The
algorithm used will be the [Fisher-Yates Algorithm](http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle).
From the above source, the pseudocode is:

    To shuffle an array a of n elements:
       for i from n - 1 downto 1 do
             j <- random integer with 0 <= j <= i
             exchange a[j] and a[i]

Our function will operate on an array, for efficiency's sake.
Accessing an array, in lisp, is constant time, but accessing the Nth
element of a list is O(*n*) time. So, to implement the above algorithm:

@code Data Structures [lang=commonlisp]
(defmethod shuffle-deck ((deck list))
    (loop for index from (1- +DECKSIZE+) downto 1
        do
            (let* ((random-index (random index))
                   (temp (nth random-index deck)))
                (setf (nth random-index deck) (nth index deck))
                (setf (nth index deck) temp)
                ))
     deck)
@=

### Players ###

With a shuffled pack of cards, the next step is to deal. To deal, we
need to have a game on, and to have a digital game on, we need to
model the game. So, let us do that.

Any player, regardless of whether they are human or AI controlled will have
a name, a hand, a flag indicating whether or not that player is currently
the dealer, and a score (the game's running score, not the points scored in
a single hand).

@code Data Structures [lang=commonlisp]
(defclass player ()
    ((name :accessor name :initarg :name :initform "Foo")
     (hand :accessor hand :initarg :hand :initform nil)
     (dealer? :accessor dealer? :initarg :dealer? :initform nil)
     (score :accessor score :initarg :score :initform 0)
    ))
@=

### Games ###

We have cards and players. It is time to put it all together and play
a game. Sheepshead game goes through the following phases:

(#)   Play a hand:
    (#)  Pass deal to the left.
    (#)  The cards are dealt to each player.
    (#)  Starting at the dealer's left, each player is given the chance to
take the blind.
    (#)  If a player takes the blind, he may bury a number of cards
equivalent to the size of the blind..
    (#)  Tricks begin. The first trick is opened by the player to the
dealer's left.
    (#)  Once all tricks have been played, the number of points is tallied
for each team.
(#)  If the provided number of game points has not been reached, go to
step #1.

@code Data Structures [lang=commonlisp]
(defclass game ()
    ((players :initarg :players :accessor players :initform nil)
     (goal-points :initarg :goal-points :accessor goal-points :initform 0)
     (hands :initarg :hands :accessor hands :initform nil)
     ))
@=

A game, then, can be simulated with a very simple method. We will
first check the number of players. While some people claim that you
can play Sheepshead with less than three or more than five players,
they are wrong--they are heretics, may they be burnt at the stake. A
valid game will have 3-5 players, and will play the Jack of Diamonds
variant when five are playing.

The top level game, then, will follow this basic format:

(#)  Verify that the number of players provided is accurate.
(#)  Play a hand.
(#)  Compare scores. If the game is over, return the winning player. If
not, go back to #1.

Items 2-3 of the above list find their implementation here. Pretty
much, we indefinitely play hands until the game's target has been
reached. A target number of points of 0 indicates that the game has no
preplanned end, even though all good things must come to an end. 

@code Data Structures [lang=commonlisp]
(defmethod play-game ((game game))
    (when-player-count (length (players game))
        (do ()
            ((game-over? game))
            (play-hand game))))
@=

We noted the conditions for a game's ending above. A succinct way of
stating the requirement is that if the target number of points for the
game is not 0 (i.e. the game is infinite) and at least one player has
as many or more points than the target, the game is over. We express
this succinctly in the method `game-over?`.

@code Data Structures [lang=commonlisp]
(defmethod game-over? ((game game))
    (if (eq (goal-points game) 0)
        nil
        (reduce #'(lambda (x y) (or x y))
                (mapcar #'(lambda (player) 
                            (>= (score player) (goal-points game)))
                        (players game))
                :initial-value nil)
    ))
@=

The basic structure of a game is now intact. The next step is to
implement the various stages pursuant of playing a hand in the
imaginatively named method `play-hand`.

First, we deal the cards and find out who (if anyone) will take the
blind. Then, we begin the main loop.

@code Data Structures [lang=commonlisp]
(defmethod play-hand ((game game))
    (setf (players game) (rotate-left (players game)))
    (setf (dealer? (first (players game))) t)

    (let* ((blind (deal-hands (players game)))
           (taker (take-round (rotate-left (players game)))))
            (unless (null taker)
                (bury taker blind)))
    (play-tricks (reverse (players game))))
@=

The next thing to do is deal with trick-playing. Let's begin by laying
out the properties of this:

1. We have to continue until there are no cards in any player's hand.
1. The number of rounds will be equal to the number of cards in a given
player's hand.
1. The ranking of cards is affected by the first card played (i.e.
which suit is lead), so that information will have to be maintained.
1. The player to win a given trick `n` starts trick `n + 1` for any
trick `> 0`.

For #1, the obvious method is to iterate from 0 to the number of cards
in a hand. Recursion would work, but is less readable in this instance
than using Lisp's `loop` facility.

For each round, we begin with the first player and prompt each player
for a card, passing in the set of cards played so far.

After each player has played, we determine the winner, add the trick
to their tricks won, and rotate the list of players to match. We can
lay this out as follows:

@code Data Structures [lang=commonlisp]
(defun play-tricks (players)
    (loop for index from 0 to (hand-size (length players))
          do (play-trick players nil nil)))
@=

The heart of this function is `play-trick`, so we will define it next.

@code Data Structures [lang=commonlisp]
(defun play-trick (remaining-players cards winning-player)
    (if (null remaining-players)
        (values (reverse cards) winning-player)
        (let ((next-card (play-card (car remaining-players) cards)))
            (if (leading-card? next-card cards)
                (play-trick (cdr remaining-players) 
                            (cons next-card cards)
                            (car remaining-players))
                (play-trick (cdr remaining-players)
                            (cons next-card cards)
                            winning-player))
                            )))
@=

As we can see, `play-trick` is a simple recursive function that takes
a card from each player, then returns the set of cards played in the
trick and the winning player. The latter is accomplished primarily
through the use of the function `leading-card?` which determines if a
new card will be the leading one of those played so far.

We can define the conditions when this will be true as follows:

1. It is the first card.
1. It is of the same suit as the first card, but the highest of that
suit to be played so far and no other trump have been played so far.
1. It is a trump card and the highest such card played so far.

As a study of contrasts, we can examine the conditions under which the
result should be false:

1. The card is neither trump nor the leading suit.
1. The card is of the leading suit, but less so than another card
played or a trump card has been played.
1. The card is trump, but of lesser rank than a trump played so far.

We will entitle the two parameters being passed in `new-card` and
`previous-cards`, respectively. Their purposes should be fairly
clear. Now, let us define in more concrete terms the method used:

1. If `previous-cards` is `nil`, return true.
1. If a trump card has been played:
    1. and the card is not a trump, return false.
    1. and the card is the highest trump card played, return true.
1. If no trump has been played:
    1. and the card is a trump, return true.
    1. and the card is not of the leading suit, return false.
    1. and the card is of the leading suit, return a simple test.

Most of the items in that list are pretty simple to determine,
particularly with the utilities that we will see a little bit later,
the only two that take a little more thought are items 2b and 3c. It
turns out that 2b and 3c are mirrors of one another in that, in both
cases, the current card is a member of the set in which the leading
card already is. To handle this, we will look at a function later
that, given a set of cards and a class to look for (a suit or trump),
will find the highest card of that class in the set.

@code Data Structures [lang=commonlisp]
(defun leading-card? (new-card previous-cards)
    (if (null previous-cards)
        t
        (let ((first-card (nth 0 previous-cards)))
            (if (contains-trump? previous-cards)
                (cond 
                    ((and (trump? new-card)
                          (> (rank new-card) 
                             (max-card-in-set previous-cards :trump)))
                          t)
                     (t nil))
                (cond
                    ((trump? new-card) t)
                    ((not (eq (suit new-card) (suit first-card))) nil)
                    ((and (eq (suit new-card) (suit first-card)))
                          (> (rank new-card)
                             (max-card-in-set previous-cards 
                                              (suit first-card)))
                          t)
                    (t nil))
            ))
    ))
@=

As a convenience, we will define a function determining whether or not
a trump has been played in a list of cards. This function will be
entitled `contains-trump?`. A trump card in Sheepshead is defined as a
card that is one or more of the following:

1. A Jack.
1. A Queen.
1. A Diamond.

Thus, we can define `trump?`:

@code Data Structures [lang=commonlisp]
(defun trump? (card)
    (or (eq (suit card) :diamonds)
        (>= (rank card) 13)))
@=

The second clause is readily explained in section 1.1. With this in
hand, we can define `contains-trump?`:

@code Data Structures [lang=commonlisp]
(defun contains-trump? (cards)
    (reduce #'(lambda (x y) (and x y))
        cards :initial-value t))
@=

It is also important to decide whether or not a given card is valid.
TODO

1. Select the set. If we are talking about clubs played, where clubs is
the leading suit without trumps, then clubs. If trump, then trump.
1. Find the maximum card in that set.
1. Compare its rank to the rank of `new-card`.

@code Data Structures [lang=commonlisp]
(defun max-card-in-set (cards discriminator)
    (let ((filter-fn (if (eq discriminator :trump)
                        (lambda (x) (or (eq (suit x) :diamond)
                                        (>= (rank x) 12)))
                        (lambda (x) (eq (suit x) discriminator))
                        )))
        (reduce #'max
            (mapcar #'rank 
                (remove-if #'(lambda (x) (not (funcall filter-fn x)))
                           cards))
            :initial-value 0)
    ))
@=

#### The Deal ####

The function `deal-hands` generates a new deck, shuffles it, and deals
a hand to each player. It returns the blind and assumes that the first
player listed is the dealer. `deal-hands` must do the following
things:

1. Generate a deck.

1. Shuffle the deck.

1. Initialize each player's hand to be an empty list of the proper
size.

1. Deal all of the cards out to the players and blind.

1. Return the blind.

The reason that the players need not be returned is that they will be
modified during the function call.

@code Data Structures [lang=commonlisp]
(defun deal-hands (players)
    (let ((deck (shuffle-deck (generate-deck)))
          (blind nil)
          (hand-size (hand-size (length players)))
          (blind-size (blind-size (length players))))
        (dolist (player players)
            (setf (hand player) nil))
        (dotimes (index hand-size)
            (unless (>= (length blind) blind-size)
                (push (pop deck) blind))
            (dolist (player players)
                (push (pop deck) (hand player))
                ))
        blind
        ))
@=

#### The Auction ####

I do not really know that auction is the correct term here. To be
sure, I do not even know what the correct term might be, but since
this word is used for the equivalent phase of the game in Bridge and
Skat, it seemed as good a choice as any.

The auction, as it is, consists of giving each player the opportunity
to take the blind, starting with the first player from the dealer's
left. We will simulate this phase in the function `take-round`. The
function itself will not be responsible for determining whether or not
a given player will take the blind. Rather, this responsibility will
be delegated to the `take-blind?` method of the player.

If a player takes the blind, their `role` slot will be updated
accordingly, and they will be returned. If no one takes the blind,
`nil` will be returned.

@code Data Structures [lang=commonlisp]
(defun take-round (players)
    (if (null players)
        nil
        (let ((decision (take-blind? (car players))))
            (if decision
                (car players)
                (take-round (cdr players))
                ))
    ))
@=

#### Burying ####

The player who takes the blind may bury two cards (as points) up
front. As with other decision making processes, the precise method
used is delegated to the specific subclass of `player` and its `bury`
method.

#### Tricks ####

Once the taking has or has not occurred, the main portion of the game
starts. The player to the dealer's left leads off the first trick and
the winner of each trick thereafter begins the next.

#### Scoring ####

The scoring happens on two levels, each occurring after the end of the
hand. Those two levels are scoring the hand (and, by extension,
determining the winner(s)) and scoring the game.

### Utilities ###

The blind size, as indicated above, varies with the number of players
in the game. The rule of thumb is that it is the number of cards
remaining after all the players have been dealt the maximum number of
cards while having equal sized hands. Since there are only three
values, it is far easier to drop them somewhere (a function, in this
case) than it is to manually recalculate them. The table looks like
this:

**Blind sizes**

Players     Cards in the blind
-------     ------------------
3           2
4           4
5           2

The only other thing to consider is when the number of players
provided is invalid. In this case, we signal an error.

@code Data Structures [lang=commonlisp]
(defun blind-size (player-count)
    (when-player-count player-count
        (case player-count
            (3 2)
            (4 4)
            (5 2))
    ))
@=

Another comparable function is `hand-size`, which also takes a single
integer parameter indicating the number of players.

@code Data Structures [lang=commonlisp]
(defun hand-size (player-count)
    (when-player-count player-count
        (case player-count
            (3 10)
            (4 7)
            (5 6))
        ))
@=

In several places, we perform an action only if the number of players
is accurate. This is done to prevent any strange bugs from either
conscious manipulation of the gamestate or from errors in the front
end. Because we use this several times, we will wrap it up in a nice
little macro.

@code core macros [lang=commonlisp]
(defmacro when-player-count (count &body body)
    `(if (valid-player-count? ,count)
        (progn ,@@body)
        (error "There must be 3-5 players. ~A provided." ,count)
        ))
@=

Often, we find ourselves checking the number of players. A quick
little utility function makes this much easier. `valid-player-count?`
ensures that the number passed to it is:

1. An integer.
1. Between the minimum number of players (3) and the maximum number of
players (5) inclusive.

@code Data Structures [lang=commonlisp]
(defun valid-player-count? (player-count)
    (and (integerp player-count)
         (>= player-count +MINPLAYERS+)
         (<= player-count +MAXPLAYERS+)))
@=

As seen above, we use rotation a great deal. That is because in a real
card game, the same things rotate. The definitions are here
(google-inspired, to be sure):

@code Data Structures [lang=commonlisp]
(defun rotate-left (some-list)
    (concatenate 'list (cdr some-list) (list (car some-list))))

(defun rotate-right (some-list)
    (concatenate 'list (last some-list) (butlast some-list)))
@=

### Packaging ###

Like most Lisp systems, we will pacakge Sheepshead with ASDF. The
package should be straightforward.

First, the package file:

@code Core Package File [out=src/sheepshead-package.lisp,lang=commonlisp]
(defpackage #:sheepshead
    (:export game player human-player card hand name players
             play-callback bury-callback blind-callback simple-bot-player
             play-game blind-size when-player-count
             +VERSION+ +MINPLAYERS+ +MAXPLAYERS+
             valid-player-count?)
    (:use :cl :asdf))
@=

And the ASDF file:

@code Core ASDF Package [out=sheepshead.asd,lang=commonlisp]
(defsystem sheepshead
    :version "0.1"
    :author "Michael McDermott"
    :license "BSD"
    :description "Sheepshead"
    :pathname "src/"
    :components ((:file "sheepshead-package") 
                 (:file "core" :depends-on ("sheepshead-package"))
                 ))
@=


