@doc Sheepshead - A Card Game [out=docs/00-sheepshead.md]
% Sheepshead - A Card Game
% Michael McDermott
# Sheepshead - A Card Game

## Introduction ##

Sheepshead is a German-American game played mostly in Wisconsin and
Minnesota. Outside of this geographic region, few people seem to
recognize it. As such, there are very few video game implementations
available and even fewer that run on good old Linux. The available
options are:

* Yahoo! Games. Maybe my reason for discounting it will sound like
  sour grapes, but I was unable to ever play a game. When I joined a
  table, the owner always booted me and when I opened a table, no one
  joined.
* schafKopf. This game has not been updated to KDE/Qt4 and does not
  seem to have any recent activity. Finally, when I went to the effort
  to dredge up KDE 3 libraries, it turned out to be quite different
  than the game I learned and loved.
* A couple of shareware options for Windows. Need I say more?

Our objectives are to create a Sheepshead game with the following
properties:

(#) The rules I grew up with (more on this later).
(#) Platform independence. At the very least, Windows and Linux. If a
Mac owner comes along, I would love to test for that platform as well.
(#) Core/UI separation. Eventually, multiple front ends would be nice.
In the immediacy, a simple Q/A-like console version will suffice.
(#) AI players.

This really does not seem like too much to ask for.

The rules we will be implementing can be found at [Pagat](http://www.pagat.com/schafk/shep.html).
The only major
caveat is that we will be implementing the Jack of Diamonds variant.

<!--- vim: set tw=75 ai: --->
