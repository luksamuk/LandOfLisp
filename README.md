# Land of Lisp

<a href="http://www.wtfpl.net/"><img src="http://www.wtfpl.net/wp-content/uploads/2012/12/wtfpl-badge-1.png" width="57" height="20" alt="WTFPL"/></a>

## Introduction

This repository was created to track my progress  while reading and solving the
exercises of the book "Land of Lisp", by Conrad Barski.

Most of this code was derived from the book, with some comments I made as
reminders for better knowing the language and etc. I won't be including build
instructions nor anything, because there is just no point. To know what I'm
talking about on each file, take a look at the book.

One more thing to add is that <strong>I am using SBCL for this book,</strong>
therefore, this may imply that I changed something here and there, since
SBCL doesn't have CLISP's built-in extensions. For things such as streams,
for example, I had to use Quicklisp to aid me.

It's an excellent book for a lisper; you should definitely buy it.

## Important notes

Since everything was implemented under Emacs+SLIME using SBCL as default
implementation, it is obvious that not everything on the book will work
flawlessly, specially the web server implementation.

There are plenty of notes about it on the files, but it may be worth saying
it here once more.

The streams and web server implementations, which made heavy use of CLISP
extensions, were tweaked to use the `usocket` system.

The web server implementation, though, is not consistent and does not play
well with modern browsers, therefore I used `hunchentoot` in order to
serve and handle URLs on Dice of Doom.

Both of these systems can be obtained from *Quicklisp*, so if you're a newbie
with not much experience, you should definitely start by setting up Quicklisp
along with your favorite Common Lisp implementation.

Links to this awesome Quicklisp tool (by Zach Beane) and to `usocket` and
`hunchentoot` (both by Eitaro Fukamachi) documentations are available below.


## Relation of files

Files listed as per order of creation.

- `guess-my-number.lisp` (Chapter 2)
- `wizards-game.lisp` (Chapter 5)
- `graph-util.lisp` (Chapter 7)
- `grand-theft-wumpus.lisp` (Chapter 8)
- `datatypes-generics.lisp` (Chapter 9)
- `orc-battle.lisp` (Chapter 9)
- `loops.lisp` (Chapter 10)
- `evolution-game.lisp` (Chapter 10)
- `attack-of-the-robots.lisp` (Chapter 11)
- `streams.lisp` (Chapter 12)
- `web-server.lisp` (Chapter 13) [do not use!]
- `dice-of-doom.lisp` (Chapter 15)
- `macros.lisp` (Chapter 16)
- `svg.lisp` (Chapter 17)
- `wizards-game-v2.lisp` (Chapter 17)
- `lazy.lisp` (Chapter 18)
- `dice-of-doom-v2.lisp` (Chapter 18)
- `dice-of-doom-v3.lisp` (Chapter 19)
- `dice-of-doom-v4.lisp` (Chapter 20)


## Other links
- [SLIME features](https://www.cliki.net/SLIME%20Features)
- [Quicklisp](https://www.quicklisp.org/beta/)
- [usocket](http://quickdocs.org/usocket/)
- [hunchentoot](http://quickdocs.org/hunchentoot/)


[![Warning: Built Using Lisp](http://www.lisperati.com/lisplogo_warning2_256.png)](http://www.lisperati.com/logo.html)

