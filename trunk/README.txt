Yes, I am nineteen years old, and have written a complex,
over-engineered IRC bot.

If you got the source from Subversion, you'll need to type "make
version.ss" to create the file "version.ss".

To run the bot, try "./run-bot.ss --help".  

To do very crude code-coverage analysis, type "./tools/coverage.ss".

Some tests:

./run-all-tests.ss

You'll need mzscheme 371.3, which hasn't yet been released.  (Most of
this code will work fine with 370.6, but the "sandbox-eval" function
can hang due to a bug that was only fixed on the PLT trunk at revision
7445.)

I tested it against the dancer-ircd package that I found in Ubuntu
Ubuntu 7.04 "feisty": 1.0.36-7, and against irc.freenode.org, which is
probably also dancer-ircd.
