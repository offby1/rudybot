Yes, I am nineteen years old, and have written a complex,
over-engineered IRC bot.

If you got the source from Subversion, you'll need to type "make
version.ss" to create the file "version.ss".

To run the bot, try "./run-bot.ss --help".  

To do very crude code-coverage analysis, type "./tools/coverage.ss".

Some tests:

./run-all-tests.ss

I developed it using bleeding-edge versions of PLT scheme (370.6),
starting July 2007.  I know that it doesn't work with Version 352; I
don't know about versions in between those.

I tested it against the dancer-ircd package that I found in Ubuntu
Ubuntu 7.04 "feisty": 1.0.36-7, and against irc.freenode.org, which is
probably also dancer-ircd.
