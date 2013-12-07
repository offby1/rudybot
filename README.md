If you're just trying the bot out, start it via
``./freenode-main.rkt`` at the shell (simply doing ``racket
freenode-main.rkt`` will fail mysteriously).  If you want it to run
continuously, and happen to have [upstart](http://upstart.ubuntu.com/)
available (which in practice means you're running Ubuntu), you can
copy ``rudybot.conf`` to ``/etc/init`` and then ``# start rudybot``.

Getting an error about ``rackunit`` not being available?  That can be
caused by using the ``racket-textual`` package instead of ``racket``.

Run the tests like this:

    $ raco test -x .

Unfortunately, you'll have to pay attention to the output of ``raco
test``, since it will likely exit with 0 status even if some of the
tests fail.  This is IMHO a misfeature of ``raco test``; the Racket
developers seem disinclined to change it (see
[this bug report](http://bugs.racket-lang.org/query/?cmd=view&pr=13573)).

# Some specs:

- [rfc1459][]
- [ctcpspec][]

[rfc1459]: http://tools.ietf.org/html/rfc1459
[ctcpspec]: http://www.irchelp.org/irchelp/rfc/ctcpspec.html
