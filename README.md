If you're just trying the bot out, start it via ``racket
freenode-main.rkt``.  If you want it to run continuously, and happen
to have [upstart](http://upstart.ubuntu.com/) available (which in
practice means you're running Ubuntu), you can copy ``rudybot.conf``
to ``/etc/init`` and then ``# start rudybot``.

Getting an error about ``rackunit`` not being available?  That can be
caused by using the ``racket-textual`` package instead of ``racket``.

Run the tests like this:

    $ raco test -x .
    
# Some specs:

- [rfc1459][]
- [ctcpspec][]

[rfc1459]: http://tools.ietf.org/html/rfc1459
[ctcpspec]: http://www.irchelp.org/irchelp/rfc/ctcpspec.html
