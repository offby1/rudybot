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

# Backups

I just started backing up the "big-log", which holds a more-or-less
raw transcript of all communication between the bot and the IRC
server.  (The file "corpus.db" is a sqlite db holding essentially the
same data; it can probably be recreated from the log via some simple
hacking.)

I do the backups with an hourly cron job that runs this command:

    aws s3 cp --quiet /mnt2/rudybot/big-log s3://rudybot-data-backups --region us-west-1

That's insanely inefficient, since it copies the same file over and
over (the file doesn't grow all that much in an hour), but perhaps
it's cheap enough.  We'll see.
