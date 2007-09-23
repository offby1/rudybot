#!/bin/bash -x

(

    echo -e ":localhost. 001 rudybot :Welcome to the faked-up shell script rudybot \\r"
    echo -e ":localhost 366 rudybot #scheme-bots :what's up, homes? \\r"
    echo -e ":localhost 366 rudybot #emacs :what's up, homes? \\r"

    echo -e ":a!b@c PRIVMSG #scheme-bots :hey buddy\\r"
    echo -e ":a!b@c PRIVMSG #scheme-bots :rudybot: hey buddy\\r"
    echo -e ":a!b@c PRIVMSG rudybot :hey buddy\\r"
    echo -e ":a!b@c PRIVMSG #scheme-bots :rudybot: seen a?\\r"
    echo -e ":a!b@c PRIVMSG #scheme-bots :rudybot: uptime\\r"

    sleep 10

    echo -e ":a!b@c PRIVMSG #scheme-bots :rudybot: news\\r"
    echo -e ":a!b@c PRIVMSG #scheme-bots :rudybot: quote\\r"
    echo -e ":a!b@c PRIVMSG rudybot :\001VERSION\001\\r"
    echo -e ":a!b@c PRIVMSG rudybot :what ho, my good man\\r"

    sleep 10

#     while true
#     do
#         sleep 120
#     done

    ) | ./run-bot.ss -c '#scheme-bots' -n rudybot
