#!/bin/bash -x

# Make sure, of course, that you have dancer-ircd running and
# listening on port 6667.  If you're on Debian, you can simply do
# "sudo aptitude install dancer-ircd".

rm -f sightings.db

./run-bot.ss -c '#yo' -n rudybot -s localhost &
sleep 10

(

    echo -e "NICK knack\\r"
    echo -e "USER knack unknown-host localhost :flood.sh, version whatever\\r"
    echo -e "JOIN #yo\\r"
    echo -e "PRIVMSG #yo :rudybot: eval (let ([x (make-string 1000 #\X)]) (values x x x x))\\r"

    while true
    do
        # if rudybot fails to respond to this, then the bug is present.
        echo -e "PRIVMSG #yo :rudybot: eval 'everything-is-ok\\r"
        sleep 20
    done

    ) | nc localhost 6667 
