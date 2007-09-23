#!/bin/sh

(
    cat<<EOF
NICK rudebot
USER erich debian irc.freenode.org :Eric Hanchrow
PRIVMSG freenode-connect :VERSION a shell script
PRIVMSG NickServ :IDENTIFY fartulosity
EOF
sleep 2
echo PRIVMSG offby1 :hey you!
echo QUIT
    ) | nc irc.freenode.org  6667 -q 10
