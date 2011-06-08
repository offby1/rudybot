#!/bin/bash

# Sure be nice if there were a built-in way to run all the tests ...

(echo incubot.rkt; egrep -l '(scheme|rack)unit' *.rkt) | while read f
do
    if [ -x $f ]
    then
        echo "-----------$f--------------------"
        ./$f
    fi
done

echo "I hope you were paying attention to see if any of those tests failed, 'cuz I sure as hell wasn't"
