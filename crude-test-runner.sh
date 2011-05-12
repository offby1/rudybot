#!/bin/bash

# Sure be nice if there were a built-in way to run all the tests ...

egrep -l schemeunit *.rkt | while read f
do
    if [ -x $f ]
    then
        echo "-----------$f--------------------"
        case $f in
            log-parser.rkt)
                echo "Skipping $f cuz it's slow and destructive"
                ;;
            *)
                ./$f
                ;;
        esac
    fi
done

echo "I hope you were paying attention to see if any of those tests failed, 'cuz I sure as hell wasn't"
