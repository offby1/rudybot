#lang setup/infotab

(define compile-omit-paths '("incubot-tests.rkt" "sighting-server.rkt"))
(define deps '("base"
               "compatibility-lib"
               "db-lib"
               "plot-gui-lib"
               "plot-lib"
               "rackunit-lib"
               "sandbox-lib"
               "scheme-lib"
               "scribble-lib"
               "srfi-lite-lib"
               "unstable-debug-lib"))
(define build-deps '("at-exp-lib"))
