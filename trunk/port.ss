#! /bin/sh
#| Hey Emacs, this is -*-scheme-*- code!
#$Id$
exec mzscheme -M errortrace --no-init-file --mute-banner --version --require "$0" -p "text-ui.ss" "schematics" "schemeunit.plt" -e "(exit (test/text-ui port-tests 'verbose))"
|#
(module port mzscheme
(require (only (planet "port.ss" ("schematics" "port.plt" ))
               port->string))

(define (port->string/close ip)
  (begin0
    (port->string ip)
    (close-input-port ip)))

(provide (all-defined))
)
