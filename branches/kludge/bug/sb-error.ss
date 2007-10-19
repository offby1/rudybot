(require (lib "sandbox.ss"))
(define e (make-evaluator 'mzscheme '(begin) '()))
(e "(require (lib \"1.ss\" \"srfi\"))")

;; $ mzscheme -qr bug/sb-error.ss
;; /usr/local/stow/plt/lib/plt/collects/mzlib/private/stxset.ss:24:37: compile: unbound variable in module (transformer environment) in: generate-expand-context

;;  === context ===
;; /usr/local/stow/plt/lib/plt/collects/mzlib/sandbox.ss:198:11
;; /usr/local/stow/plt/lib/plt/collects/mzlib/sandbox.ss:182:2: call-with-limits
;; /usr/local/stow/plt/lib/plt/collects/mzlib/sandbox.ss:419:6: loop
