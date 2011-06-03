#lang racket/base

(require scheme/sandbox)

;; Convenient utility
(define-syntax-rule (in-sandbox e body ...)
  (call-in-sandbox-context e (lambda () body ...)))

;; A subtle point here is memory that is accessible from the sandbox:
;; the value shouldn't be accessible outside the originating sandbox to
;; prevent this from being a security hole (use `give' to avoid being
;; charged for the allocated memory).  Solve this by registering the
;; value with a gensym handle in the sending sandbox's namespace, and
;; make the handle accessible in the other sandbox.  The handle is
;; available in the receiving sandbox and weakly held in the giving
;; sandbox, so if the receiver dies the handle can be GCed and with it
;; the value.
(define given-handles   (gensym 'given-values))
(define received-handle (gensym 'received-values))
(define (sandbox->given-registry e)
  (in-sandbox e
    (namespace-variable-value given-handles #f
      (lambda ()
        (let ([t (make-weak-hasheq)])
          (namespace-set-variable-value! given-handles t)
          t)))))

(define (give from to expr)
  ;; Evaluate the expression (all the usual things apply: should catch
  ;; errors, and require a single value too). See above for an
  ;; explanation for the handle.
  (define val (from expr))
  (define handle (gensym 'given))
  (hash-set! (sandbox->given-registry from) handle val)
  (in-sandbox to
    (namespace-set-variable-value! 'grab
      (lambda ()
        (if (evaluator-alive? from)
          ;; note: this could be replaced with `val' -- but then this
          ;; closure will keep a reference for the value, making it
          ;; available from the receiving thread!
          (hash-ref (sandbox->given-registry from) handle
            (lambda ()
              (error 'take "internal error (the value disappeared)")))
          (error 'take "the sending evaluator died")))))
  ;; should use the sender's name in the message
  (printf "~a: to grab the value, use `(grab)'\n" to))

;; example
(define eli    (make-evaluator 'scheme))
(define offby1 (make-evaluator 'scheme))
(eli "(define money 1000)
      (define (withdraw n) (set! money (- money n)))
      (define (make-check n)
        (let ([cashed? #f])
          (lambda ()
            (if cashed?
              (error \"THIEF!!!\")
              (begin (set! cashed? #t) (withdraw n))))))")
(give eli offby1 "(make-check 100)")
(offby1 "((grab))")
(eli "money") ; now has 900 moneys

;; ----------
;; Note:

;; Each sandbox lives in its own world, for example:
(eli "(define p (delay 123))
      (promise? p)") ; yes, it's a promise
(give eli offby1 "p")
(offby1 "(grab)") ;; it prints like a promise
(offby1 "(promise? (grab))") ;; but it's not a promise in this world
#|
This is very similar to both sandboxes evaluating

  (define-struct foo (blah))

if a (make-foo 123) value is sent across, the other side will get #f
for (foo? (grab)) because it's a different struct.

The above particular problem can be solved by using

  (parameterize ([sandbox-namespace-specs
                  (list make-base-namespace 'scheme)])
    (make-evaluator 'scheme))

which will create a sandbox that shares the toplevel `scheme' module
(it will also require less memeory overall, since all sandboxes will
share state that is part of the `scheme' module).  But note that structs
defined by other required modules will have the same problem (for
example, swindle objects).
|#

#|
<eli> In that sketch things are easy because I just use sandboxes instead of nicks.
<eli> (which might be a slight problem: if I give something to `foo', it would be better to not
      create a sanndbox for `foo', but that's technicalities)                              [19:06]
<eli> Anyway,
<eli> When someone gets something, it writes a `grab' binding.  There is therefore exactly one
      value that can wait for you.  Seems fine for irc-style interactions.                 [19:07]
<eli> So in your own sandbox `(grab)' will actually run the function that is created outside the
      sandbox, and that will drag over the value that you were given.
<eli> So the interaction would be:                                                         [19:08]
<eli> rudybot: give foo some-expr
<eli> and then foo will:
<eli> rudybot: eval (define blah (grab))
<rudybot> eli: error: reference to undefined identifier: grab
<eli> The implementation is slightly tricky: the whole point is thta if I give you a value, the
      value is not accessible outside of my sandbox.                                       [19:09]
<eli> This will be a memory leak -- allowing people to grab memory and throw the responsibility
      away.
<eli> So the solution is to keep the value inside my sandbox,                              [19:10]
<eli> it's stored as a hash value for a random gensymmed handle -- and that handle goes to my
      sandbox
<eli> (or goes from the giver to the taker)
<eli> only when the taker invokes (grab) it will grab the value from the giver's sandbox, and use
      that as the result, and then the taker can be charged for it too, which is file.     [19:11]
<eli> That's all.
<offby1> I will amend your sketch with what you just said.                                 [19:19]
<eli> OK
<eli> How about when I wake up you ask me more question,
<eli> I'll say some more stuff,
<eli> and you'll add that too -- so you can call it the second amendment.                  [19:20]
|#
