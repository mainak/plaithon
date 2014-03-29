#lang plai-typed

(require "python-core-syntax.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

(define debug-lib #f)

(define-type-alias Lib (ExprC -> ExprC))

(define-type LibBinding
  [bind (left : symbol) (right : ExprC)])

(define lib-functions
  empty)

(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (LetC name value
                                 (python-lib/recur (rest libs)))))]))]
         (python-lib/recur lib-functions)))
