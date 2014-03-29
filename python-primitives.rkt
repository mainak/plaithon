#lang plai-typed

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

(require (typed-in racket/base
                   [display : (string -> void)]
                   [string<? : (string string -> boolean)]
                   [string>? : (string string -> boolean)]
                   [char<? : (char char -> boolean)]
                   [char>? : (char char -> boolean)]
                   [abs : (number -> number)]
                   [bitwise-not : (number -> number)]
                   [bitwise-and : (number number -> number)]
                   [bitwise-ior : (number number -> number)]
                   [bitwise-xor : (number number -> number)])

         "python-core-syntax.rkt"
         "python-base.rkt"
         "python-bootstrap.rkt")

(define debug-prim 0)

(define (issubclass/impl [cls : Location] [super : Location]) : Location
  (begin
    (if (> debug-prim 2)
        (display
         (concat
          (list
           "issubclass/impl: " (pretty cls) " "
           (pretty super) "\n")))
        (void))
    (begin
      (if (> debug-prim 2)
          (display
           (concat
            (list
             "issubclass/impl (bases): "
             (pretty (some-v (hsh-get (get-dict cls) (obj-str "__mro__"))))
             "\n")))
          (void))
      (type-case (optionof Location) (hsh-get (get-dict cls) (obj-str "__mro__"))
        [none () (get-singleton-object 'False)]
        [some (bases-loc)
              (type-case ObjV (fetch-object bases-loc)
                [ListV (cl d f bases)
                       (if (foldl (lambda (c res)
                                    (or (= c super) res))
                                  #f (unbox bases))
                           (get-singleton-object 'True)
                           (get-singleton-object 'False))]
                [else (error 'issubclass/impl "Something terrible. __mro__ is not a tuple!!")])]))))

(define (isinstance/impl [obj : Location] [cls : Location]) : Location
  (issubclass/impl (get-class obj) cls))

(define (print arg)
  (display (pretty arg)))

(define (get-truth [loc : Location]) : Location
  (cond
   [(= loc (get-singleton-object 'None)) (get-singleton-object 'False)]
   [(= loc (get-singleton-object 'False)) (get-singleton-object 'False)]
   [(= loc (get-singleton-object 'True)) (get-singleton-object 'True)]
   [(= loc (get-singleton-object 'Ellipsis)) (get-singleton-object 'True)]
   [else
    (type-case ObjV (fetch-object loc)
      [StrV (c f s) (if (equal? s "") (get-singleton-object 'False) (get-singleton-object 'True))]
      [NumV (c f n) (if (= n 0) (get-singleton-object 'False) (get-singleton-object 'True))]
      [ListV (c f fr? l) (if (= (length (unbox l)) 0)
                             (get-singleton-object 'False)
                             (get-singleton-object 'True))]
      [DictV (c f fr? d) (if (= (length (unbox d)) 0)
                             (get-singleton-object 'False)
                             (get-singleton-object 'True))]
      [else (get-singleton-object 'True)])]))

(define (not_ [loc : Location]) : AnswerC
  (if (= (get-truth loc) (get-singleton-object 'True))
      (ValueA (get-singleton-object 'False))
      (ValueA (get-singleton-object 'True))))

(define (truth [loc : Location]) : AnswerC
  (ValueA (get-truth loc)))

(define (abs_ [loc : Location]) : AnswerC
  (type-case ObjV (fetch-object loc)
    [NumV (c d n) (ValueA (store-object (NumV
                                         (if (= c (lookup/boot 'bool)) (lookup/boot 'int) c)
                                         (box empty) (abs n))))]
    [else (interp-error 'TypeError "abs not defined for non-numeric types")]))

(define (bitwise-not_ [loc : Location]) : AnswerC
  (type-case ObjV (fetch-object loc)
    [NumV (c d n) (ValueA (store-object (NumV
                                         (if (= c (lookup/boot 'bool)) (lookup/boot 'int) c)
                                         (box empty) (bitwise-not n))))]
    [else (interp-error 'TypeError "abs not defined for non-numeric types")]))

(define (add [l : Location] [r : Location]) : AnswerC
  (let ([l-val (fetch-object l)]
        [r-val (fetch-object r)])
    (type-case ObjV l-val
      [NumV (lc f ld)
            (type-case ObjV r-val
              [NumV (rc f rd) (ValueA (store-object (NumV
                                                     (if (= lc (lookup/boot 'bool)) (lookup/boot 'int) lc)
                                                     (box empty) (+ ld rd))))]
              [else (interp-error 'TypeError "rhs not a number")])]
      [StrV (lc f ld)
            (type-case ObjV r-val
              [StrV (rc f rd) (if (= lc rc)
                                  (ValueA (store-object (StrV lc (box empty) (string-append ld rd))))
                                  (interp-error 'TypeError "type mismatch while concatenating"))]
              [else (interp-error 'TypeError "rhs not a string")])]
      [ListV (lc f lfrozen? ld)
             (type-case ObjV r-val
               [ListV (rc f rfrozen? rd)
                      (if (= lc rc)
                          (local [(define (eat-data data addendum)
                                    (if (empty? data)
                                        addendum
                                        (cons (first data) (eat-data (rest data) addendum))))]
                                 (ValueA (store-object (ListV lc
                                                              (box empty)
                                                              lfrozen?
                                                              (box (eat-data (unbox ld) (unbox rd)))))))
                          (interp-error 'TypeError "type mismatch while concatenating"))]
               [else (interp-error 'TypeError "rhs not an iterable")])]
      [DictV (lc f lfrozen? ld)
             (type-case ObjV r-val
               [DictV (rc f rfrozen? rd)
                      (if (and (= lc rc))
                          (local [(define (eat-addendum data addendum)
                                    (if (empty? addendum)
                                        (box data)
                                        (let ([addone (box data)]
                                              [item (first addendum)])
                                          (begin
                                            (hsh-set addone
                                                     (entV-key item)
                                                     (get-singleton-object 'None))
                                            (eat-addendum (unbox addone) (rest addendum))))))]
                                 (ValueA (store-object (DictV lc
                                                              (box empty)
                                                              lfrozen?
                                                              (eat-addendum (unbox ld) (unbox rd))))))
                          (interp-error 'TypeError "type mismatch while concatenating"))]
               [else (interp-error 'TypeError "rhs not an iterable")])]
      [else (interp-error 'TypeError "lhs not an addable")])))

(define (contains [needle-loc : Location] [haystack-loc : Location]) : AnswerC
  (let ([haystack-obj (fetch-object haystack-loc)]
        [needle-obj (fetch-object needle-loc)])
    (type-case ObjV haystack-obj
      [StrV (lc f haystack)
            (type-case ObjV needle-obj
              [StrV (rc f needle)
                    (local [(define (eat-both haystack needle)
                              (cond
                               [(empty? needle) #t]
                               [(empty? haystack) #f]
                               [(char=? (first haystack) (first needle)) (eat-both (rest haystack) (rest needle))]
                               [else #f]))
                            (define (eat-hay haystack needle) : AnswerC
                              (begin
                                (if (> debug-prim 1)
                                    (display
                                     (concat
                                      (list
                                       "contains/eat-hay: '"
                                       (list->string haystack)
                                       "', '" (list->string needle) "'\n")))
                                    (void))
                                (cond
                                 [(empty? needle) (ValueA (get-singleton-object 'True))]
                                 [(empty? haystack) (ValueA (get-singleton-object 'False))]
                                 [(eat-both haystack needle) (ValueA (get-singleton-object 'True))]
                                 [else (eat-hay (rest haystack) needle)])))]
                           (eat-hay (string->list haystack) (string->list needle)))]
              [else (interp-error 'TypeError "rhs not a string")])]
      [ListV (lc f fr haystack)
             (local [(define (eat-hay haystack) : AnswerC
                       (if (empty? haystack)
                           (ValueA (get-singleton-object 'False))
                           (if (= (test-equal (first haystack) needle-loc)
                                  (get-singleton-object 'True))
                               (ValueA (get-singleton-object 'True))
                               (eat-hay (rest haystack)))))]
                    (eat-hay (unbox haystack)))]
      [DictV (lc f fr haystack)
             (local [(define (eat-hay haystack)
                       (if (empty? haystack)
                           (ValueA (get-singleton-object 'False))
                           (if (= (test-equal (entV-key (first haystack)) needle-loc)
                                  (get-singleton-object 'True))
                               (ValueA (get-singleton-object 'True))
                               (eat-hay (rest haystack)))))]
                    (eat-hay (unbox haystack)))]
      [else (interp-error 'TypeError "lhs not an iterable")])))

(define (not_in [needle : Location] [haystack : Location])
  (type-case AnswerC (contains needle haystack)
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ExceptionA (e) (ExceptionA e)]
    [ValueA (v) (not_ v)]))

(define (truediv [l : Location] [r : Location])
  (let ([l-val (fetch-object l)]
        [r-val (fetch-object r)])
    (type-case ObjV l-val
      [NumV (lc f ld)
            (type-case ObjV r-val
              [NumV (rc f rd)
                    (if (= rd 0)
                        (interp-error 'ZeroDivisionError "division by zero")
                        (ValueA (store-object (NumV
                                               (if (= lc (lookup/boot 'bool)) (lookup/boot 'int) lc)
                                               (box empty) (/ ld rd)))))]
              [else (interp-error 'TypeError "rhs not a number")])]
      [else (interp-error 'TypeError "lhs not a number")])))

(define (floordiv [l : Location] [r : Location])
  (let ([l-val (fetch-object l)]
        [r-val (fetch-object r)])
    (type-case ObjV l-val
      [NumV (lc f ld)
            (type-case ObjV r-val
              [NumV (rc f rd)
                    (if (= rd 0)
                        (interp-error 'ZeroDivisionError "integer division or modulo by zero")
                        (ValueA (store-object (NumV
                                               (if (= lc (lookup/boot 'bool)) (lookup/boot 'int) lc)
                                               (box empty) (floor (/ ld rd))))))]
              [else (interp-error 'TypeError "rhs not a number")])]
      [else (interp-error 'TypeError "lhs not a number")])))

(define (is_ [l : Location] [r : Location])
  (if (= l r)
      (ValueA (get-singleton-object 'True))
      (ValueA (get-singleton-object 'False))))

(define (is_not [l : Location] [r : Location])
  (if (= l r)
      (ValueA (get-singleton-object 'False))
      (ValueA (get-singleton-object 'True))))

(define (mod [l : Location] [r : Location])
  (let ([l-val (fetch-object l)]
        [r-val (fetch-object r)])
    (type-case ObjV l-val
      [NumV (lc f ld)
            (type-case ObjV r-val
              [NumV (rc f rd)
                    (if (= rd 0)
                        (interp-error 'ZeroDivisionError "integer division or modulo by zero")
                        (ValueA (store-object (NumV
                                               (if (= lc (lookup/boot 'bool)) (lookup/boot 'int) lc)
                                               (box empty) (remainder ld rd)))))]
              [else (interp-error 'TypeError "rhs not a number")])]
      [else (interp-error 'TypeError "lhs not a number")])))

(define (instanceof obj cls)
  (= (get-singleton-object 'True)
     (isinstance/impl obj cls)))

(define (subclassof cls sup)
  (= (get-singleton-object 'True)
     (issubclass/impl cls sup)))

(define (sub [l : Location] [r : Location])
  (local
   [(define (set-diff ls rs)
      (let ([ret (box (unbox ls))])
        (if (empty? (unbox rs))
            ret
            (begin
              (hsh-del ret (entV-key (first (unbox rs))))
              (set-diff ret (box (rest (unbox rs))))))))]
   (let ([l-val (fetch-object l)]
         [r-val (fetch-object r)])
     (type-case ObjV l-val
       [NumV (lc f ld)
             (type-case ObjV r-val
               [NumV (rc f rd) (ValueA (store-object (NumV
                                                      (if (= lc (lookup/boot 'bool)) (lookup/boot 'int) lc)
                                                      (box empty) (- ld rd))))]
               [else (interp-error 'TypeError "rhs not a number")])]
       [DictV (lc f frl ld)
              (if (or (subclassof lc (lookup/boot 'set))
                      (subclassof lc (lookup/boot 'frozenset)))
                  (type-case ObjV r-val
                    [DictV (rc f frr rd)
                           (if (or (subclassof rc (lookup/boot 'set))
                                   (subclassof rc (lookup/boot 'frozenset)))
                               (ValueA (store-object (DictV lc (box empty) frl (set-diff ld rd))))
                               (interp-error 'TypeError "rhs not a set"))]
                    [else (interp-error 'TypeError "rhs not a set")])
                  (interp-error 'TypeError "lhs not a set"))]
       [else (interp-error 'TypeError "lhs not a number")]))))

(define (bitxor [l : Location] [r : Location])
  (local
   [(define (check ent rs)
      (if (empty? rs)
	  #t
	  (if (= (test-equal (entV-key ent) (entV-key (first rs)))
		 (get-singleton-object 'True))
	      #f
	      (check ent (rest rs)))))
    (define (set-xor ret ls rs)
      (if (empty? ls)
	  ret
	  (if (check (first ls) rs)
	      (begin
		(hsh-set ret
			 (entV-key (first ls))
			 (get-singleton-object 'None))
		(set-xor ret (rest ls) rs))
	      (set-xor ret (rest ls) rs))))]
   (let ([l-val (fetch-object l)]
         [r-val (fetch-object r)])
     (type-case ObjV l-val
       [NumV (lc f ld)
             (type-case ObjV r-val
               [NumV (rc f rd) (ValueA (store-object (NumV
                                                      (if (= lc (lookup/boot 'bool)) (lookup/boot 'int) lc)
                                                      (box empty) (bitwise-xor ld rd))))]
               [else (interp-error 'TypeError "rhs not a number")])]
       [DictV (lc f frl ld)
              (if (or (subclassof lc (lookup/boot 'set))
                      (subclassof lc (lookup/boot 'frozenset)))
                  (type-case ObjV r-val
                    [DictV (rc f frr rd)
                           (if (or (subclassof rc (lookup/boot 'set))
                                   (subclassof rc (lookup/boot 'frozenset)))
                               (ValueA (store-object (DictV lc
							    (box empty)
							    frl
							    (set-xor (set-xor (box empty)
									      (unbox ld)
									      (unbox rd))
								     (unbox rd)
								     (unbox ld)))))
                               (interp-error 'TypeError "rhs not a set"))]
                    [else (interp-error 'TypeError "rhs not a set")])
                  (interp-error 'TypeError "lhs not a set"))]
       [else (interp-error 'TypeError "lhs not a number")]))))

(define (bitor [l : Location] [r : Location])
  (local
   [(define (set-or ret rs)
      (if (empty? rs)
	  ret
	  (begin
	    (hsh-set ret
		     (entV-key (first rs))
		     (get-singleton-object 'None))
	    (set-or ret (rest rs)))))]
   (let ([l-val (fetch-object l)]
         [r-val (fetch-object r)])
     (type-case ObjV l-val
       [NumV (lc f ld)
             (type-case ObjV r-val
               [NumV (rc f rd) (ValueA (store-object (NumV
                                                      (if (= lc (lookup/boot 'bool)) (lookup/boot 'int) lc)
                                                      (box empty) (bitwise-ior ld rd))))]
               [else (interp-error 'TypeError "rhs not a number")])]
       [DictV (lc f frl ld)
              (if (or (subclassof lc (lookup/boot 'set))
                      (subclassof lc (lookup/boot 'frozenset)))
                  (type-case ObjV r-val
                    [DictV (rc f frr rd)
                           (if (or (subclassof rc (lookup/boot 'set))
                                   (subclassof rc (lookup/boot 'frozenset)))
                               (ValueA (store-object (DictV lc (box empty) frl
							    (set-or (set-or (box empty) (unbox ld)) (unbox rd)))))
                               (interp-error 'TypeError "rhs not a set"))]
                    [else (interp-error 'TypeError "rhs not a set")])
                  (interp-error 'TypeError "lhs not a set"))]
       [else (interp-error 'TypeError "lhs not a number")]))))

(define (bitand [l : Location] [r : Location])
  (local
   [(define (check ent rs)
      (if (empty? rs)
	  #f
	  (if (= (test-equal (entV-key ent) (entV-key (first rs)))
		 (get-singleton-object 'True))
	      #t
	      (check ent (rest rs)))))
    (define (set-and ret ls rs)
      (if (empty? ls)
	  ret
	  (if (check (first ls) rs)
	      (begin
		(hsh-set ret
			 (entV-key (first ls))
			 (get-singleton-object 'None))
		(set-and ret (rest ls) rs))
	      (set-and ret (rest ls) rs))))]
   (let ([l-val (fetch-object l)]
         [r-val (fetch-object r)])
     (type-case ObjV l-val
       [NumV (lc f ld)
             (type-case ObjV r-val
               [NumV (rc f rd) (ValueA (store-object (NumV
                                                      (if (= lc (lookup/boot 'bool)) (lookup/boot 'int) lc)
                                                      (box empty) (bitwise-and ld rd))))]
               [else (interp-error 'TypeError "rhs not a number")])]
       [DictV (lc f frl ld)
              (if (or (subclassof lc (lookup/boot 'set))
                      (subclassof lc (lookup/boot 'frozenset)))
                  (type-case ObjV r-val
                    [DictV (rc f frr rd)
                           (if (or (subclassof rc (lookup/boot 'set))
                                   (subclassof rc (lookup/boot 'frozenset)))
                               (ValueA (store-object (DictV lc (box empty) frl
                                                            (set-and (box empty)
                                                                     (unbox ld)
                                                                     (unbox rd)))))
                               (interp-error 'TypeError "rhs not a set"))]
                    [else (interp-error 'TypeError "rhs not a set")])
                  (interp-error 'TypeError "lhs not a set"))]
       [else (interp-error 'TypeError "lhs not a number")]))))

(define (deep-copy [l : Location])
  (local
   [(define (deep-copy-box l)
      (box (unbox l)))]
   (ValueA
    (store-object
     (type-case ObjV (fetch-object l)
       [UndefinedV () (UndefinedV)]
       [StrV (c at d) (StrV c (deep-copy-box at) d)]
       [NumV (c at d) (NumV c (deep-copy-box at) d)]
       [ListV (c at fr d) (ListV c (deep-copy-box at) fr (deep-copy-box d))]
       [DictV (c at fr d) (DictV c (deep-copy-box at) fr (deep-copy-box d))]
       [ClosV (c at a b e) (ClosV c (deep-copy-box at) a b e)]
       [ExnV (c at d) (ExnV c (deep-copy-box at) d)]
       [EmptyV (c at) (EmptyV c (deep-copy-box at))])))))

(define (mul [l : Location] [r : Location])
  (local
   [(define data (fetch-object l))
    (define data-loc l)
    (define times (fetch-object r))
    (define (grow times)
      (begin
        (if (> debug-prim 1)
            (display (concat
                      (list
                       "mul/grow: times:" (to-string times))))
            (void))
        (cond
         [(< times 1)
          (type-case ObjV data
            [StrV (c d s) (ValueA (store-object (StrV c (box empty) "")))]
            [ListV (c d fr l) (ValueA (store-object (ListV c (box empty) fr (box empty))))]
            [else (interp-error 'TypeError "Can't grow non-seq types")])]
         [(= 1 times) (deep-copy data-loc)]
         [else
          (type-case AnswerC (grow (sub1 times))
            [ReturnA (r) (ReturnA r)]
            [BreakA () (BreakA)]
            [ExceptionA (e) (ExceptionA e)]
            [ValueA (base) (add data-loc base)])])))]
   (begin
     (if (NumV? data)
         (let ([tmp-val times])
           (begin
             (set! times data)
             (set! data tmp-val)
             (set! data-loc r)))
         (void))
     (type-case ObjV times
       [NumV (rc f rd)
             (type-case ObjV data
               [NumV (lc f ld) (ValueA (store-object (NumV
                                                      (if (= lc (lookup/boot 'bool)) (lookup/boot 'int) lc)
                                                      (box empty) (* ld rd))))]
               [StrV (lc f ld) (grow rd)]
               [ListV (lc f fr ld) (grow rd)]
               [else (interp-error 'TypeError "lhs not a number, string or sequence")])]
       [else (interp-error 'TypeError "rhs not a number")]))))

(define (eq [l : Location] [r : Location]) (ValueA (test-equal l r)))

(define (ne [l : Location] [r : Location]) (not_ (ValueA-v (eq l r))))

(define (lt [l : Location] [r : Location]) : AnswerC
  (let ([l-val (fetch-object l)]
        [r-val (fetch-object r)])
    (cond
     [(= l r) (ValueA (get-singleton-object 'False))]
     [else
      (type-case ObjV l-val
        [NumV (lc f ld)
              (type-case ObjV r-val
                [NumV (rc f rd)
                      (if (< ld rd)
                          (ValueA (get-singleton-object 'True))
                          (ValueA (get-singleton-object 'False)))]
                [else (interp-error 'TypeError "unorderable types")])]
        [StrV (lc f ld)
              (type-case ObjV r-val
                [StrV (rc f rd)
                      (begin
                        (if (> debug-prim 1)
                            (display (concat (list "check: " ld " < " rd "\n")))
                            (void))
                        (if (string<? ld rd)
                            (ValueA (get-singleton-object 'True))
                            (ValueA (get-singleton-object 'False))))]
                [else (interp-error 'TypeError "unorderable types")])]
        [ListV (lc f lfr ld)
               (type-case ObjV r-val
                 [ListV (rc f rfr rd)
                        (if (not (equal? lfr rfr))
                            (interp-error 'TypeError "unorderable types")
                            (local [(define (eat-both ll rl) : AnswerC
                                      (if (empty? rl)
                                          (ValueA (get-singleton-object 'False))
                                          (if (empty? ll)
                                              (ValueA (get-singleton-object 'True))
                                              (type-case AnswerC (gt (first ll) (first rl))
                                                [ReturnA (r) (ReturnA r)]
                                                [BreakA () (BreakA)]
                                                [ExceptionA (e) (ExceptionA e)]
                                                [ValueA (v)
                                                        (if (= v (get-singleton-object 'True))
                                                            (ValueA (get-singleton-object 'False))
                                                            (eat-both (rest ll) (rest rl)))]))))]
                                   (eat-both (unbox ld) (unbox rd))))]
                 [else (interp-error 'TypeError "unorderable types")])]
        [else (interp-error 'TypeError "unorderable types")])])))

(define (gt [l : Location] [r : Location])
  (let ([l-val (fetch-object l)]
        [r-val (fetch-object r)])
    (cond
     [(= l r) (ValueA (get-singleton-object 'False))]
     [else
      (type-case ObjV l-val
        [NumV (lc f ld)
              (type-case ObjV r-val
                [NumV (rc f rd) (if (> ld rd)
                                    (ValueA (get-singleton-object 'True))
                                    (ValueA (get-singleton-object 'False)))]
                [else (interp-error 'TypeError "unorderable types")])]
        [StrV (lc f ld)
              (type-case ObjV r-val
                [StrV (rc f rd) (if (string>? ld rd)
                                    (ValueA (get-singleton-object 'True))
                                    (ValueA (get-singleton-object 'False)))]
                [else (interp-error 'TypeError "unorderable types")])]
        [ListV (lc f lfr ld)
               (type-case ObjV r-val
                 [ListV (rc f rfr rd)
                        (if (not (equal? lfr rfr))
                            (interp-error 'TypeError "unorderable types")
                            (local [(define (eat-both ll rl) : AnswerC
                                      (if (empty? rl)
                                          (ValueA (get-singleton-object 'False))
                                          (if (empty? ll)
                                              (ValueA (get-singleton-object 'True))
                                              (type-case AnswerC (lt (first ll) (first rl))
                                                [ReturnA (r) (ReturnA r)]
                                                [BreakA () (BreakA)]
                                                [ExceptionA (e) (ExceptionA e)]
                                                [ValueA (v)
                                                        (if (= v (get-singleton-object 'True))
                                                            (ValueA (get-singleton-object 'False))
                                                            (eat-both (rest ll) (rest rl)))]))))]
                                   (eat-both (unbox ld) (unbox rd))))]
                 [else (interp-error 'TypeError "unorderable types")])]
        [else (interp-error 'TypeError "unorderable types")])])))

(define (le [l : Location] [r : Location])
  (if (= (get-singleton-object 'True)
         (test-equal l r))
      (ValueA (get-singleton-object 'True))
      (lt l r)))

(define (ge [l : Location] [r : Location])
  (if (= (get-singleton-object 'True)
         (test-equal l r))
      (ValueA (get-singleton-object 'True))
      (gt l r)))

(define (extremum [cmp : symbol] [seq-loc : Location])
  (local
   [(define (ext/impl cmp-lam l)
      (if (empty? (rest l))
          (first l)
          (let ([res (ext/impl cmp-lam (rest l))])
            (if (cmp-lam (first l) res)
                (first l)
                res))))
    (define loc-cmp-err 0)
    (define (loc-cmp a-loc b-loc)
      (type-case AnswerC (python-prim2 cmp a-loc b-loc)
        [ReturnA (r) (error 'extremum/loc-cmp "How did ReturnA happen?")]
        [BreakA ()  (error 'extremum/loc-cmp "How did BreakA happen?")]
        [ExceptionA (e) (begin (set! loc-cmp-err e) #f) ]
        [ValueA (v) (if (= v (get-singleton-object 'True)) #t #f)]))]
   (type-case ObjV (fetch-object seq-loc)
     [StrV (c a str)
           (if (string=? str "")
               (interp-error 'ValueError "empty string for extremum")
               (ValueA
                (store-object
                 (StrV c (box empty)
                       (list->string
                        (list
                         (ext/impl (if (symbol=? cmp 'lt)
                                       (lambda (a b) (char<? a b))
                                       (lambda (a b) (char>? a b)))
                                   (string->list str))))))))]
     [ListV (c a fr l)
            (if (empty? (unbox l))
                (interp-error 'ValueError "empty sequence for extremum")
                (let [(ext (ext/impl loc-cmp (unbox l)))]
                  (if (= 0 loc-cmp-err)
                      (ValueA ext)
                      (ExceptionA loc-cmp-err))))]
     [else (interp-error 'TypeError "not iterable for extremum detection")])))

(define (python-prim1 [op : symbol] [arg : Location]) : AnswerC
  (case op
    [(print) (begin (print arg) (ValueA (get-singleton-object 'None)))]
    [(not) (not_ arg)]
    [(truth) (truth arg)]
    [(uadd) (type-case ObjV (fetch-object arg)
              [NumV (c d n) (add (store-object (NumV c (box empty) 0)) arg)]
              [else (interp-error 'TypeError "usub argument not a number")])]
    [(usub) (type-case ObjV (fetch-object arg)
              [NumV (c d n) (sub (store-object (NumV c (box empty) 0)) arg)]
              [else (interp-error 'TypeError "usub argument not a number")])]
    [(min) (extremum 'lt arg)]
    [(max) (extremum 'gt arg)]
    [(abs) (abs_ arg)]
    [(invert) (bitwise-not_ arg)]
    [else (interp-error 'RuntimeError (string-append "Unknown op: " (symbol->string op)))]))

(define (python-prim2 [op : symbol] [l-loc : Location] [r-loc : Location]) : AnswerC
  (case op
    [(add) (add l-loc r-loc)]
    [(sub) (sub l-loc r-loc)]
    [(mul) (mul l-loc r-loc)]
    [(div) (truediv l-loc r-loc)]
    [(floordiv) (floordiv l-loc r-loc)]
    [(mod) (mod l-loc r-loc)]
    [(eq) (eq l-loc r-loc)]
    [(ne) (ne l-loc r-loc)]
    [(lt) (lt l-loc r-loc)]
    [(gt) (gt l-loc r-loc)]
    [(le) (le l-loc r-loc)]
    [(ge) (ge l-loc r-loc)]
    [(is) (is_ l-loc r-loc)]
    [(isnot) (is_not l-loc r-loc)]
    [(in) (contains l-loc r-loc)]
    [(notin) (not_in l-loc r-loc)]
    [(bitxor) (bitxor l-loc r-loc)]
    [(bitor) (bitor l-loc r-loc)]
    [(bitand) (bitand l-loc r-loc)]
    [else (interp-error 'RuntimeError (string-append "Unknown op: " (symbol->string op)))]))
