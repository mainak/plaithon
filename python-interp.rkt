#lang plai-typed

(require "python-core-syntax.rkt"
         "python-base.rkt"
         "python-primitives.rkt"
         "python-bootstrap.rkt")

;; raise-user-error raises errors that don't have any prefix on them
(require (typed-in racket/base (raise-user-error : (string -> ObjV))))

(define debug-interp 0)

;; interp the given expr to produce a value
(define (interp [exprC : ExprC]) : ObjV
  (begin (init-memory)
         (type-case AnswerC (interp-full exprC builtin-env (list (nullE)) (get-singleton-object 'Undefined))
           [ExceptionA (exn-loc) (raise-user-error (pretty exn-loc))]
           [ValueA (val) (fetch-object val)]
           [ReturnA (r) (fetch-object r)]
           [BreakA () (raise-user-error
                       (pretty (store-object (ExnV (lookup/boot 'RuntimeError) (box empty)
                                                   (obj-str "can't BreakA in top level interp")))))])))

;; evaluates the list of expressions in order with
;; side-effects carried forward and returned
(define (interp-expr-list [exprs : (listof ExprC)]
                          [env : Env] [stack : (listof Env)] a-exn)
  (if (empty? exprs)
      (ValueListA empty)
      (type-case AnswerC (interp-full (first exprs) env stack a-exn)
        [ValueA (val) (type-case MultiValAnswerC (interp-expr-list (rest exprs) env stack a-exn)
                        [ValueListA (vlist) (ValueListA (cons val vlist))]
                        [MVExceptionA (v-exn) (MVExceptionA v-exn)]
                        [MVReturnA (r) (MVReturnA r)]
                        [MVBreakA () (MVBreakA)])]
        [ExceptionA (v-exn) (MVExceptionA v-exn)]
        [ReturnA (r) (MVReturnA r)]
        [BreakA () (MVBreakA)])))

(define (interp-dict cls maps is-frozen? env stack a-exn)
  (letrec ([table (box empty)]
           [interp-mappings (lambda (ml)
                              ;; returns list with single element errH if error occurs
                              ;; otherwise returns a list of hashables
                              (if (empty? ml)
                                  (ValueA (store-object (DictV cls (box empty) is-frozen? table)))
                                  (type-case MultiValAnswerC (interp-expr-list (list (entC-key (first ml))
                                                                                     (entC-val (first ml))) env stack a-exn)
                                    [MVExceptionA (e) (ExceptionA e)]
                                    [MVReturnA (r) (ReturnA r)]
                                    [MVBreakA () (BreakA)]
                                    [ValueListA (vlist)
                                                (begin (hsh-set table (first vlist) (second vlist))
                                                       (interp-mappings (rest ml)))])))])
    (interp-mappings maps)))

(define (interp-list cls l is-frozen? env stack a-exn)
  (type-case MultiValAnswerC (interp-expr-list l env stack a-exn)
    [ValueListA (vl) (ValueA (store-object (ListV cls (box empty) is-frozen? (box vl))))]
    [MVExceptionA (e) (ExceptionA e)]
    [MVReturnA (r) (ReturnA r)]
    [MVBreakA () (BreakA)]))

(define (insert-none lst)
  (if (empty? lst)
      empty
      (cons (entC (first lst) (NoneC))
            (insert-none (rest lst)))))

(define (search-mro [cls : Location] [field : Location]) : AnswerC
  (if (not (= (lookup/boot 'type) (get-class cls)))
      (interp-error 'TypeError "mro can be searched only in type objs")
      (letrec ([mro-loc (hsh-get (get-dict cls) (obj-str "__mro__"))]
               [search (lambda (mro-l) : AnswerC
                               (begin
                                 (if (> debug-interp 2)
                                     (display "searching mro...\n")
                                     (void))
                                 (if (empty? mro-l)
                                     (interp-error 'AttributeError
                                                   (concat
                                                    (list (pretty cls)
                                                          " has no attribute: "
                                                          (pretty field))))
                                     (let ([attr (hsh-get (get-dict (first mro-l)) field)])
                                       (if (none? attr)
                                           (search (rest mro-l))
                                           (ValueA (some-v attr)))))))])
        (type-case ObjV (fetch-object (some-v mro-loc))
          [ListV (c d fr mro) (search (unbox mro))]
          [else (interp-error 'TypeError "type mro is not a tuple obj!!")]))))

(define (interp-getcallable [objc : ExprC] env stack a-exn)
  (type-case AnswerC (interp-full objc env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (obj-l)
            (if (ClosV? (fetch-object obj-l))
                (ValueA obj-l)
                (type-case AnswerC (search-mro (get-class obj-l)
                                               (obj-str "__call__"))
                  [ExceptionA (e) (ExceptionA e)]
                  [ReturnA (r) (ReturnA r)]
                  [BreakA () (BreakA)]
                  [ValueA (call)
                          (if (ClosV? (fetch-object call))
                              (bind-first-arg call obj-l #t "getcallable")
                              (interp-error 'TypeError
                                            (concat
                                             (list "__call__ method of type '"
                                                   (pretty (get-class obj-l))
                                                   "' is not a ClosV"))))]))]))

(define (bind-first-arg [closv-loc : Location] [obj : Location] from-mro? dbg-src)
  (local
   [(define error (BreakA))
    (define (has-vararg? attr)
      (let ([op (hsh-get attr (obj-str "__has-vararg__"))])
        (if (none? op)
            #f
            (= (get-singleton-object 'True)
               (get-truth (some-v op))))))
    (define (get-bound-attrs attrs arg-sym arg-loc)
      (let ([n-attrs (box (unbox attrs))]
            [bindings (hsh-get attrs (obj-str "__bindings__"))]
            [defaults (hsh-get attrs (obj-str "__defaults__"))])
        (begin
          (if (none? bindings)
              (hsh-set n-attrs
                       (obj-str "__bindings__")
                       (obj-dict (list (entV (obj-str (symbol->string arg-sym))
                                             arg-loc))))
              (type-case ObjV (fetch-object (some-v bindings))
                [DictV (c d fr bs)
                       (hsh-set bs
                                (obj-str (symbol->string arg-sym))
                                arg-loc)]
                [else (set! error (interp-error 'ValueError "__bindings__ of function not a dict"))]))
          (if (none? defaults)
              (void)
              (type-case ObjV (fetch-object (some-v defaults))
                [ListV (c a fr d)
                       (if (empty? (unbox d))
                           (void)
                           (if (or (= (length (unbox d)) (length (ClosV-args (fetch-object closv-loc))))
                                   (and (has-vararg? attrs)
                                        (= (length (unbox d)) (sub1 (length (ClosV-args (fetch-object closv-loc)))))))
                               (hsh-set n-attrs (obj-str "__defaults__")
                                        (store-object (ListV c a fr (box (rest (unbox d))))))
                               (void)))]
                [else (set! error (interp-error 'ValueError "__defaults__ of function is not a list"))]))
          n-attrs)))
    (define (bind-arg [clos : ObjV] [arg : Location])
      (type-case ObjV clos
        [ClosV (cls attr args body cl-env)
               (let ([cl-attr (get-bound-attrs attr (first args) arg)])
                 (if (BreakA? error)
                     (ValueA
                      (let ([bound-cl
                             (store-object
                              (ClosV (lookup/boot 'function)
                                     cl-attr
                                     (rest args)
                                     body
                                     cl-env))])
                        (begin
                          (if (> debug-interp 2)
                              (display
                               (concat
                                (list "\ncalled by "
                                      dbg-src
                                      ", bind produced...\n"
                                      (pretty bound-cl)
                                      "\nbind returned\n")))
                              (void))
                          bound-cl)))
                     error))]
        [else (interp-error 'RuntimeError "wasn't expecting a non-closure while binding")]))]
   (let* ([fun (fetch-object closv-loc)]
          [decorators (hsh-get (ClosV-dict fun) (obj-str "__decorators__"))])
     (if (none? decorators)
         (if from-mro?
             (bind-arg fun obj)
             (ValueA closv-loc))
         (type-case AnswerC (contains (obj-str "classmethod")
                                      (some-v decorators))
           [ExceptionA (e) (ExceptionA e)]
           [ReturnA (r) (ReturnA r)]
           [BreakA () (BreakA)]
           [ValueA (in?)
                   (if (= in? (get-singleton-object 'True))
                       (if (= (isinstance/impl obj (lookup/boot 'type))
                              (get-singleton-object 'True))
                           (bind-arg fun obj)
                           (bind-arg fun (get-class obj)))
                       (if from-mro?
                           (bind-arg fun obj)
                           (ValueA closv-loc)))])))))

(define (interp-bind [fun : ExprC] [obj : ExprC] [env : Env] [stack : (listof Env)] a-exn) : AnswerC
  (let ([fun-ans (interp-full fun env stack a-exn)])
    (if (ValueA? fun-ans)
        (if (ClosV? (fetch-object (ValueA-v fun-ans)))
            (let ([obj-ans (interp-full obj env stack a-exn)])
              (if (ValueA? obj-ans)
                  (bind-first-arg (ValueA-v fun-ans) (ValueA-v obj-ans) #t "bind")
                  obj-ans))
            (interp-error 'TypeError "first arg to bind is not a closure"))
        fun-ans)))

(define (get-attr [obj : Location] [field : Location] [env : Env] [stack : (listof Env)] a-exn) : AnswerC
  (cond
   [(= (test-equal field (obj-str "__class__")) (get-singleton-object 'True))
    (ValueA (get-class obj))]
   [(= (test-equal field (obj-str "__dict__")) (get-singleton-object 'True))
    (ValueA (obj-dict (unbox (get-dict obj))))]
   [else
    (let ([data  (hsh-get (get-dict obj) field)])
      (if (some? data)
          (if (ClosV? (fetch-object (some-v data)))
              (bind-first-arg (some-v data) obj #f "get-attr-dict")
              (ValueA (some-v data)))
          (if (= (get-class obj) (lookup/boot 'type))
              (let ([self-mro (search-mro obj field)])
                (if (ValueA? self-mro)
                    (if (ClosV? (fetch-object (ValueA-v self-mro)))
                        (bind-first-arg (ValueA-v self-mro) obj #t "get-attr-mro")
                        self-mro)
                    (type-case AnswerC (search-mro (get-class obj) field)
                      [ExceptionA (e) (ExceptionA e)]
                      [ReturnA (r) (ReturnA r)]
                      [BreakA () (BreakA)]
                      [ValueA (data)
                              (if (ClosV? (fetch-object data))
                                  (bind-first-arg data obj #t "get-attr-mro")
                                  (ValueA data))])))
              (type-case AnswerC (search-mro (get-class obj) field)
                [ExceptionA (e) (ExceptionA e)]
                [ReturnA (r) (ReturnA r)]
                [BreakA () (BreakA)]
                [ValueA (data)
                        (if (ClosV? (fetch-object data))
                            (bind-first-arg data obj #t "get-attr-mro")
                            (ValueA data))]))))]))

(define (set-attr [obj : Location] [field : Location] [val : ExprC] [env : Env] [stack : (listof Env)] a-exn) : AnswerC
  (type-case AnswerC (interp-full val env stack a-exn)
    [ValueA (v-loc) (begin
                      (hsh-set (get-dict obj) field v-loc)
                      (ValueA (get-singleton-object 'None)))]
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]))

(define (del-attr [obj : Location] [field : Location]) : AnswerC
  (begin (hsh-del (get-dict obj) field)
         (ValueA (get-singleton-object 'None))))

(define (manip-attr [obj-expr : ExprC] [f-expr : ExprC] [env : Env] [stack : (listof Env)] a-exn task) : AnswerC
  (type-case AnswerC (interp-full obj-expr env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (obj-loc)
            (type-case AnswerC (interp-full f-expr env stack a-exn)
              [ExceptionA (e) (ExceptionA e)]
              [ReturnA (r) (ReturnA r)]
              [BreakA () (BreakA)]
              [ValueA (field) (task obj-loc field)])]))

(define (get-data coll index)
  (type-case ObjV (fetch-object coll)
    [StrV (c flds str)
          (cond
           [(or (= (lookup/boot 'int) (get-class index))
                (= (lookup/boot 'slice) (get-class index)))
            (type-case (optionof Location) (str-get str index)
              [none () (interp-error 'IndexError "string index out of range")]
              [some (v) (ValueA v)])]
           [else (interp-error 'TypeError "index must be integer or slice")])]
    [ListV (c flds fr l)
           (cond
            [(= (lookup/boot 'int) (get-class index))
             (type-case (optionof (listof Location)) (vec-get l index)
               [none () (interp-error 'IndexError "list index out of range")]
               [some (v) (ValueA (first v))])]
            [(= (lookup/boot 'slice) (get-class index))
             (ValueA (store-object (ListV c (box empty) fr (box (some-v (vec-get l index))))))]
            [else (interp-error 'TypeError "index must be integer or slice")])]
    [DictV (c flds f d) (type-case (optionof Location) (hsh-get d index)
                          [none () (interp-error 'KeyError
                                                 (string-append (pretty index)
                                                                ": key not found"))]
                          [some (v) (ValueA v)])]
    [else (interp-error 'TypeError "object is not subscriptable")]))

(define (set-data coll index val-expr env stack a-exn)
  (type-case AnswerC (interp-full val-expr env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (val)
            (type-case ObjV (fetch-object coll)
              [ListV (c flds frozen? l)
                     (if frozen?
                         (interp-error 'TypeError "object does not support append")
                         (type-case ObjV (fetch-object index)
                           [NumV (c flds n) (begin (vec-set l n val)
                                                   (ValueA (get-singleton-object 'None)))]
                           [else (interp-error 'TypeError "indices must be integers")]))]
              [DictV (c flds frozen? d)
                     (if frozen?
                         (interp-error 'TypeError "object does not support item assignment")
                         (begin (hsh-set d index val)
                                (ValueA (get-singleton-object 'None))))]
              [else (interp-error 'TypeError "object does not support item assignment")])]))

(define (append-data vec obj-l)
  (type-case ObjV (fetch-object vec)
    [ListV (c flds frozen? l)
           (if frozen?
               (interp-error 'TypeError "object does not support append")
               (begin (vec-append l obj-l)
                      (ValueA (get-singleton-object 'None))))]
    [else (interp-error 'TypeError "object does not support append")]))

(define (del-data coll index)
  (type-case ObjV (fetch-object coll)
    [ListV (c flds frozen? l)
           (if frozen?
               (interp-error 'TypeError "object does not support item deletion")
               (type-case ObjV (fetch-object index)
                 [NumV (c flds n) (begin (vec-del l n)
                                         (ValueA (get-singleton-object 'None)))]
                 [else (interp-error 'TypeError "indices must be integers")]))]
    [DictV (c flds frozen? d)
           (if frozen?
               (interp-error 'TypeError "object does not support item deletion")
               (begin (hsh-del d index)
                      (ValueA (get-singleton-object 'None))))]
    [else (interp-error 'TypeError "object does not support item assignment")]))

(define (manip-data coll index env stack a-exn task)
  (type-case AnswerC (interp-full coll env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (c-loc)
            (type-case AnswerC (interp-full index env stack a-exn)
              [ExceptionA (e) (ExceptionA e)]
              [ReturnA (r) (ReturnA r)]
              [BreakA () (BreakA)]
              [ValueA (i-loc) (task c-loc i-loc)])]))

;; delegate the work to various functions
(define (interp-full [exprC : ExprC] [env : Env] [stack : (listof Env)] [a-exn : Location]) : AnswerC
  (begin (if (> debug-interp 0)
             (begin
                                        ; (display   "=========ACTIVE EXCEPTION:============================================================\n")
                                        ; (display (pretty a-exn))
                                        ; (display "\n=========CALLER ENV:==================================================================")
                                        ; (display (pretty-env stack))
               (display "\n=========CURRENT ENV:=================================================================")
               (display (pretty-env env))
               (display "\n#########CODE:########################################################################\n")
               (display (pretty-exprc exprC))
               (display "\n######################################################################################\n\n\n"))
             (void))
         (type-case ExprC exprC
           [ObjC (cls attrs data) (interp-object cls attrs data env stack a-exn)]
           [StrC (data) (ValueA (store-object (StrV (lookup/boot 'str) (box empty) data)))]
           [NumC (cls data) (ValueA (store-object (NumV (lookup/boot cls) (box empty) data)))]

           [TupleC     (l) (interp-list (lookup/boot 'tuple) l #t env stack a-exn)]
           [ListC      (l) (interp-list (lookup/boot 'list) l #f env stack a-exn)]
           #|
           [BytesC     (cls l) (interp-list cls l #t env stack a-exn)]
           [ByteArrayC (cls l) (interp-list cls l #f env stack a-exn)]
           |#
           [DictC      (d) (interp-dict (lookup/boot 'dict) d #f env stack a-exn)]
           [SetC       (l) (interp-dict (lookup/boot 'set) (insert-none l) #f env stack a-exn)]
           [FrozenSetC (l) (interp-dict (lookup/boot 'frozenset) (insert-none l) #t env stack a-exn)]

           [LamC  (args body) (ValueA (store-object (ClosV (lookup/boot 'function)
                                                           (box (list (entV (obj-str "__name__")
                                                                            (obj-str "lambda/anon"))))
                                                           args
                                                           body
                                                           env)))]
           [ErrorC (type expr) (type-case AnswerC (interp-full expr env stack a-exn)
                                 [ValueA (v) (ExceptionA (store-object (ExnV (lookup/boot type) (box empty) v)))]
                                 [ReturnA (r) (ExceptionA (store-object (ExnV (lookup/boot type) (box empty) r)))]
                                 [BreakA () (ExceptionA (store-object (ExnV (lookup/boot type) (box empty) (obj-str "how did we throw BreakA?"))))]
                                 [ExceptionA (e) (ExceptionA (store-object (ExnV (lookup/boot type) (box empty) e)))])]
           [DynErrorC (exn)
                      (type-case AnswerC (interp-full exn env stack a-exn)
                        [ValueA (v)
                                (if (= v (get-singleton-object 'Undefined))
                                    (if (= a-exn (get-singleton-object 'Undefined))
                                        (interp-error 'RuntimeError "No active exceptions to raise")
                                        (ExceptionA a-exn))
                                    (ExceptionA v))]
                        [ReturnA (r) (interp-error 'RuntimeError "how return in dynerr?")]
                        [BreakA () (interp-error 'RuntimeError "how break in dynerr?")]
                        [ExceptionA (e) (ExceptionA e)])]

           [SuperC (type obj)
                   (interp-super type obj env stack a-exn)]

           [NoneC     () (ValueA (get-singleton-object 'None))]
           [TrueC     () (ValueA (get-singleton-object 'True))]
           [FalseC    () (ValueA (get-singleton-object 'False))]
           [EllipsisC () (ValueA (get-singleton-object 'Ellipsis))]

           [UndefinedC () (ValueA (get-singleton-object 'Undefined))]

           [BindC (fun obj) (interp-bind fun obj env stack a-exn)]
           [GetAttrC  (obj field)     (manip-attr obj field env stack a-exn (lambda (obj fld)
                                                                              (get-attr obj fld env stack a-exn)))]
           [SetAttr!C (obj field val) (manip-attr obj field env stack a-exn (lambda (obj fld)
                                                                              (set-attr obj fld val env stack a-exn)))]
           [DelAttr!C (obj field)     (manip-attr obj field env stack a-exn del-attr)]
           [SearchMroC (cls field) (interp-searchmro cls field env stack a-exn)]

           [CopyObjC  (objc)     (type-case AnswerC (interp-full objc env stack a-exn)
                                   [ExceptionA (e) (ExceptionA e)]
                                   [ReturnA (r) (ReturnA r)]
                                   [BreakA () (BreakA)]
                                   [ValueA (v) (deep-copy v)])]
           [GetCallableC  (objc)     (interp-getcallable objc env stack a-exn)]
           [GetItemAtIdxC  (coll index)     (interp-getitematidx coll index env stack a-exn)]
           [GetDataC  (coll index)     (manip-data coll index env stack a-exn get-data)]
           [SetData!C (coll index val) (manip-data coll index env stack a-exn (lambda (cl idx) (set-data cl idx val env stack a-exn)))]
           [DelData!C (coll index)     (manip-data coll index env stack a-exn del-data)]
           [AppendData!C (vec new-data) (manip-data vec new-data env stack a-exn append-data)]

           [AppC (func args) (interp-app func args env stack a-exn)]
           [DynAppC (func args) (interp-dyn-app func args env stack a-exn)]

           [ModuleC (name locals expr) (interp-full expr
                                                    (grow-env (create-env name env 'module)
                                                              (map (lambda (id)
                                                                     (lclB id (box (get-singleton-object 'Undefined))))
                                                                   locals))
                                                    stack a-exn)]
           [FuncDefC (name globals nonlocals wr-locals rd-locals args defaults decorators body)
                     (interp-funcdef name globals nonlocals wr-locals rd-locals args defaults decorators body
                                     env stack a-exn)]
           [ClassDefC (name globals nonlocals locals expr)
                      (interp-full expr
                                   (grow-env (create-env name env 'class)
                                             (append (map (lambda (id)
                                                            (glbB id))
                                                          globals)
                                                     (append (map (lambda (id)
                                                                    (nlcB id))
                                                                  nonlocals)
                                                             (map (lambda (id)
                                                                    (lclB id (box (get-singleton-object 'Undefined))))
                                                                  locals))))
                                   stack a-exn)]


           [LetC (id bind body) (interp-letc id bind body env stack a-exn)]
           [Set!C (id value) (interp-setc id value env stack a-exn)]
           [IdC (id) (lookup-env env id)]
           [Del!C (id) (delete-env env id)]

           [TypeC (obj) (interp-type obj env stack a-exn)]
           [LenC (obj) (interp-len obj env stack a-exn)]
           [ToStrC (obj) (interp-tostr obj env stack a-exn)]

           [TryCatchC (body param catch) (interp-try-catch body param catch env stack a-exn)]
           [TryFinallyC (body finalbody) (interp-try-finally body finalbody env stack a-exn)]

           [Prim1C (op arg) (interp-prim1 op arg env stack a-exn)]
           [Prim2C (op arg1 arg2) (interp-prim2 op arg1 arg2 env stack a-exn)]

           [SeqC (e1 e2) (type-case AnswerC (interp-full e1 env stack a-exn)
                           [ValueA (v) (interp-full e2 env stack a-exn)]
                           [ExceptionA (e) (ExceptionA e)]
                           [ReturnA (r) (ReturnA r)]
                           [BreakA () (BreakA)])]
           [IfC (cnd thn els) (interp-if cnd thn els env stack a-exn)]
           [WhileC (test body orelse) (interp-while test body orelse env stack a-exn)]

           [ReturnC (val)
                    (type-case AnswerC (interp-full val env stack a-exn)
                      [ExceptionA (e) (ExceptionA e)]
                      [ValueA (v) (ReturnA v)]
                      [ReturnA (v) (interp-error 'RuntimeError "can't return a ReturnA")]
                      [BreakA () (interp-error 'RuntimeError "can't return a BreakA")])]
           [BreakC () (BreakA)]

           [IsInstanceC (obj cls) (interp-isinstance obj cls env stack a-exn)]
           [IsSubclassC (cls super) (interp-issubclass cls super env stack a-exn)]

           [RunCoreC (op args) (interp-runcore op args env stack a-exn)])))

(define (interp-super type obj env stack a-exn)
  (local
   [(define error (BreakA))
    (define (find-enclosing-class env)
      (type-case Env env
        [nullE () (get-singleton-object 'Undefined)]
        [clssE (name outer table)
               (let ([ans (lookup-env env name)])
                 (type-case AnswerC ans
                   [ValueA (v) v]
                   [else (begin (set! error ans)
                                (get-singleton-object 'Undefined))]))]
        [modlE (name outer table)
               (find-enclosing-class outer)]
        [funcE (name outer table)
               (find-enclosing-class outer)]))
    (define (find-bound-obj env)
      (type-case Env env
        [funcE (name outer table)
               (begin
                 (if (> debug-interp 2)
                     (display
                      (concat
                       (list
                        "\ntrying func: "
                        (symbol->string name))))
                     (void))
                 (if (clssE? outer)
                     (begin
                       (if (> debug-interp 2)
                           (display
                            (concat
                             (list
                              "\nouter: "
                              (symbol->string (clssE-name outer))
                              "\n")))
                           (void))
                       (let ([fun-ans (lookup-env outer name)])
                         (begin
                           (if (> debug-interp 2)
                               (display
                                (concat
                                 (list
                                  "\nlkup-ans: "
                                  (pretty-answer fun-ans)
                                  "\n")))
                               (void))
                           (type-case AnswerC fun-ans
                             [ValueA (funobj)
                                     (type-case ObjV (fetch-object funobj)
                                       [ClosV (c attr args b e)
                                              (if (empty? args)
                                                  (begin (set! error (interp-error 'RuntimeError "how can a bound function be nullary?"))
                                                         (get-singleton-object 'Undefined))
                                                  (type-case AnswerC (lookup-env env (first args))
                                                    [ValueA (v) v]
                                                    [else (begin (set! error (interp-error 'RuntimeError "how can a bound function be nullary?"))
                                                                 (get-singleton-object 'Undefined))]))]
                                       [else (begin
                                               (set! error (interp-error 'TypeError "name is not a function in enclosing scope"))
                                               (get-singleton-object 'Undefined))])]
                             [else (get-singleton-object 'Undefined)]))))
                     (find-bound-obj outer)))]
        [else (get-singleton-object 'Undefined)]))
    (define (find-type-and-obj stack)
      (if (empty? stack)
          (begin
            (set! error (interp-error 'RuntimeError "no enclosing classdef found"))
            (values (get-singleton-object 'Undefined)
                    (get-singleton-object 'Undefined)))
          (let ([env (first stack)])
            (type-case Env env
              [funcE (name outer names)
                     (let ([enc-clss (find-enclosing-class outer)]
                           [bound-obj (find-bound-obj env)])
                       (if (or (= enc-clss (get-singleton-object 'Undefined))
                               (= enc-clss (lookup/boot 'type)))
                           (find-type-and-obj (rest stack))
                           (values enc-clss bound-obj)))]
              [else (find-type-and-obj (rest stack))]))))
    (define (create-super-obj type-l obj-l)
      (type-case (optionof Location) (hsh-get (get-dict type-l)
                                              (obj-str "__mro__"))
        [none () (interp-error 'RuntimeError "type object does not have __mro__ attr")]
        [some (v) (type-case ObjV (fetch-object v)
                    [ListV (c a fr mro-list)
                           (ReturnA (store-object
                                     (EmptyV (lookup/boot 'super)
                                             (box
                                              (list
                                               (entV (obj-str "__type__") type-l)
                                               (entV (obj-str "__instance__")
                                                     (if (and (not (= obj-l (get-singleton-object 'Undefined)))
                                                              (= (isinstance/impl obj-l (lookup/boot 'type))
                                                                 (get-singleton-object 'True)))
                                                         (get-singleton-object 'Undefined)
                                                         obj-l))
                                               (entV (obj-str "__subtype__")
                                                     (if (and (not (= obj-l (get-singleton-object 'Undefined)))
                                                              (= (isinstance/impl obj-l (lookup/boot 'type))
                                                                 (get-singleton-object 'True)))
                                                         obj-l
                                                         (get-singleton-object 'Undefined)))
                                               (entV (obj-str "__mro__")
                                                     (obj-tup (rest (unbox mro-list)))))))))]
                    [else (interp-error 'ValueError "__mro__ of a type not a list")])]))]
   (type-case AnswerC (interp-full type env stack a-exn)
     [ExceptionA (e) (ExceptionA e)]
     [BreakA () (BreakA)]
     [ReturnA (r) (ReturnA r)]
     [ValueA (type-l)
             (type-case AnswerC (interp-full obj env stack a-exn)
               [ExceptionA (e) (ExceptionA e)]
               [BreakA () (BreakA)]
               [ReturnA (r) (ReturnA r)]
               [ValueA (obj-l)
                       (begin
                         (if (= (get-singleton-object 'Undefined) type-l)
                             (local
                              [(define-values (t-l o-l) (find-type-and-obj stack))]
                              (begin
                                (if (> debug-interp 2)
                                    (display
                                     (concat
                                      (list
                                       "\ntype: " (pretty t-l)
                                       "\nobj:  " (pretty o-l)
                                       "\n")))
                                    (void))
                                (set! type-l t-l)
                                (set! obj-l o-l)))
                             (void))
                         (if (= (get-singleton-object 'Undefined) type-l)
                             error
                             (if (= (get-singleton-object 'Undefined) obj-l)
                                 (create-super-obj type-l obj-l)
                                 (if (or (= (isinstance/impl obj-l type-l)
                                            (get-singleton-object 'True))
                                         (= (issubclass/impl obj-l type-l)
                                            (get-singleton-object 'True)))
                                     (create-super-obj type-l obj-l)
                                     (interp-error 'TypeError "obj not a subclass or subtype of type")))))])])))

(define (interp-funcdef name globals nonlocals wr-locals rd-locals args [defaults : (listof ExprC)] decorators body
                        env stack a-exn)
  (type-case AnswerC (interp-list (lookup/boot 'tuple) defaults #t env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [BreakA () (BreakA)]
    [ReturnA (r) (ReturnA r)]
    [ValueA (defs)
            (ValueA
             (store-object
              (ClosV (lookup/boot 'function)
                     (box
                      (list
                       (entV (obj-str "__name__")
                             (obj-str (symbol->string name)))
                       (entV (obj-str "__globals__")
                             (obj-tup
                              (map obj-str (map symbol->string globals))))
                       (entV (obj-str "__nonlocals__")
                             (obj-tup
                              (map obj-str (map symbol->string nonlocals))))
                       (entV (obj-str "__wr-locals__")
                             (obj-tup
                              (map obj-str (map symbol->string wr-locals))))
                       (entV (obj-str "__rd-locals__")
                             (obj-tup
                              (map obj-str
                                   (map symbol->string
                                        (filter (lambda (id)
                                                  (none? (table-lookup (modlE-names builtin-env) id)))
                                                rd-locals)))))
                       (entV (obj-str "__defaults__")
                             defs)
                       (entV (obj-str "__decorators__")
                             (obj-tup
                              (map obj-str (map symbol->string decorators))))))
                     args
                     body
                     env)))]))
(define (interp-while test body orelse env stack a-exn)
  (type-case AnswerC (interp-full test env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (v)
            (if (= (get-truth v)
                   (get-singleton-object 'True))
                (type-case AnswerC (interp-full body env stack a-exn)
                  [ExceptionA (e) (ExceptionA e)]
                  [ReturnA (r) (ReturnA r)]
                  [BreakA () (ValueA (get-singleton-object 'None))]
                  [ValueA (v) (interp-while test body orelse env stack a-exn)])
                (interp-full orelse env stack a-exn))]))

(define (interp-tostr [objC : ExprC] env stack a-exn)
  (type-case AnswerC (interp-full objC env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (v) (ValueA (store-object (StrV (lookup/boot 'str) (box empty) (pretty v))))]))

(define (interp-getitematidx [coll : ExprC] [idx : ExprC] env stack a-exn)
  (local
   [(define (extract-list data) data)
    (define (extract-dict data) (entV-key data))
    (define (extract-str chr)
      (store-object (StrV (lookup/boot 'str) (box empty) (list->string (list chr)))))
    (define (search-data data index cur extract)
      (if (empty? data)
          (interp-error 'IndexError "index too big")
          (if (= index cur)
              (ValueA (extract (first data)))
              (search-data (rest data) index (add1 cur) extract))))]
   (type-case AnswerC (interp-full idx env stack a-exn)
     [ExceptionA (e) (ExceptionA e)]
     [ReturnA (r) (ReturnA r)]
     [BreakA () (BreakA)]
     [ValueA (idx-loc)
             (type-case ObjV (fetch-object idx-loc)
               [NumV (c a index)
                     (type-case AnswerC (interp-full coll env stack a-exn)
                       [ExceptionA (e) (ExceptionA e)]
                       [ReturnA (r) (ReturnA r)]
                       [BreakA () (BreakA)]
                       [ValueA (coll-loc)
                               (type-case ObjV (fetch-object coll-loc)
                                 [DictV (c a fr data) (search-data (unbox data) index 0 extract-dict)]
                                 [ListV (c a fr data) (search-data (unbox data) index 0 extract-list)]
                                 [StrV (c a data) (search-data (string->list data) index 0 extract-str)]
                                 [else (interp-error 'TypeError "collection not a dict object")])])]
               [else (interp-error 'TypeError "index not a number")])])))

(define (interp-searchmro cls field env stack a-exn) : AnswerC
  (type-case AnswerC (interp-full cls env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (cls-loc)
            (type-case AnswerC (interp-full field env stack a-exn)
              [ExceptionA (e) (ExceptionA e)]
              [ReturnA (r) (ReturnA r)]
              [BreakA () (BreakA)]
              [ValueA (field-loc) (search-mro cls-loc field-loc)])]))

(define (interp-runcore [op : symbol] [args : (listof ExprC)] env stack a-exn) : AnswerC
  (local
   [(define (add-one table h)
      (if (empty? table)
          h
          (type-case Binding (first table)
            [lclB (id b-loc)
                  (begin
                    (if (> debug-interp 1)
                        (display
                         (concat
                          (list
                           "local id: "
                           (symbol->string id)
                           ": "
                           (pretty (unbox b-loc))
                           "\n")))
                        (void))
                    (if (= (unbox b-loc) (get-singleton-object 'Undefined))
                        (void)
                        (hsh-set h
                                 (obj-str (symbol->string id))
                                 (unbox b-loc)))
                    (add-one (rest table) h))]
            [else (add-one (rest table) h)])))
    (define (get-ans dict)
      (if (none? dict)
          (interp-error 'RuntimeError "asked for null environment listing!")
          (ValueA (store-object (DictV (lookup/boot 'dict) (box empty) #f (some-v dict))))))
    (define (compute-locals-dict env)
      (type-case Env env
        [modlE (n outer t) (some (add-one t (box empty)))]
        [nullE () (none)]
        [funcE (n outer t) (some (add-one t (box empty)))]
        [clssE (n outer t) (some (add-one t (box empty)))]))
    (define (compute-globals-dict env)
      (type-case Env env
        [modlE (n outer t) (compute-locals-dict env)]
        [nullE () (none)]
        [funcE (n outer t) (compute-globals-dict outer)]
        [clssE (n outer t) (compute-globals-dict outer)]))]
   (case op
     [(compute-bases) (interp-computebases (first args) env stack a-exn)]
     [(compute-mro) (interp-computemro (first args) env stack a-exn)]
     [(compute-locals) (get-ans (compute-locals-dict (first stack)))]
     [(compute-globals) (get-ans (compute-globals-dict (first stack)))]
     [else (interp-error 'Exception (string-append "Unknown core op: " (symbol->string op)))])))

(define (interp-computebases bases env stack a-exn)
  (type-case AnswerC (interp-full bases env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (v)
            (type-case ObjV (fetch-object v)
              [ListV (c d fr l)
                     (cond
                      [(empty? (unbox l))
                       (ValueA (store-object (ListV (lookup/boot 'tuple)
                                                    (box empty)
                                                    #t
                                                    (box (list (lookup/boot 'object))))))]
                      [(not (= (lookup/boot 'object)
                               (list-ref (unbox l) (sub1 (length (unbox l))))))
                       (ValueA (store-object (ListV (lookup/boot 'tuple)
                                                    (box empty)
                                                    #t
                                                    (box (append (unbox l)
                                                                 (list (lookup/boot 'object)))))))]
                      [else (ValueA v)])]
              [else (interp-error 'TypeError "bases are not a tuple")])]))

(define (interp-computemro [cls : ExprC] [env : Env] [stack : (listof Env)] a-exn) : AnswerC
  (local
   [(define (check (heads : (listof (listof Location)))
                   (lists : (listof (listof Location)))) : (optionof Location)
                   (if (empty? heads)
                       (none)
                       (if (foldl (lambda (l res)
                                    (and res
                                         (foldl (lambda (c res)
                                                  (and res (not (= c (first (first heads))))))
                                                #t
                                                (rest l))))
                                  #t
                                  lists)
                           (some (first (first heads)))
                           (none))))
    (define (prune [cand : Location] [lists : (listof (listof Location))]) : (listof (listof Location))
      (filter (lambda (l)
                (not (= 0 (length l))))
              (map (lambda (l)
                     (filter (lambda (c)
                               (not (= c cand))) l))
                   lists)))
    (define (merge [lists : (listof (listof Location))]) : (optionof (listof Location))
      (if (empty? lists)
          (some empty)
          (let ([cand (check lists lists)])
            (if (none? cand)
                (none)
                (let ([rest (merge (prune (some-v cand) lists))])
                  (if (none? rest)
                      rest
                      (some (cons (some-v cand) (some-v rest)))))))))
    (define (get-mro-list [bases : (listof Location)] [bl : (listof Location)]) : (listof (listof Location))
      (if (empty? bases)
          (list bl)
          (type-case ObjV (fetch-object (some-v (hsh-get (get-dict (first bases)) (obj-str "__mro__"))))
            [ListV (c d fr l) (cons (unbox l) (get-mro-list (rest bases) bl))]
            [else (error 'get-mro-list "base class does not have __mro__??!!")])))]
   (type-case AnswerC (interp-full cls env stack a-exn)
     [ExceptionA (e) (ExceptionA e)]
     [ReturnA (r) (ReturnA r)]
     [BreakA () (BreakA)]
     [ValueA (cls-loc)
             (if (= (lookup/boot 'type) (get-class cls-loc))
                 (type-case ObjV (fetch-object (some-v (hsh-get (get-dict cls-loc) (obj-str "__bases__"))))
                   [ListV (c d fr bases)
                          (let ([mro (merge (get-mro-list (unbox bases) (unbox bases)))])
                            (if (none? mro)
                                (interp-error 'TypeError "failed to resolve MRO")
                                (ValueA (store-object (ListV (lookup/boot 'tuple) (box empty) #t (box (cons cls-loc (some-v mro))))))))]
                   [else (interp-error 'TypeError "class does not have __bases__")])
                 (interp-error 'TypeError "cannot compute MRO for non-type instances"))])))

(define (interp-object [cls : ExprC] [attrs : ExprC] [data : (listof ExprC)] [env : Env] [stack : (listof Env)] a-exn)
  (type-case AnswerC (interp-full cls env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (cls-loc)
            (type-case ObjV (fetch-object cls-loc)
              [EmptyV (cls-type cl-attrs)
                      (if (= (lookup/boot 'type) cls-type)
                          (let ([cls-name
                                 (StrV-data (fetch-object
                                             (some-v (hsh-get cl-attrs
                                                              (obj-str "__name__")))))])
                            (type-case AnswerC (interp-full attrs env stack a-exn)
                              [ExceptionA (e) (ExceptionA e)]
                              [ReturnA (r) (ReturnA r)]
                              [BreakA () (BreakA)]
                              [ValueA (attr-loc)
                                      (begin
                                        (if (> debug-interp 2)
                                            (begin
                                              (display (pretty attr-loc))
                                              (display "\n")
                                              (display (DictV-cls (fetch-object attr-loc))))
                                            (void))
                                        (type-case ObjV (fetch-object attr-loc)
                                          [DictV (c a frozen? attr-d)
                                                 (type-case MultiValAnswerC (interp-expr-list data env stack a-exn)
                                                   [MVExceptionA (e) (ExceptionA e)]
                                                   [MVReturnA (r) (ReturnA r)]
                                                   [MVBreakA () (BreakA)]
                                                   [ValueListA (vlist) (create-obj cls-loc cls-name attr-d vlist env stack a-exn)])]
                                          [else (interp-error 'TypeError "attr not a dict object")]))]))
                          (interp-error 'TypeError (string-append (pretty cls-loc) " not a type")))]
              [else (interp-error 'TypeError (string-append (pretty cls-loc) "not an EmptyV obj"))])]))

(define (create-obj [cls-loc : Location] [cls-name : string]
                    [attr : Hash] [data : (listof Location)]
                    [env : Env] [stack : (listof Env)] a-exn)
  (local
   [(define (create-hash/impl data dict)
      (if (empty? data)
          dict
          (begin
            (hsh-set dict (first data) (second data))
            (create-hash/impl (rest (rest data)) dict))))
    (define (create-hash mappings)
      (create-hash/impl mappings (box empty)))
    (define (insert-none-val lst)
      (if (empty? lst)
          empty
          (cons (first lst)
                (cons (get-singleton-object 'None)
                      (insert-none-val (rest lst))))))]
   (cond
    [(= (get-singleton-object 'True)
        (issubclass/impl cls-loc (lookup/boot 'str)))
     (if (empty? data)
         (interp-error 'TypeError
                       (concat (list
                                "did not pass any data to create instance of '"
                                cls-name "' subclass of str")))
         (if (StrV? (fetch-object (first data)))
             (ValueA (store-object (StrV cls-loc
                                         attr
                                         (StrV-data (fetch-object (first data))))))
             (interp-error 'TypeError
                           (concat (list
                                    "did not pass a string to create instance of '"
                                    cls-name "' subclass of str")))))]
    [(= (get-singleton-object 'True)
        (issubclass/impl cls-loc (lookup/boot 'int)))
     (if (empty? data)
         (interp-error 'TypeError
                       (concat (list
                                "did not pass any data to create instance of '"
                                cls-name "' subclass of int")))
         (if (NumV? (fetch-object (first data)))
             (ValueA (store-object (NumV cls-loc
                                         attr
                                         (NumV-data (fetch-object (first data))))))
             (interp-error 'TypeError
                           (concat (list
                                    "did not pass a number to create instance of '"
                                    cls-name "' subclass of int")))))]
    [(= (get-singleton-object 'True)
        (issubclass/impl cls-loc (lookup/boot 'float)))
     (if (empty? data)
         (interp-error 'TypeError
                       (concat (list
                                "did not pass any data to create instance of '"
                                cls-name "' subclass of float")))
         (if (NumV? (fetch-object (first data)))
             (ValueA (store-object (NumV cls-loc
                                         attr
                                         (NumV-data (fetch-object (first data))))))
             (interp-error 'TypeError
                           (concat (list
                                    "did not pass a number to create instance of '"
                                    cls-name "' subclass of float")))))]
    [(= (get-singleton-object 'True)
        (issubclass/impl cls-loc (lookup/boot 'list)))
     (ValueA (store-object (ListV cls-loc
                                  attr #f
                                  (box data))))]
    [(= (get-singleton-object 'True)
        (issubclass/impl cls-loc (lookup/boot 'tuple)))
     (ValueA (store-object (ListV cls-loc
                                  attr #t
                                  (box data))))]
    [(= (get-singleton-object 'True)
        (issubclass/impl cls-loc (lookup/boot 'dict)))
     (if (odd? (length data))
         (interp-error 'TypeError
                       (concat (list
                                "did not pass equal number of keys and vals to create instance of '"
                                cls-name "' subclass of dict")))
         (ValueA (store-object (DictV cls-loc
                                      attr #f
                                      (create-hash data)))))]
    [(= (get-singleton-object 'True)
        (issubclass/impl cls-loc (lookup/boot 'set)))
     (ValueA (store-object (DictV cls-loc
                                  attr #f
                                  (create-hash (insert-none-val data)))))]
    [(= (get-singleton-object 'True)
        (issubclass/impl cls-loc (lookup/boot 'frozenset)))
     (ValueA (store-object (DictV cls-loc
                                  attr #t
                                  (create-hash (insert-none-val data)))))]
    [(= (get-singleton-object 'True)
        (issubclass/impl cls-loc (lookup/boot 'Exception)))
     (if (empty? data)
         (interp-error 'TypeError
                       (concat (list
                                "did not pass any data to create instance of '"
                                cls-name "' subclass of Exception")))
         (ValueA (store-object (ExnV cls-loc
                                     attr
                                     (first data)))))]
    [else (ValueA (store-object (EmptyV cls-loc attr)))])))

(define (interp-isinstance obj cls env stack a-exn)
  (type-case AnswerC (interp-full obj env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (obj-v)
            (type-case AnswerC (interp-full cls env stack a-exn)
              [ExceptionA (e) (ExceptionA e)]
              [ReturnA (r) (ReturnA r)]
              [BreakA () (BreakA)]
              [ValueA (cls-v)
                      (if (= (lookup/boot 'type) (get-class cls-v))
                          (ValueA (isinstance/impl obj-v cls-v))
                          (interp-error 'TypeError "second arg must be of type 'type' for isinstance"))])]))

(define (interp-issubclass cls super env stack a-exn)
  (type-case AnswerC (interp-full cls env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (cls-v)
            (if (= (lookup/boot 'type) (get-class cls-v))
                (type-case AnswerC (interp-full super env stack a-exn)
                  [ExceptionA (e) (ExceptionA e)]
                  [ReturnA (r) (ReturnA r)]
                  [BreakA () (BreakA)]
                  [ValueA (super-v)
                          (if (= (lookup/boot 'type) (get-class super-v))
                              (ValueA (issubclass/impl cls-v super-v))
                              (interp-error 'TypeError "second arg must be of type 'type' for issubclass"))])
                (interp-error 'TypeError "second arg must be of type 'type' for issubclass"))]))

(define (interp-len [obj : ExprC] [env : Env] [stack : (listof Env)] a-exn) : AnswerC
  (type-case AnswerC (interp-full obj env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (v) (type-case ObjV (fetch-object v)
                  [StrV (c d str) (ValueA (store-object (NumV (lookup/boot 'int) (box empty) (length (string->list str)))))]
                  [ListV (c d fr data) (ValueA (store-object (NumV (lookup/boot 'int) (box empty) (length (unbox data)))))]
                  [DictV (c d fr data) (ValueA (store-object (NumV (lookup/boot 'int) (box empty) (length (unbox data)))))]
                  [else (interp-error 'TypeError "not an iterable object")])]))

(define (interp-type [obj : ExprC] [env : Env] [stack : (listof Env)] a-exn) : AnswerC
  (type-case AnswerC (interp-full obj env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (v) (ValueA (get-class v))]))

;; interpret a try-catch block. first evaluate body and if no
;; exceptions are thrown, then return the result. Otherwise, bound the
;; exception to the environment, evaluate catch block and return
;; result
(define (interp-try-catch [body : ExprC] [param : symbol] [catch : ExprC]
                          [env : Env] [stack : (listof Env)] a-exn)
  (let ([exn-env (extend-env env (tmpB param (box (get-singleton-object 'Undefined))))])
    (type-case AnswerC (interp-full body exn-env stack a-exn)
      [ValueA (val) (ValueA val)]
      [ReturnA (r) (ReturnA r)]
      [BreakA () (BreakA)]
      [ExceptionA (exn)
                  (type-case AnswerC (update-env exn-env param exn)
                    [ValueA (v) (interp-full catch exn-env stack exn)]
                    [ReturnA (r) (ReturnA r)]
                    [BreakA () (BreakA)]
                    [ExceptionA (e) (ExceptionA e)])])))

;; interpret a try-finally block. first evaluate body and then evaluate finally.
;; taking care of returns and breaks
(define (interp-try-finally [body : ExprC] [finalBody : ExprC]
                            [env : Env] [stack : (listof Env)] a-exn)
  (type-case AnswerC (interp-full body env stack a-exn)
    [ValueA (val)
            (type-case AnswerC (interp-full finalBody env stack a-exn)
              [ValueA (v) (ValueA val)]
              [ExceptionA (e) (ExceptionA e)]
              [ReturnA (r) (ReturnA r)]
              [BreakA () (BreakA)])]
    [ReturnA (r)
             (type-case AnswerC (interp-full finalBody env stack a-exn)
               [ValueA (v) (ReturnA r)]
               [ExceptionA (e) (ExceptionA e)]
               [ReturnA (n-r) (ReturnA n-r)]
               [BreakA () (ReturnA r)])]
    [BreakA ()
            (type-case AnswerC (interp-full finalBody env stack a-exn)
              [ValueA (v) (BreakA)]
              [ExceptionA (e) (ExceptionA e)]
              [ReturnA (n-r) (ReturnA n-r)]
              [BreakA () (BreakA)])]
    [ExceptionA (e)
                (type-case AnswerC (interp-full finalBody env stack e)
                  [ValueA (v) (ExceptionA e)]
                  [ExceptionA (n-e) (ExceptionA n-e)]
                  [ReturnA (n-r) (ExceptionA e)]
                  [BreakA () (ExceptionA e)])]))

;; interpret the unary primary operations
(define (interp-prim1 [op : symbol] [arg : ExprC] [env : Env] [stack : (listof Env)] a-exn)
  (type-case AnswerC (interp-full arg env stack a-exn)
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ExceptionA (e) (ExceptionA e)]
    [ValueA (val) (python-prim1 op val)]))

;; evaluate the two args in order using interp-expr-list and finally
;; delegate the actual operation to racket
(define (interp-prim2 [op : symbol] [lhs : ExprC] [rhs : ExprC]
                      [env : Env] [stack : (listof Env)] a-exn)
  (type-case AnswerC (interp-full lhs env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (lhs-loc)
            (cond
             [(symbol=? op 'and) (if (= (get-truth lhs-loc) (get-singleton-object 'False))
                                     (ValueA (get-singleton-object 'False))
                                     (type-case AnswerC (interp-full rhs env stack a-exn)
                                       [ExceptionA (e) (ExceptionA e)]
                                       [ReturnA (r) (ReturnA r)]
                                       [BreakA () (BreakA)]
                                       [ValueA (rhs-loc) (truth rhs-loc)]))]
             [(symbol=? op 'or) (if (= (get-truth lhs-loc) (get-singleton-object 'True))
                                    (ValueA (get-singleton-object 'True))
                                    (type-case AnswerC (interp-full rhs env stack a-exn)
                                      [ReturnA (r) (ReturnA r)]
                                      [BreakA () (BreakA)]
                                      [ExceptionA (e) (ExceptionA e)]
                                      [ValueA (rhs-loc) (truth rhs-loc)]))]
             [else (type-case AnswerC (interp-full rhs env stack a-exn)
                     [ExceptionA (e) (ExceptionA e)]
                     [ReturnA (r) (ReturnA r)]
                     [BreakA () (BreakA)]
                     [ValueA (rhs-loc) (python-prim2 op lhs-loc rhs-loc)])])]))

;; interpret first the condition and in the context of the new store evaluate
;; then or else clauses
(define (interp-if [cnd : ExprC] [thn : ExprC] [els : ExprC]
                   [env : Env] [stack : (listof Env)] a-exn)
  (type-case AnswerC (interp-full cnd env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (test-v)
            (if (= (get-truth test-v) (get-singleton-object 'True))
                (interp-full thn env stack a-exn)
                (interp-full els env stack a-exn))]))

;; create a new env with the binding argument interpreted
;; and then interpret body in that environment
(define (interp-letc [id : symbol] [bind : ExprC] [body : ExprC]
                     [env : Env] [stack : (listof Env)] a-exn)
  (type-case AnswerC (interp-full bind env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (b-loc) (interp-full body (extend-env env (tmpB id (box b-loc))) stack a-exn)]))

;; find the location of the given id, interpret the new value and override
;; the resultant store at the location found earlier with the new val
(define (interp-setc [id : symbol] [expr : ExprC] [env : Env] [stack : (listof Env)] a-exn)
  (type-case AnswerC (interp-full expr env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (val-loc) (update-env env id val-loc)]))

;; interpret a function application
(define (interp-app [func : ExprC] [arguments : (listof ExprC)]
                    [env : Env] [stack : (listof Env)] a-exn)
  ;; recursively builds up the closure env and evaluates the body in that
  (type-case AnswerC (interp-full func env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (clos)
            (type-case MultiValAnswerC (interp-expr-list arguments env stack a-exn)
              [MVExceptionA (e) (ExceptionA e)]
              [MVReturnA (r) (ReturnA r)]
              [MVBreakA () (BreakA)]
              [ValueListA (v-list)
                          (execute-app (fetch-object clos) v-list (cons env stack) a-exn)])]))

(define (interp-dyn-app [func : ExprC] [args : ExprC] [env : Env] [stack : (listof Env)] a-exn)
  (type-case AnswerC (interp-full func env stack a-exn)
    [ExceptionA (e) (ExceptionA e)]
    [ReturnA (r) (ReturnA r)]
    [BreakA () (BreakA)]
    [ValueA (fun-loc)
            (type-case AnswerC (interp-full args env stack a-exn)
              [ExceptionA (e) (ExceptionA e)]
              [ReturnA (r) (ReturnA r)]
              [BreakA () (BreakA)]
              [ValueA (args-v)
                      (type-case ObjV (fetch-object args-v)
                        [ListV (c d fr l)
                               (execute-app (fetch-object fun-loc) (unbox l) (cons env stack) a-exn)]
                        [else (interp-error 'TypeError "dyn-app did not receive a list type")])])]))

(define (execute-app [closure : ObjV] [args : (listof Location)] stack a-exn)
  (local
   [(define error (BreakA))
    (define (read-str loc)
      (if (BreakA? error)
          (type-case ObjV (fetch-object loc)
            [StrV (c d s) (string->symbol s)]
            [else (begin
                    (set! error
                          (interp-error 'ValueError
                                        (string-append "can't interpret as symbol: "
                                                       (pretty loc))))
                    '\#nul)])
          '\#nul))
    (define (read-list [lst : Location] [attr : string])
      (if (BreakA? error)
          (type-case ObjV (fetch-object lst)
            [ListV (c a fr l)
                   (map read-str (unbox l))]
            [else (begin
                    (set! error
                          (interp-error 'ValueError
                                        (string-append attr
                                                       " not a list type for names list")))
                    empty)])
          empty))
    (define (get-syms [dict : Hash] [attr : symbol])
      (if (BreakA? error)
          (let ([loc-op (hsh-get dict (obj-str (symbol->string attr)))])
            (if (none? loc-op)
                empty
                (read-list (some-v loc-op) (symbol->string attr))))
          empty))
    (define (has-vararg? attrs)
      (begin
        (if (> debug-interp 3)
            (display
             (concat
              (list
               "has-vararg? got:\nattrs: "
               (pretty (obj-dict (unbox attrs))) "\nerr: "
               (to-string error) "\n")))
            (void))
        (if (BreakA? error)
            (type-case (optionof Location) (hsh-get attrs (obj-str "__has-vararg__"))
              [none () #f]
              [some (v) (if (= v (get-singleton-object 'True))
                            #t
                            #f)])
            #f)))
    (define (map-args syms args defaults [has-vararg? : boolean])
      (begin
        (if (> debug-interp 3)
            (display
             (concat
              (list
               "map-args got:\nsyms: "
               (to-string syms) "\nargs: "
               (to-string args) "\ndefs: "
               (to-string defaults) "\nvarg: "
               (to-string has-vararg?) "\nerr: "
               (to-string error) "\n")))
            (void))
        (if (BreakA? error)
            (if (empty? syms)
                (if (empty? args)
                    empty
                    (begin
                      (set! error (interp-error 'TypeError
                                                (string-append "too many arguments passed to "
                                                               (symbol->string (get-func-name (ClosV-dict closure))))))
                      empty))
                (if (and (empty? (rest syms)) has-vararg?)
                    (list (lclB (first syms)
                                (box (obj-tup args))))
                    (if (empty? args)
                        (if (= (first defaults)
                               (get-singleton-object 'Undefined))
                            (begin
                              (set! error (interp-error 'TypeError
                                                        (string-append "too few arguments passed to "
                                                                       (symbol->string (get-func-name (ClosV-dict closure))))))
                              empty)
                            (cons (lclB (first syms)
                                        (box (first defaults)))
                                  (map-args (rest syms) args (rest defaults) has-vararg?)))
                        (cons (lclB (first syms)
                                    (box (first args)))
                              (map-args (rest syms) (rest args) (rest defaults) has-vararg?)))))
            empty)))
    (define (get-func-name attrs)
      (if (BreakA? error)
          (type-case (optionof Location) (hsh-get attrs (obj-str "__name__"))
            [none () (begin
                       (set! error (interp-error 'AttributeError "__name__ not in closure object"))
                       '\#nul)]
            [some (v)
                  (type-case ObjV (fetch-object v)
                    [StrV (c d s) (string->symbol s)]
                    [else (begin
                            (set! error (interp-error 'ValueError "attr __name__ of closure not a string"))
                            '\#nul)])])
          '\#nul))
    (define (exclude-args locals args)
      (if (empty? locals)
          empty
          (if (foldl (lambda (a-id res)
                       (or res (symbol=? a-id (first locals))))
                     #f args)
              (exclude-args (rest locals) args)
              (cons (first locals)
                    (exclude-args (rest locals) args)))))
    (define (mapv->binding entv)
      (if (BreakA? error)
          (type-case ObjV (fetch-object (entV-key entv))
            [StrV (c d s)
                  (lclB (string->symbol s) (box (entV-val entv)))]
            [else (begin
                    (set! error (interp-error'ValueError "key is not of str type in __bindings__ of functions"))
                    (glbB '\#nul))])
          (glbB '\#nul)))
    (define (map-bindings attr)
      (if (BreakA? error)
          (type-case (optionof Location) (hsh-get attr (obj-str "__bindings__"))
            [none () empty]
            [some (v)
                  (type-case ObjV (fetch-object v)
                    [DictV (c a fr l)
                           (map mapv->binding (unbox l))]
                    [else (begin
                            (set! error (interp-error 'ValueError "__bindings__ of function not a dict"))
                            empty)])])
          empty))
    (define (get-enclosing-val id name cl-env)
      (type-case AnswerC (lookup-env (create-env name cl-env 'function) id)
        [ValueA (v) v]
        [ExceptionA (e) (get-singleton-object 'Undefined)]
        [ReturnA (r) (get-singleton-object 'Undefined)]
        [BreakA () (get-singleton-object 'Undefined)]))
    (define (prepend-undefs total defs)
      (if (= total (length defs))
          defs
          (cons (get-singleton-object 'Undefined)
                (prepend-undefs (sub1 total) defs))))
    (define (get-defaults attr num-args) : (listof Location)
      (type-case (optionof Location) (hsh-get attr (obj-str "__defaults__"))
        [none () (prepend-undefs num-args empty)]
        [some (l)
              (if (ListV? (fetch-object l))
                  (prepend-undefs num-args (unbox (ListV-data (fetch-object l))))
                  (begin (set! error (interp-error 'ValueError "__defaults__ of function not a list or tuple"))
                         empty))]))]
   (type-case ObjV closure
     [ClosV (c attrs syms body cl-env)
            (let* ([name (get-func-name attrs)]
                   [wr-locals (get-syms attrs '__wr-locals__)]
                   [rd-locals (get-syms attrs '__rd-locals__)]
                   [globals (get-syms attrs '__globals__)]
                   [nonlocals (get-syms attrs '__nonlocals__)]
                   [excl-locals (exclude-args wr-locals syms)]
                   [bindings (map-bindings attrs)]
                   [defaults (get-defaults attrs
                                           (if (has-vararg? attrs)
                                               (sub1 (length syms))
                                               (length syms)))]
                   [all-binds (append-all
                               (list
                                (map glbB globals)
                                (map nlcB nonlocals)
                                (map (lambda (id)
                                       (lclB id
                                             (box (get-enclosing-val id name cl-env))))
                                     rd-locals)
                                (map (lambda (id)
                                       (lclB id
                                             (box (get-singleton-object 'Undefined))))
                                     excl-locals)
                                bindings
                                (map-args syms args defaults (has-vararg? attrs))))])
              (if (BreakA? error)
                  (type-case AnswerC (interp-full body
                                                  (grow-env (create-env name
                                                                        cl-env 'function)
                                                            all-binds)
                                                  stack
                                                  a-exn)
                    [ExceptionA (e) (ExceptionA e)]
                    [BreakA () (interp-error 'RuntimeError "BreakA should have been handled by looping")]
                    [ReturnA (r) (ValueA r)]
                    [ValueA (v) (ValueA v)])
                  error))]
     [else (interp-error 'TypeError "did not get a closure for app")])))
