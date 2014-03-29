#lang plai-typed

(require (typed-in racket/base (raise-user-error : (string -> Location))))
(require "python-core-syntax.rkt")

(define debug-base 0)

(define (extend-env [env : Env] [bind : Binding]) : Env
  (local
   [(define (get-name bind)
      (type-case Binding bind
        [glbB (n) n]
        [nlcB (n) n]
        [lclB (n v) n]
        [tmpB (n v) n]))
    (define (new-table tab)
      (let ([old-bind (table-lookup tab (get-name bind))])
        (if (or (none? old-bind)
                (tmpB? bind))
            (cons bind tab)
            (error 'extend-env
                   (concat
                    (list
                     "trying to override non-tmp binding:"
                     (pretty-binding (some-v old-bind))
                     " with "
                     (pretty-binding bind)))))))]
   (type-case Env env
     [nullE ()        (error 'extend-env "no environment to extend")]
     [modlE (n o names) (modlE n o (new-table names))]
     [funcE (n o names) (funcE n o (new-table names))]
     [clssE (n o names) (clssE n o (new-table names))])))

(define (grow-env [env : Env] [binds : (listof Binding)])
  (if (empty? binds)
      env
      (grow-env (extend-env env (first binds))
                (rest binds))))

(define (create-env [name : symbol] [outer : Env] [type : symbol])
  (case type
    [(module)   (modlE name outer empty)]
    [(function) (funcE name outer empty)]
    [(class)    (clssE name outer empty)]))

(define (table-lookup table id)
  (if (empty? table)
      (none)
      (let ([cand (first table)])
        (cond
         [(and (glbB? cand) (symbol=? (glbB-name cand) id)) (some cand)]
         [(and (nlcB? cand) (symbol=? (nlcB-name cand) id)) (some cand)]
         [(and (lclB? cand) (symbol=? (lclB-name cand) id)) (some cand)]
         [(and (tmpB? cand) (symbol=? (tmpB-name cand) id)) (some cand)]
         [else (table-lookup (rest table) id)]))))

;; looks up the symbol in env, then global and finally in builtin and returns the location.
;; Will return the first instance.
;; none if not found
(define (update-env [env : Env] [id : symbol] [val : Location]) : AnswerC
  (local
   [(define (upd-err type str)
      (interp-error type (string-append (symbol->string id)
                                        (string-append ": " str))))
    (define (update/impl env honour-class? local-scope? global-scope?)
      (type-case Env env
        [nullE () (upd-err 'NameError "not defined in global scope")]
        [modlE (name outer table)
               (let ([b (table-lookup table id)])
                 (if (none? b)
                     (update/impl outer #f #f global-scope?)
                     (type-case Binding (some-v b)
                       [glbB (n) (upd-err 'RuntimeError "can't have global bindings in module scope")]
                       [nlcB (n) (upd-err 'RuntimeError "can't have nonlocal bindings in module scope")]
                       [lclB (n v) (begin (set-box! v val)
                                          (ValueA (get-singleton-object 'None)))]
                       [tmpB (n v) (begin (set-box! v val)
                                          (ValueA (get-singleton-object 'None)))])))]
        [funcE (name outer table)
               (if global-scope?
                   (update/impl outer #f #f #t)
                   (let ([b (table-lookup table id)])
                     (if (none? b)
                         (update/impl outer #f #f #f)
                         (type-case Binding (some-v b)
                           [glbB (n) (update/impl outer #f #f #t)]
                           [nlcB (n) (if (modlE? outer)
                                         (upd-err 'RuntimeError "should not have nonlocal bindings at top-level")
                                         (update/impl outer #f #f #f))]
                           [lclB (n v)
                                 (begin (set-box! v val)
                                        (ValueA (get-singleton-object 'None)))]
                           [tmpB (n v)
                                 (begin (set-box! v val)
                                        (ValueA (get-singleton-object 'None)))]))))]
        [clssE (name outer table)
               (if honour-class?
                   (let ([b (table-lookup table id)])
                     (if (none? b)
                         (update/impl outer #f #f #f)
                         (type-case Binding (some-v b)
                           [glbB (n) (update/impl outer #f #f #t)]
                           [nlcB (n) (if (modlE? outer)
                                         (upd-err 'RuntimeError "should not have nonlocal bindings at top-level")
                                         (update/impl outer #f #f #f))]
                           [lclB (n v)
                                 (begin (set-box! v val)
                                        (ValueA (get-singleton-object 'None)))]
                           [tmpB (n v)
                                 (begin (set-box! v val)
                                        (ValueA (get-singleton-object 'None)))])))
                   (update/impl outer #f #f global-scope?))]))]
   (update/impl env (clssE? env) (funcE? env) #f)))

;; looks up the symbol in env, then global and finally in builtin and returns the location.
;; Will return the first instance.
;; none if not found
(define (lookup-env [env : Env] [id : symbol]) : AnswerC
  (begin
    (if (> debug-base 0)
        (display
         (concat
          (list
           "\n-------------------------------------\n"
           (symbol->string id)
           "\n-------------------------------------\n"
           (type-case Env env
             [nullE () "NULL"]
             [modlE (n o t) (symbol->string n)]
             [funcE (n o t) (symbol->string n)]
             [clssE (n o t) (symbol->string n)]))))
        (void))
    (local
     [(define (lkup-err type str)
        (interp-error type (string-append (symbol->string id)
                                          (string-append ": " str))))
      (define (lookup/impl env honour-class? local-scope? global-scope?)
        (type-case Env env
          [nullE () (lkup-err 'NameError "not defined in global scope")]
          [modlE (name outer table)
                 (let ([b (table-lookup table id)])
                   (if (none? b)
                       (lookup/impl outer #f #f global-scope?)
                       (type-case Binding (some-v b)
                         [glbB (n) (lkup-err 'RuntimeError "can't have global bindings in module scope")]
                         [nlcB (n) (lkup-err 'RuntimeError "Can't have nonlocal bindings in module scope")]
                         [lclB (n v) (if (= (unbox v) (get-singleton-object 'Undefined))
                                         (lkup-err 'NameError "not defined in global scope")
                                         (ValueA (unbox v)))]
                         [tmpB (n v) (if (= (unbox v) (get-singleton-object 'Undefined))
                                         (lkup-err 'NameError "tmp object not initialized in global scope")
                                         (ValueA (unbox v)))])))]
          [funcE (name outer table)
                 (if global-scope?
                     (lookup/impl outer #f #f #t)
                     (let ([b (table-lookup table id)])
                       (if (none? b)
                           (lookup/impl outer #f #f #f)
                           (type-case Binding (some-v b)
                             [glbB (n) (lookup/impl outer #f #f #t)]
                             [nlcB (n) (if (modlE? outer)
                                           (lkup-err 'RuntimeError "should not have nonlocal bindings at top-level")
                                           (lookup/impl outer #f #f #f))]
                             [lclB (n v) (if (= (unbox v) (get-singleton-object 'Undefined))
                                             (if local-scope?
                                                 (lkup-err 'UnboundLocalError "local variable referenced before assignment")
                                                 (lkup-err 'NameError "free variable referenced before assignment in enclosing scope"))
                                             (ValueA (unbox v)))]
                             [tmpB (n v) (if (= (unbox v) (get-singleton-object 'Undefined))
                                             (lkup-err 'NameError "tmp object not initialized in local scope")
                                             (ValueA (unbox v)))]))))]
          [clssE (name outer table)
                 (if honour-class?
                     (let ([b (table-lookup table id)])
                       (if (none? b)
                           (lookup/impl outer #f #f #f)
                           (type-case Binding (some-v b)
                             [glbB (n) (lookup/impl outer #f #f #t)]
                             [nlcB (n) (if (modlE? outer)
                                           (lkup-err 'RuntimeError "should not have nonlocal bindings at top-level")
                                           (lookup/impl outer #f #f #f))]
                             [lclB (n v) (if (= (unbox v) (get-singleton-object 'Undefined))
                                             (lkup-err 'NameError "name not defined")
                                             (ValueA (unbox v)))]
                             [tmpB (n v) (if (= (unbox v) (get-singleton-object 'Undefined))
                                             (lkup-err 'NameError "tmp object not initialized in local scope")
                                             (ValueA (unbox v)))])))
                     (lookup/impl outer #f #f global-scope?))]))]
     (lookup/impl env (clssE? env) (funcE? env) #f))))

;; looks up the symbol in env, then global and finally in builtin and returns the location.
;; Will return the first instance.
;; none if not found
(define (delete-env [env : Env] [id : symbol]) : AnswerC
  (local
   [(define (dlt-err type str)
      (interp-error type (string-append (symbol->string id)
                                        (string-append ": " str))))
    (define (delete/impl env honour-class? local-scope? global-scope?)
      (type-case Env env
        [nullE () (dlt-err 'NameError "not defined in global scope")]
        [modlE (name outer table)
               (let ([b (table-lookup table id)])
                 (if (none? b)
                     (delete/impl outer #f #f global-scope?)
                     (type-case Binding (some-v b)
                       [glbB (n) (dlt-err 'RuntimeError "can't have global bindings in module scope")]
                       [nlcB (n) (dlt-err 'RuntimeError "Can't have nonlocal bindings in module scope")]
                       [lclB (n v) (if (= (unbox v) (get-singleton-object 'Undefined))
                                       (dlt-err 'NameError "not defined in global scope")
                                       (begin (set-box! v (get-singleton-object 'Undefined))
                                              (ValueA (get-singleton-object 'None))))]
                       [tmpB (n v) (if (= (unbox v) (get-singleton-object 'Undefined))
                                       (dlt-err 'NameError "tmp object not initialized in global scope")
                                       (begin (set-box! v (get-singleton-object 'Undefined))
                                              (ValueA (get-singleton-object 'None))))])))]
        [funcE (name outer table)
               (if global-scope?
                   (delete/impl outer #f #f #t)
                   (let ([b (table-lookup table id)])
                     (if (none? b)
                         (delete/impl outer #f #f #f)
                         (type-case Binding (some-v b)
                           [glbB (n) (delete/impl outer #f #f #t)]
                           [nlcB (n) (if (modlE? outer)
                                         (dlt-err 'RuntimeError "should not have nonlocal bindings at top-level")
                                         (delete/impl outer #f #f #f))]
                           [lclB (n v) (if (= (unbox v) (get-singleton-object 'Undefined))
                                           (if local-scope?
                                               (dlt-err 'UnboundLocalError "local variable referenced before assignment")
                                               (dlt-err 'NameError "free variable referenced before assignment in enclosing scope"))
                                           (begin (set-box! v (get-singleton-object 'Undefined))
                                                  (ValueA (get-singleton-object 'None))))]
                           [tmpB (n v) (if (= (unbox v) (get-singleton-object 'Undefined))
                                           (dlt-err 'NameError "tmp object not initialized in local scope")
                                           (begin (set-box! v (get-singleton-object 'Undefined))
                                                  (ValueA (get-singleton-object 'None))))]))))]
        [clssE (name outer table)
               (if honour-class?
                   (let ([b (table-lookup table id)])
                     (if (none? b)
                         (delete/impl outer #f #f #f)
                         (type-case Binding (some-v b)
                           [glbB (n) (delete/impl outer #f #f #t)]
                           [nlcB (n) (if (modlE? outer)
                                         (dlt-err 'RuntimeError "should not have nonlocal bindings at top-level")
                                         (delete/impl outer #f #f #f))]
                           [lclB (n v) (if (= (unbox v) (get-singleton-object 'Undefined))
                                           (dlt-err 'NameError "name not defined")
                                           (begin (set-box! v (get-singleton-object 'Undefined))
                                                  (ValueA (get-singleton-object 'None))))]
                           [tmpB (n v) (if (= (unbox v) (get-singleton-object 'Undefined))
                                           (dlt-err 'NameError "tmp object not initialized in local scope")
                                           (begin (set-box! v (get-singleton-object 'Undefined))
                                                  (ValueA (get-singleton-object 'None))))])))
                   (delete/impl outer #f #f global-scope?))]))]
   (delete/impl env (clssE? env) (funcE? env) #f)))

(define (concat [strs : (listof string)])
  (if (empty? strs)
      ""
      (if (empty? (rest strs))
          (first strs)
          (string-append (first strs) (concat (rest strs))))))

(define gen-sym
  (let ([n 0])
    (lambda (prefix)
      (begin (set! n (add1 n))
             (string->symbol
              (concat (list (symbol->string prefix) "_$!@" (to-string n))))))))

(define memory (make-hash empty))

;; return a location which is free in the current store. We are assuming unlimited memory
;; for the time being
(define store-object
  (let ([curr-index 2000])
    (lambda ([obj : ObjV]) : Location
            (begin
              (hash-set! memory curr-index obj)
              (set! curr-index (add1 curr-index))
              (sub1 curr-index)))))

(define (fetch-object [loc : Location]) : ObjV
  (some-v (hash-ref memory loc)))

(define (append l1 l2)
  (if (empty? l1)
      l2
      (cons (first l1) (append (rest l1) l2))))

(define (append-all lists)
  (foldl append empty lists))

(define (pretty-answer a)
  (type-case AnswerC a
    [ValueA (v) (string-append "Value: " (pretty v))]
    [ExceptionA (e) (string-append "Exception: " (pretty e))]
    [ReturnA (v) (string-append "Return: " (pretty v))]
    [BreakA () "Break"]))

(define (maps->list [maps : (listof MappingC)])
  (if (empty? maps)
      empty
      (cons (entC-key (first maps))
            (cons (entC-val (first maps)) (maps->list (rest maps))))))

(define (pretty-exprc e) : string
  (letrec
      ([pretty-items/c
        (lambda ([as-list? : boolean] [l : (listof ExprC)] [ind : string])
          (concat (list (if as-list? (concat (list "\n" ind "(list\n")) "\n")
                        (if (empty? l) ind (pretty/c (first l) (if as-list? (string-append ind "  ") ind)))
                        (if (or (empty? l) (empty? (rest l)))
                            ""
                            (concat (map (lambda (e)
                                           (string-append "\n"
                                                          (pretty/c e (if as-list? (string-append ind "  ") ind))))
                                         (rest l))))
                        (if as-list? ")" ""))))]
       [pretty-list/c
        (lambda ([as-list? : boolean] [type : string] [l : (listof ExprC)] [ind : string])
          (concat (list ind "(" type (pretty-items/c as-list? l (string-append ind "  ")) ")")))]
       [pretty-arg-list/c
        (lambda ([start? : boolean] [lst : (listof symbol)])
          (if (empty? lst)
              "[]"
              (if (empty? (rest lst))
		  (if start?
		      (string-append "["
				     (pretty-arg-list/c #f lst))
		      (string-append (symbol->string (first lst)) "]"))
                  (if start?
                      (string-append "["
				     (pretty-arg-list/c #f lst))
                      (string-append (symbol->string (first lst))
                                     (string-append ", "
                                                    (pretty-arg-list/c #f (rest lst))))))))]
       [pretty/c
        (lambda ([expr : ExprC] [ind : string]) : string
                (type-case ExprC expr
                  [ObjC (cls attrs data) (pretty-list/c #t
                                                        (concat (list "ObjC " (to-string cls) "\n"
                                                                      (pretty/c attrs (string-append ind "  "))))
                                                        data ind)]

                  [StrC (s) (concat (list ind "(StrC \"" s "\")"))]
                  [NumC (cls n) (concat (list ind "(NumC " (to-string cls) " " (to-string n) ")"))]

                  [TupleC (tup) (pretty-list/c #t "TupleC" tup ind)]
                  [ListC  (lst) (pretty-list/c #t "ListC" lst ind)]
                  #|
                  [BytesC  (cls : symbol) (expandable? : boolean) (b : string)]
                  [ByteArrayC  (cls : symbol) (expandable? : boolean) (b : string)]
                  |#
                  [DictC      (dict) (pretty-list/c #t "DictC" (maps->list dict) ind)]
                  [SetC       (s) (pretty-list/c #t "SetC" s ind)]
                  [FrozenSetC (s) (pretty-list/c #t "FrozenSetC" s ind)]

                  [LamC   (args body) (concat (list ind "(LamC "
                                                    (pretty-arg-list/c #t args)
                                                    "\n"
                                                    (pretty/c body (string-append ind "  "))
                                                    ")"))]
                  [ErrorC  (cls expr) (concat (list ind "(ErrorC " (to-string cls) " "
                                                    "\n"
                                                    (pretty/c expr (string-append ind "  "))
                                                    ")"))]
                  [DynErrorC (exn) (concat (list ind "(DynErrorC\n" (pretty/c exn (string-append ind "  ")) ")"))]

		  [SuperC (type obj) (pretty-list/c #f "SuperC" (list type obj) ind)]
		  
                  [NoneC () (string-append ind "(NoneC)")]
                  [TrueC () (string-append ind "(TrueC)")]
                  [FalseC () (string-append ind "(FalseC)")]
                  [EllipsisC () (string-append ind "(EllipsisC)")]

                  [UndefinedC () (string-append ind "(UndefinedC)")]

                  [LenC (coll) (concat (list ind "(LenC\n" (pretty/c coll (string-append ind "  ")) ")"))]
                  [ToStrC (obj) (concat (list ind "(ToStrC\n" (pretty/c obj (string-append ind "  ")) ")"))]
                  [GetCallableC (obj) (concat (list ind "(GetCallableC\n" (pretty/c obj (string-append ind "  ")) ")"))]
                  [CopyObjC (obj) (concat (list ind "(CopyObjC\n" (pretty/c obj (string-append ind "  ")) ")"))]
                  [BindC  (fun obj) (pretty-list/c #f "BindC" (list fun obj) ind)]
                  [GetAttrC  (obj field) (pretty-list/c #f "GetAttrC" (list obj field) ind)]
                  [SetAttr!C  (obj field val) (pretty-list/c #f "SetAttr!C" (list obj field val) ind)]
                  [DelAttr!C  (obj field) (pretty-list/c #f "DelAttr!C" (list obj field) ind)]
                  [SearchMroC  (obj field) (pretty-list/c #f "SearchMroC" (list obj field) ind)]

                  [GetItemAtIdxC (coll index) (pretty-list/c #f "GetItemAtIdxC" (list coll index) ind)]
                  [GetDataC     (coll index) (pretty-list/c #f "GetDataC" (list coll index) ind)]
                  [SetData!C    (coll index value) (pretty-list/c #f "SetData!C" (list coll index value) ind)]
                  [DelData!C    (coll index) (pretty-list/c #f "DelData!C" (list coll index) ind)]
                  [AppendData!C (vect obj) (pretty-list/c #f "AppendData!C" (list vect obj) ind)]

                  [AppC (func args) (concat (list ind "(AppC\n"
                                                  (pretty/c func (string-append ind "  "))
                                                  " "
                                                  (pretty-items/c #t args (string-append ind "  "))
                                                  ")"))]
                  [DynAppC (func args) (pretty-list/c #f "DynAppC" (list func args) ind)]

                  [ModuleC (name locs expr)
                           (concat
                            (list
                             ind "(ModuleC "
                             (symbol->string name) " "
                             (pretty-arg-list/c #t locs)
                             "\n"
                             (pretty/c expr (string-append ind "  "))
                             ")"))]
                  [FuncDefC (name gl nl wlc rlc ar defs decrs expr)
                            (concat
                             (list
                              ind "(FuncDefC "
                              (symbol->string name) " "
                              (pretty-arg-list/c #t gl)
                              " "
                              (pretty-arg-list/c #t nl)
                              " "
                              (pretty-arg-list/c #t wlc)
                              " "
                              (pretty-arg-list/c #t rlc)
                              " "
                              (pretty-arg-list/c #t ar)
			      " "
			      (pretty-arg-list/c #t decrs)
                              "\n"
			      (pretty-list/c #t "" defs ind)
			      "\n"
                              (pretty/c expr (string-append ind "  "))
                              ")"))]
                  [ClassDefC (name gl nl lc expr)
                             (concat
                              (list
                               ind "(ClassDefC "
                               (symbol->string name) " "
                               (pretty-arg-list/c #t gl)
                               " "
                               (pretty-arg-list/c #t nl)
                               " "
                               (pretty-arg-list/c #t lc)
                               "\n"
                               (pretty/c expr (string-append ind "  "))
                               ")"))]

                  [LetC (id bind expr) (concat
                                        (list
                                         ind "(LetC " (symbol->string id) "\n"
                                         (pretty/c bind (string-append ind "  "))
                                         "\n"
                                         (pretty/c expr (string-append ind "  "))
                                         ")"))]
                  [IdC (id) (concat (list ind "(IdC " (to-string id) ")"))]
                  [Del!C (id) (concat (list ind "(Del!C " (to-string id) ")"))]
                  [Set!C (id value) (concat (list ind "(Set!C " (to-string id) "\n"
                                                  (pretty/c value (string-append ind "  ")) ")"))]

                  [TypeC (value) (concat (list ind "(TypeC\n"
                                               (pretty/c value (string-append ind "  ")) ")"))]

                  [IfC (cnd then els) (pretty-list/c #f "IfC" (list cnd then els) ind)]
                  [SeqC (e1 e2) (pretty-list/c #f "SeqC" (list e1 e2) ind) ]
                  [WhileC (test body oe) (pretty-list/c #f "WhileC" (list test body oe) ind)]

                  [BreakC () (string-append ind "(BreakC)")]
                  [ReturnC (val) (concat
                                  (list
                                   ind "(ReturnC\n"
                                   (pretty/c val (string-append ind "  "))))]

                  [TryCatchC (body param catch) (concat (list ind "(TryCatchC\n"
                                                              (pretty/c body (string-append ind "  "))
                                                              "\n" ind (to-string param) "\n"
                                                              (pretty/c catch (string-append ind "  ")) ")"))]
                  [TryFinallyC (body catch) (concat (list ind "(TryFinallyC\n"
                                                          (pretty/c body (string-append ind "  "))
                                                          "\n"
                                                          (pretty/c catch (string-append ind "  ")) ")"))]

                  [Prim1C (op arg) (concat (list ind "(Prim1C " (to-string op) "\n"
                                                 (pretty/c arg (string-append ind "  ")) ")"))]
                  [Prim2C (op arg1 arg2) (concat (list ind "(Prim2C " (to-string op) "\n"
                                                       (pretty/c arg1 (string-append ind "  ")) "\n"
                                                       (pretty/c arg2 (string-append ind "  "))
                                                       ")"))]

                  [IsInstanceC (obj cls) (pretty-list/c #f "IsInstanceC" (list obj cls) ind)]
                  [IsSubclassC (cls super) (pretty-list/c #f "IsSubclassC" (list cls super) ind)]

                  [RunCoreC (op args) (pretty-list/c #t (string-append "RunCoreC " (symbol->string op)) args ind)]
                  ;;                  [ComputeMroC (cls) (concat (list ind "(ComputeMroC\n" (pretty/c cls (string-append ind "  ")) ")"))]
                  ))])
    (pretty/c e "")))

(define (pretty-binding b) : string
  (type-case Binding b
    [glbB (n) (string-append (symbol->string n) "[glbl]")]
    [nlcB (n) (string-append (symbol->string n) "[nlcl]")]
    [lclB (n v) (string-append (symbol->string n)
                               (string-append "[locl]: "
                                              (pretty (unbox v))))]
    [tmpB (n v) (string-append (symbol->string n)
                               (string-append "[temp]: "
                                              (pretty (unbox v))))]))

(define (pretty-env env) : string
  (local
   [(define (pretty-tab [lst : (listof Binding)]) : string
      (if (empty? lst)
          ""
          (concat (list
                   (pretty-binding (first lst))
                   "\n"
                   (pretty-tab (rest lst))))))
    (define (pretty-env/impl env suppress-builtin?)
      (type-case Env env
        [nullE () ""]
        [modlE (name outer tab)
               (if (and suppress-builtin? (symbol=? name '__builtin__))
                   ""
                   (concat
                    (list
                     (pretty-env/impl outer #t)
                     "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                     (symbol->string name)
                     " (Module environment)~~~~~~~~~~~~~~~~~~~~~\n"
                     (pretty-tab tab))))]
        [funcE (name outer tab)
               (concat
                (list
                 (pretty-env/impl outer #t)
                 "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                 (symbol->string name)
                 " (Function environment)~~~~~~~~~~~~~~~~~~~~~\n"
                 (pretty-tab tab)))]
        [clssE (name outer tab)
               (concat
                (list
                 (pretty-env/impl outer #t)
                 "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                 (symbol->string name)
                 " (ClassDef environment)~~~~~~~~~~~~~~~~~~~~~\n"
                 (pretty-tab tab)))]))]
   (pretty-env/impl env #f)))

(define (pretty [loc : Location]) : string
  (type-case ObjV (fetch-object loc)
    [UndefinedV () "undef. WTF!!. How? Why?"]
    [EmptyV (c f)
            (if (= c (lookup/boot 'type))
                (concat (list "type "
                              (let ([name (hsh-get f (obj-str "__name__"))])
                                (if (none? name)
                                    "malformed"
                                    (pretty (some-v name))))
                              " at " (to-string loc)))
                (concat (list
                         "object("
                         (pretty (store-object (DictV (lookup/boot 'dict) (box empty) #t f)))
                         ") at "
                         (to-string loc))))]
    [StrV (c f s) s]
    [NumV (c f n) (if (= c (lookup/boot 'bool))
                      (if (= n 0)
                          "False"
                          "True")
                      (to-string n))]
    [ListV (c f frozen? l)
           (local [(define (pretty-lst lst delimiter paren-close)
                     (if (empty? lst)
                         paren-close
                         (string-append delimiter
                                        (string-append (pretty (first lst))
                                                       (pretty-lst (rest lst) " ," paren-close)))))]
                  (if frozen?
                      (string-append "(" (pretty-lst (unbox l) "" ")"))
                      (string-append "[" (pretty-lst (unbox l) "" "]"))))]
    [DictV (c f frozen? d)
           (letrec
               ([pretty-ent (if (or (= c (lookup/boot 'set))
                                    (= c (lookup/boot 'frozenset)))
                                (lambda (mapping) (pretty (entV-key mapping)))
                                (lambda (mapping) (string-append (pretty (entV-key mapping))
                                                                 (string-append ": "
                                                                                (pretty (entV-val mapping))))))]
                [pretty-dict (lambda  (lst delimiter)
                               (if (empty? lst)
                                   "}"
                                   (string-append delimiter
                                                  (string-append (pretty-ent (first lst))
                                                                 (pretty-dict (rest lst) ", ")))))])
             (string-append "{" (pretty-dict (unbox d) "")))]
    [ClosV (c f a b e) (concat (list "function"
                                     (to-string a) " at " (to-string loc)
                                     " ,attrs: "
                                     (pretty (store-object (DictV (lookup/boot 'dict) (box empty) #t f)))))]
    [ExnV (c f d) (string-append
                   (let ([name (hsh-get (EmptyV-dict (fetch-object c)) (obj-str "__name__"))])
                     (if (none? name)
                         "malformed"
                         (pretty (some-v name))))
                   (string-append ": " (pretty d)))]))

(define (get-dict [obj : Location]) : Hash
  (type-case ObjV (fetch-object obj)
    [UndefinedV () (error 'get-dict "Something terrible. attr lookup on undefined")]
    [EmptyV (c d) d]
    [StrV   (c d s) d]
    [NumV   (c d n) d]
    [ListV  (c d b l) d]
    [DictV  (c d b da) d]
    [ClosV  (c d a b e) d]
    [ExnV   (c d l) d]))

(define (get-class [obj : Location]) : Location
  (type-case ObjV (fetch-object obj)
    [UndefinedV () (error 'get-dict "Something terrible. class lookup on undefined")]
    [EmptyV (c d) c]
    [StrV   (c d s) c]
    [NumV   (c d n) c]
    [ListV  (c d b l) c]
    [DictV  (c d b da) c]
    [ClosV  (c d a b e) c]
    [ExnV   (c d l) c]))


;; looks up the symbol in builtin and returns the location.
;; Will return the first instance.
;; none if not found
(define (lookup/boot [id : symbol]) : Location
  (type-case (optionof Binding) (table-lookup (modlE-names builtin-env) id)
    [none () (error 'lookup/boot "RuntimeError: undefined builtin variable requested at boot")]
    [some (v)
          (type-case Binding v
            [lclB (n v) (unbox v)]
            [else (error 'lookup/boot "RuntimeError: builtin variable requested at boot not local")])]))

(define (create-bind-list [syms : (listof symbol)] [start : number])
  (if (empty? syms)
      empty
      (let ([bottom (create-bind-list (rest syms) start)])
        (if (empty? bottom)
            (list (lclB (first syms) (box start)))
            (cons (lclB (first syms)
                        (box (add1 (unbox (lclB-value (first bottom))))))
                  bottom)))))

;; builtin names, functions and types
(define builtin-env
  (modlE '__builtin__ (nullE)
         (append
          ;; builtin functions
          (append
           (create-bind-list
            (list
	     'next
             'locals
             'globals
             'any
             'all
             'callable
             'min
             'max
             'len
             'isinstance
             'issubclass
             'print
             'abs
             '___assertTrue
             '___assertFalse
             '___assertIn
             '___assertNotIn
             '___assertEqual
             '___assertNotEqual
             '___assertIs
             '___assertIsNot
             '___assertFail
             '___assertRaises
             ) 200)
           ;; constants
           (create-bind-list
            (list
             'Ellipsis
             'False
             'True
             'None
             'Undefined) 100))
          ;; builtin types
          (create-bind-list
           (list
            'iter
            'range
            'map
            'filter
            'ValueError
            'StopIteration
            'ZeroDivisionError
            'IndexError
            'KeyError
            'AttributeError
            'TypeError
            'NameError
            'RuntimeError
            'AssertionError
            'UnboundLocalError
            'Exception
            'ellipsis
            'NoneType
            'slice
            'float
            'frozenset
            'set
            'dict
            'tuple
            'list
            'function
            'str
            'int
            'bool
	    'super
            'type
            'object) 10))))

(define (interp-error [type : symbol] [msg : string]) : AnswerC
  (ExceptionA (store-object (ExnV (lookup/boot type) (box empty) (store-object (StrV (lookup/boot 'str) (box empty) msg))))))

(define-type singleton
  [objS (loc : Location) (data : ObjV)])

(define (get-singleton-object name) : Location
  (case name
    [(None Ellipsis True False Undefined) (lookup/boot name)]
    [else (error 'get-singleton-object
                 (string-append (symbol->string name)
                                " is not of singleton type"))]))

(define (get-slice-val [attr : symbol] [sl : ObjV])
  (type-case ObjV sl
    [EmptyV (c dict)
            (let ([res (hsh-get dict (obj-str (symbol->string attr)))])
              (if (none? res)
                  (error 'get-slice-val (string-append "slice does not have attr: "
                                                       (symbol->string attr)))
                  (if (= (get-singleton-object 'None) (some-v res))
                      (none)
                      (type-case ObjV (fetch-object (some-v res))
                        [NumV (c d n) (some n)]
                        [else (error 'get-slice-val "slice problem")]))))]
    [else (error 'get-slice-val "not a slice object passed")]))

(define (normalize-slice [sl : ObjV] [length : number])
  (let* ( [step (if (none? (get-slice-val '__step__ sl))
                    1
                    (some-v (get-slice-val '__step__ sl)))]
          [start (if (none? (get-slice-val '__start__ sl))
                     (if (< step 0) -1 0)
                     (some-v (get-slice-val '__start__ sl)))]
          [stop (if (none? (get-slice-val '__stop__ sl))
                    (if (< step 0) (- 0 (add1 length)) length)
                    (some-v (get-slice-val '__stop__ sl)))])
    (begin
      (if (> debug-base 1)
          (display (concat (list
                            "normalize-slice: start: "
                            (to-string start)
                            ", stop: " (to-string stop)
                            ", step: " (to-string step) "\n" )))
          (void))
      (if (>= step 0)
          (if (>= stop 0)
              (if (>= stop length)
                  (if (>= start 0)
                      (if (>= start length)
                          (values 0 0 step)
                          (values start length step))
                      (if (<= start (- 0 length))
                          (values 0 length step)
                          (values (+ length start) length step)))
                  (if (>= start 0)
                      (if (>= start stop)
                          (values 0 0 1)
                          (values start length step))
                      (if (<= start (- 0 length))
                          (values 0 stop step)
                          (if (< start (- length stop))
                              (values start (- length stop) step)
                              (values 0 0 step)))))
              (if (<= stop (- 0 length))
                  (values 0 0 step)
                  (if (>= start 0)
                      (if (>= start length)
                          (values 0 0 step)
                          (if (< start (+ length stop))
                              (values start (+ length stop) step)
                              (values 0 0 step)))
                      (if (< start (- 0 length))
                          (values 0 0 step)
                          (if (< start stop)
                              (values start stop step)
                              (values 0 0 step))))))
          (if (>= stop 0)
              (if (>= stop (sub1 length))
                  (values 0 0 step)
                  (if (>= start 0)
                      (if (> start stop)
                          (values start stop step)
                          (values 0 0 step))
                      (if (< start (- 0 length))
                          (values 0 0 step)
                          (if (> start (- stop length))
                              (values start (- stop length) step)
                              (values 0 0 step)))))
              (if (<= stop (- 0 length))
                  (if (>= start 0)
                      (if (>= start length)
                          (values -1 (sub1 (- 0 length)) step)
                          (values (- start length) (sub1 (- 0 length)) step))
                      (if (< start (- 0 length))
                          (values 0 0 step)
                          (values start (sub1 (- 0 length)) step)))
                  (if (>= start 0)
                      (if (>= start length)
                          (values -1 stop step)
                          (if (> (- start length) stop)
                              (values (- start length) stop step)
                              (values 0 0 step)))
                      (if (< start (- 0 length))
                          (values 0 0 step)
                          (if (> start stop)
                              (values start stop step)
                              (values 0 0 step))))))))))

(define (get-slice lst slice)
  (local
   [(define-values (start stop step)
      (normalize-slice slice (length lst)))
    (define (get-next-val idx)
      (cond
       [(and (> step 0) (>= idx stop)) empty]
       [(and (< step 0) (<= idx stop)) empty]
       [else (cons (if (< idx 0)
                       (list-ref lst (+ (length lst) idx))
                       (list-ref lst idx))
                   (get-next-val (+ idx step)))]))]
   (begin
     (if (> debug-base 1)
         (display (concat (list
                           "get-slice: start: "
                           (to-string start)
                           ", stop: " (to-string stop)
                           ", step: " (to-string step) "\n" )))
         (void))
     (get-next-val start))))

(define (get-elem-at-index lst index)
  (local
   [(define (check-and-get idx)
      (if (or (< idx 0) (> idx (length lst)))
          (none)
          (some (list-ref lst idx))))]
   (if (< index 0)
       (begin
         (set! index (+ index (length lst)))
         (check-and-get index))
       (check-and-get index))))

(define (str-get [str : string] [i-loc : Location]) : (optionof Location)
  (let ([idx (fetch-object i-loc)])
    (type-case ObjV idx
      [NumV (c d n)
            (type-case (optionof char) (get-elem-at-index (string->list str) n)
              [none () (none)]
              [some (c) (some (store-object (StrV (lookup/boot 'str)
                                                  (box empty)
                                                  (list->string (list c)))))])]
      [EmptyV (c d)
              (if (= c (lookup/boot 'slice))
                  (some (store-object (StrV (lookup/boot 'str)
                                            (box empty)
                                            (list->string (get-slice (string->list str) idx)))))
                  (none))]
      [else (none)])))

(define (hsh-set [tbl : Hash] [key : Location] [val : Location]) : void
  (local [(define (find-and-replace lst)
            (if (empty? lst)
                (list (entV key val))
                (if (= (test-equal key (entV-key (first lst)))
                       (get-singleton-object 'True))
                    (cons (entV key val) (rest lst))
                    (cons (first lst) (find-and-replace (rest lst))))))]
         (set-box! tbl (find-and-replace (unbox tbl)))))

(define (hsh-del [tbl : Hash] [key : Location]) : void
  (local [(define (find-and-delete lst)
            (if (empty? lst)
                empty
                (if (= (test-equal key (entV-key (first lst)))
                       (get-singleton-object 'True))
                    (rest lst)
                    (cons (first lst) (find-and-delete (rest lst))))))]
         (set-box! tbl (find-and-delete (unbox tbl)))))

(define (hsh-get [tbl : Hash] [key : Location]) : (optionof Location)
  (let ([lst (unbox tbl)])
    (if (empty? lst)
        (none)
        (if (= (test-equal key (entV-key (first lst)))
               (get-singleton-object 'True))
            (some (entV-val (first lst)))
            (hsh-get (box (rest (unbox tbl))) key)))))

(define (vec-append [vec : Vector] [val : Location]) : void
  (local [(define (append lst)
            (if (empty? lst)
                (list val)
                (cons (first lst) (append (rest lst)))))]
         (set-box! vec (append (unbox vec)))))

(define (vec-set [vec : Vector] [key : number] [val : Location]) : void
  (local [(define (find-and-replace lst idx)
            (if (= 0 idx)
                (cons val (rest lst))
                (cons (first lst) (find-and-replace (rest lst) (sub1 idx)))))]
         (set-box! vec (find-and-replace (unbox vec) key))))

(define (vec-del [vec : Vector] [key : number]) : void
  (local [(define (find-and-delete lst idx)
            (if (= 0 idx)
                (rest lst)
                (cons (first lst) (find-and-delete (rest lst) (sub1 idx)))))]
         (set-box! vec (find-and-delete (unbox vec) key))))

(define (vec-get [vec : Vector] [idx-loc : Location]) : (optionof (listof Location))
  (let ([idx (fetch-object idx-loc)])
    (type-case ObjV idx
      [NumV (c d n)
            (type-case (optionof Location) (get-elem-at-index (unbox vec) n)
              [none () (none)]
              [some (loc) (some (list loc))])]
      [EmptyV (c d)
              (if (= c (lookup/boot 'slice))
                  (some (get-slice (unbox vec) idx))
                  (none))]
      [else (none)])))

(define (test-equal [l : Location] [r : Location]) : Location
  (let ([l-val (fetch-object l)]
        [r-val (fetch-object r)])
    (cond
     [(= l r) (get-singleton-object 'True)]
     [else
      (type-case ObjV l-val
        [NumV (lc f ld)
              (type-case ObjV r-val
                [NumV (rc f rd) (if (= ld rd)
                                    (get-singleton-object 'True)
                                    (get-singleton-object 'False))]
                [else (get-singleton-object 'False)])]
        [StrV (lc f ld)
              (type-case ObjV r-val
                [StrV (rc f rd) (if (string=? ld rd)
                                    (get-singleton-object 'True)
                                    (get-singleton-object 'False))]
                [else (get-singleton-object 'False)])]
        [ListV (lc f lfr ld)
               (type-case ObjV r-val
                 [ListV (rc f rfr rd)
                        (if (not (equal? lfr rfr))
                            (get-singleton-object 'False)
                            (local [(define (eat-both ll rl)
                                      (if (empty? ll)
                                          (if (empty? rl)
                                              (get-singleton-object 'True)
                                              (get-singleton-object 'False))
                                          (if (empty? rl)
                                              (get-singleton-object 'False)
                                              (if (= (test-equal (first ll) (first rl))
                                                     (get-singleton-object 'False))
                                                  (get-singleton-object 'False)
                                                  (eat-both (rest ll) (rest rl))))))]
                                   (eat-both (unbox ld) (unbox rd))))]
                 [else (get-singleton-object 'False)])]
        [DictV (lc f lfr ld)
               (type-case ObjV r-val
                 [DictV (rc f rfr rd)
                        (cond
                         [(not (equal? lfr rfr)) (get-singleton-object 'False)]
                         [(not (= (length (unbox ld)) (length (unbox rd)))) (get-singleton-object 'False)]
                         [else
                          (local [(define (eat-left ll)
                                    (if (empty? ll)
                                        (get-singleton-object 'True)
                                        (type-case (optionof Location) (hsh-get rd (entV-key (first ll)))
                                          [none () (get-singleton-object 'False)]
                                          [some (v)
                                                (if (= (test-equal (entV-val (first ll)) v)
                                                       (get-singleton-object 'True))
                                                    (eat-left (rest ll))
                                                    (get-singleton-object 'False))])))]
                                 (eat-left (unbox ld)))])]
                 [else (get-singleton-object 'False)])]
        [else (get-singleton-object 'False)])])))

(define (obj-int n) : Location
  (store-object (NumV (lookup/boot 'int) (box empty) n)))

(define (obj-str s) : Location
  (store-object (StrV (lookup/boot 'str) (box empty) s)))

(define (obj-tup l) : Location
  (store-object (ListV (lookup/boot 'tuple) (box empty) #t (box l))))

(define (obj-list l) : Location
  (store-object (ListV (lookup/boot 'list) (box empty) #f (box l))))

(define (obj-dict d) : Location
  (store-object (DictV (lookup/boot 'dict) (box empty) #f (box d))))
