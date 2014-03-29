#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "python-base.rkt"
         "python-bootstrap.rkt"
         "python-primitives.rkt"
         "python-lib.rkt")

(define debug-surface 0)

(define unimplemented-exception (ErrorC 'Exception (StrC "not yet implemented")))

(define (ds-error str) (ErrorC 'Exception (StrC str)))

(define (desugar [mod : ModP]) : ExprC
  (let ([res (type-case ModP mod
               [Expression (e) (ds-expr e)]
               [Module (body)
                       (let ([globals
                              (dedup
                               (append (append-all (map extract-assigned-names body))
                                       (append-all (map extract-all-globals body))))])
                         (ModuleC '__main__ globals
                                  (ds-stmts body)))]
               [else unimplemented-exception])])
    (if (> debug-surface 1)
        (begin
          (display mod)
          (display "\n\n")
          (display (pretty-exprc res))
          res)
        res)))

;; think hard about how and when to process and pass along list of nonlocals
(define (ds-stmt [stmt : StmtP])
  (begin (if (> debug-surface 0)
             (begin
               (display (to-string stmt))
               (display "\n=================================================================\n\n"))
             (void))
         (type-case StmtP stmt
           [FunctionDef (name args body decorator_list returns)
                        (Set!C name (ds-func-stmt name args body decorator_list))]
           [ClassDef (name bases keywords starargs kwargs body decorator_list)
                     (Set!C name (ds-cls-stmt name bases body))]
           [Return (value) (ReturnC (ds-expr value))]
           [Delete (targets) (seq/c (map ds-del-stmt targets))]
           [Assign (targets value) (ds-assign-stmt targets value)]
           [AugAssign (target op value) (ds-augassign-stmt target op value)]
           [For (target iter body orelse) (ds-for-stmt target iter body orelse)]
           [While (test body orelse) (ds-while-stmt test body orelse)]
           [If (test body orelse)
               (IfC (ds-expr test)
                    (ds-stmts body)
                    (if (empty? orelse)
                        (NoneC)
                        (ds-stmts orelse)))]
           [Raise (exc cause) (ds-raise-stmt exc cause)]
           [TryExcept (body handlers orelse)
                      (ds-tryexcept-stmt body handlers orelse)]
           [TryFinally (body finalbody)
                       (TryFinallyC (ds-stmts body) (ds-stmts finalbody))]
           [Expr (v) (ds-expr v)]
           [Global (n) (NoneC)]
           [Nonlocal (n) (NoneC)]
           [Pass () (NoneC)]
           [Break () (BreakC)]
           [else unimplemented-exception])))

(define (ds-stmts body)
  (seq/c (map ds-stmt body)))

(define (ds-raise-stmt exc cause)
  (if (none? exc)
      (DynErrorC (UndefinedC))
      (if (Name? (some-v exc))
          (DynErrorC (call-nullary/c (IdC (Name-id (some-v exc)))))
          (DynErrorC (ds-expr (some-v exc))))))

(define (ds-for-stmt [targetP : ExprP] [iterP : ExprP] [body : (listof StmtP)] [orelse : (listof StmtP)])
  (let ([iter (gen-sym 'iter)]
        [keep-looping (gen-sym 'keep-looping)]
        [exn (gen-sym 'exn)])
    (LetC iter (call-args/c
                (IdC 'iter) (list (ds-expr iterP)))
          (LetC keep-looping (TrueC)
                (WhileC (IdC keep-looping)
                        (TryCatchC
                         (SeqC
                          (ds-assign-single targetP
                                            (call-nullary/c
                                             (get-attr/c (IdC iter) '__next__)))
                          (ds-stmts body))
                         exn
                         (IfC (IsInstanceC (IdC exn) (IdC 'StopIteration))
                              (Set!C keep-looping (FalseC))
                              (DynErrorC (IdC exn))))
                        (ds-stmts orelse))))))

(define (ds-while-stmt [testP : ExprP] [bodyS : (listof StmtP)] [orelseS : (listof StmtP)]
                       )
  (WhileC (ds-expr testP)
          (ds-stmts bodyS)
          (ds-stmts orelseS)))

(define (ds-tryexcept-stmt body handlers orelse)
  (let ([excepted? (gen-sym 'excepted?)]
        [exn-sym (gen-sym 'exn-sym)])
    (LetC excepted? (FalseC)
          (SeqC
           (TryCatchC
            (ds-stmts body)
            exn-sym
            (SeqC
             (Set!C excepted? (TrueC))
             (ds-handlers exn-sym handlers)))
           (IfC (IdC excepted?)
                (NoneC)
                (ds-stmts orelse))))))

(define (ds-handlers exn-sym handlers)
  (local
   [(define cand-set (gen-sym 'cand-set))
    (define cand-idx (gen-sym 'cand-idx))
    (define search-lst (gen-sym 'search-lst))
    (define (match handler) : ExprC
      (if (none? (Excepthandler-type handler))
          (TrueC)
          (LetC cand-set (ds-expr (some-v (Excepthandler-type handler)))
                (IfC (IsSubclassC (IdC cand-set) (IdC 'Exception))
                     (IfC (IsInstanceC (IdC exn-sym) (IdC cand-set))
                          (TrueC)
                          (FalseC))
                     (LetC search-lst
                           (LamC (list cand-idx) (ErrorC 'Exception (StrC "dummy")))
                           (SeqC
                            (Set!C search-lst
                                   (LamC (list cand-idx)
                                         (IfC (Prim2C 'eq
                                                      (IdC cand-idx)
                                                      (LenC (IdC cand-set)))
                                              (FalseC)
                                              (IfC (IsInstanceC (IdC exn-sym)
                                                                (GetDataC (IdC cand-set) (IdC cand-idx)))
                                                   (TrueC)
                                                   (AppC (IdC search-lst)
                                                         (list (Prim2C 'add
                                                                       (IdC cand-idx)
                                                                       (NumC 'int 1))))))))
                            (AppC (IdC search-lst) (list (NumC 'int 0)))))))))]
   (if (empty? handlers)
       (DynErrorC (NoneC))
       (let ([handler (first handlers)])
         (IfC (match handler)
              (if (none? (Excepthandler-name handler))
                  (ds-stmts (Excepthandler-body handler))
                  (SeqC
                   (Set!C (some-v (Excepthandler-name handler))
                          (IdC exn-sym))
                   (ds-stmts (Excepthandler-body handler))))
              (ds-handlers exn-sym (rest handlers)))))))

(define (ds-augassign-stmt target op value)
  (ds-assign-single target
                    (Prim2C (op->sym op)
                            (ds-expr target)
                            (ds-expr value))))

(define (ds-assign-single target valC) : ExprC
  (local
   [(define iter (gen-sym 'iter))
    (define (assign-list lst)
      (if (empty? lst)
          (NoneC)
          (SeqC
           (ds-assign-single (first lst)
                             (call-nullary/c (get-attr/c (IdC iter) '__next__)))
           (assign-list (rest lst)))))]
   (type-case ExprP target
     [Attribute (obj attr ctx) (set-attr/c (ds-expr obj) attr valC)]
     [Subscript (coll index ctx) (SetData!C (ds-expr coll) (ds-slice index) valC)]
     [Name (id ctx) (Set!C id valC)]
     [List (l c) (LetC iter (call-nullary/c (get-attr/c valC '__iter__))
                       (assign-list l))]
     [Tuple (l c) (LetC iter (call-nullary/c (get-attr/c valC '__iter__))
                        (assign-list l))]
     [else (ErrorC 'SyntaxError (StrC "can't assign random"))])))

(define (ds-lval-single target) : ExprC
  (type-case ExprP target
    [Attribute (obj attr ctx) (get-attr/c (ds-expr obj) attr)]
    [Subscript (coll index ctx) (GetDataC (ds-expr coll) (ds-slice index))]
    [Name (id ctx) (IdC id)]
    [List (l c) (ListC (map ds-lval-single l))]
    [Tuple (l c) (ListC (map ds-lval-single l))]
    [else (ErrorC 'SyntaxError (StrC "can't assign random"))]))

(define (ds-assign-stmt targets value) : ExprC
  (if (empty? targets)
      (ds-error "nothing to assign to")
      (if (empty? (rest targets))
          (ds-assign-single (first targets) (ds-expr value))
          (SeqC (ds-assign-stmt (rest targets) value)
                (ds-assign-single (first targets) (ds-lval-single (first (rest targets))))))))

(define (ds-del-stmt [target : ExprP])
  (local
   [(define (del-list lst)
      (if (empty? lst)
          (NoneC)
          (SeqC
           (ds-del-stmt (first lst))
           (del-list (rest lst)))))]
   (type-case ExprP target
     [Attribute (obj attr ctx) (DelAttr!C (ds-expr obj) (StrC (symbol->string attr)))]
     [Subscript (coll index ctx) (DelData!C (ds-expr coll) (ds-slice index))]
     [Name (id ctx) (Del!C id)]
     [List (l c) (del-list l)]
     [Tuple (l c) (del-list l)]
     [else (ErrorC 'SyntaxError (StrC "can't delete random"))])))

(define (not-in-list list id)
  (if (empty? list)
      #t
      (if (symbol=? (first list) id)
          #f
          (not-in-list (rest list) id))))

(define (ds-cls-stmt [name : symbol] [bases : (listof ExprP)] [body : (listof StmtP)]) : ExprC
  (let* ([assigned-names (dedup (append-all (map extract-assigned-names body)))]
         [globals (dedup (append-all (map extract-globals body)))]
         [nonlocals (dedup (append-all (map extract-nonlocals body)))]
         [locals (dedup (filter (lambda (id)
                                  (not-in-list (append globals nonlocals) id))
                                assigned-names))])
    (ClassDefC name globals nonlocals locals
               (SeqC
                (seq/c
                 (map (lambda (s)
                        (ds-stmt s))
                      body))
                (AppC (GetCallableC (IdC 'type))
                      (list
                       (StrC (symbol->string name))
                       (TupleC (map ds-expr bases))
                       (call-nullary/c (IdC 'locals))))))))

(define (ds-func-stmt name [args : arguments] [body : (listof StmtP)] [decorator_list : (listof ExprP)]) : ExprC
  (let* ([arg-syms (map Arg-arg (Arguments-args args))]
         [assigned-names (filter (lambda (id)
                                   (not-in-list arg-syms id))
                                 (dedup (append-all (map extract-assigned-names body))))]
         [all-names (filter (lambda (id)
                              (not-in-list arg-syms id))
                            (dedup (append-all (map extract-all-names body))))]
         [globals (dedup (append-all (map extract-globals body)))]
         [nonlocals (dedup (append-all (map extract-nonlocals body)))]
         [wr-locals (dedup (filter (lambda (id)
                                     (not-in-list (append globals nonlocals) id))
                                   assigned-names))]
         [rd-locals (dedup (filter (lambda (id)
                                     (not-in-list (append wr-locals (append globals nonlocals)) id))
                                   all-names))]
         [defaults (map ds-expr (Arguments-defaults args))]
         [decorators (map Name-id decorator_list)])
    (FuncDefC name globals nonlocals wr-locals rd-locals arg-syms defaults decorators
              (ds-stmts body))))

(define (ds-expr [expr : ExprP]) : ExprC
  (type-case ExprP expr
    [BoolOp (op args) (seq-bool-op (boolop->sym op) args)]
    [BinOp (l op r) (Prim2C (op->sym op) (ds-expr l) (ds-expr r))]
    [UnaryOp (op operand) (Prim1C (unaryop->sym op) (ds-expr operand))]
    [Lambda (args body) (ds-func-stmt 'lambda/user args (list (Return body)) empty)]
    [IfExp (test body orelse) (IfC (ds-expr test) (ds-expr body) (ds-expr orelse))]
    [Dict (keys values) (DictC (create-mappings keys values))]
    [Set (keys) (SetC (map ds-expr keys))]
    [Compare (fa ops args) (seq-cmp-op fa (map cmpop->sym ops) args)]
    [Call (func args kw sa kwa) (ds-call-expr (ds-expr func) (map ds-expr args) (map ds-keyword kw))]
    [Num (n) (if (or (has-char? n #\e) (has-char? n #\.)) (NumC 'float n) (NumC 'int n))]
    [Str (s) (StrC s)]
    [Attribute (val attr ctx) (ds-attr-expr val attr ctx)]
    [Subscript (val idx ctx) (if (or (Load? ctx) (AugLoad? ctx))
                                 (GetDataC (ds-expr val) (ds-slice idx))
                                 (ds-error "subscript not in load context"))]
    [GeneratorExp (exp comps)
		  (ds-gen-exp exp comps)]
    [ListComp (exp comps)
	      (call-args/c (IdC 'list)
			   (list (ds-gen-exp exp comps)))]
    [SetComp (exp comps)
	      (call-args/c (IdC 'set)
			   (list (ds-gen-exp exp comps)))]
    [DictComp (exp comps)
	      (call-args/c (IdC 'dict)
			   (list (ds-gen-exp exp comps)))]
    [Name (id ctx) (IdC id)]
    [List (elts ctx) (ListC (map ds-expr elts))]
    [Tuple (elts ctx) (TupleC (map ds-expr elts))]
    [else unimplemented-exception]))

(define (ds-comprehension comp n-expc)
  (local
   [(define (ds-checks ifs)
      (if (empty? ifs)
          n-expc
          (IfC (ds-expr (first ifs))
               (ds-checks (rest ifs))
               (NoneC))))]
   (let ([iter (gen-sym 'iter)]
         [exn (gen-sym 'exn)])
     (LetC iter (call-args/c (IdC 'iter)
                             (list (ds-expr (Comprehension-iter comp))))
	   (WhileC
	    (TrueC)
	    (TryCatchC
	     (SeqC
	      (ds-assign-single (Comprehension-target comp)
				(call-nullary/c (get-attr/c (IdC iter) '__next__)))
	      (ds-checks (Comprehension-ifs comp)))
	     exn
	     (IfC (IsInstanceC (IdC exn) (IdC 'StopIteration))
		  (BreakC)
		  (DynErrorC (UndefinedC))))
	    (NoneC))))))

(define (ds-comp-list [comps : (listof comprehension)] [lst : ExprC] [expr : ExprP])
  (if (empty? comps)
      (AppendData!C lst (ds-expr expr))
      (ds-comprehension (first comps) (ds-comp-list (rest comps) lst expr))))


(define (ds-gen-exp expr comps) : ExprC
  (let* ([wr-locals (dedup (append-all (map extract-assigned-names/comp comps)))]
         [all-names (dedup (append-all (map extract-all-names/comp comps)))]
         [rd-locals (dedup (filter (lambda (id)
                                     (not-in-list wr-locals id))
                                   all-names))]
	 [gen-lambda (gen-sym 'gen-lambda)]
	 [res (gen-sym 'res)])
    (LetC gen-lambda
          (FuncDefC gen-lambda empty empty wr-locals rd-locals empty empty empty
                    (LetC res (ListC empty)
                          (SeqC (ds-comp-list comps
                                              (IdC res)
                                              expr)
                                (ReturnC (IdC res)))))
	  (call-args/c (IdC 'iter)
		       (list (call-nullary/c (IdC gen-lambda)))))))

(define (extract-assigned-names/comp comp)
  (extract-assigned-names/expr (Comprehension-target comp)))

(define (extract-all-names/comp comp)
  (append-all
   (list
    (extract-all-names/expr (Comprehension-target comp))
    (extract-all-names/expr (Comprehension-iter comp))
    (append-all (map extract-all-names/expr (Comprehension-ifs comp))))))

(define (ds-attr-expr val attr ctx)
  (let ([exn (gen-sym 'exn)]
        [delg (gen-sym 'delg)]
        [obj (gen-sym 'obj)])
    (if (or (Load? ctx) (AugLoad? ctx))
        (LetC obj (ds-expr val)
              (LetC delg
                    (TryCatchC
                     (get-attr/c (IdC obj) '__getattr__)
                     exn
                     (IdC obj))
                    (IfC (Prim2C 'is (IdC obj) (IdC delg))
                         (get-attr/c (IdC delg) attr)
                         (AppC (IdC delg) (list (StrC (symbol->string attr)))))))
        (ds-error "attribute not in load context"))))

(define (ds-call-expr [func : ExprC]
                      [args : (listof ExprC)]
                      [kw : (listof ExprC)])
  (AppC (GetCallableC func)
        args))

(define (ds-keyword [kw : keyword])
  (NoneC))

(define (ds-option ds op)
  (if (none? op)
      (NoneC)
      (ds (some-v op))))

(define (ds-slice [sl : slice])
  (local
   [(define (check-ds n-op)
      (if (none? n-op)
          (NoneC)
          (ds-expr (some-v n-op))))]
   (type-case slice sl
     [Slice (lower upper step) (call-args/c (IdC 'slice)
                                            (list (check-ds lower)
                                                  (check-ds upper)
                                                  (check-ds step)))]
     [Index (value) (ds-expr value)]
     [else unimplemented-exception])))

(define (extract-assigned-names [stmt : StmtP]) : (listof symbol)
  (type-case StmtP stmt
    [FunctionDef (n a b d r) (list n)]
    [ClassDef (n ba kw sa kwa b d) (list n)]
    [Assign (targets v) (append-all (map extract-assigned-names/expr targets))]
    [AugAssign (t o v)  (extract-assigned-names/expr t)]
    [For (t i body orelse)
         (append (extract-assigned-names/expr t)
                 (append (append-all (map extract-assigned-names body))
                         (append-all (map extract-assigned-names orelse))))]
    [While (t body orelse)
           (append (append-all (map extract-assigned-names body))
                   (append-all (map extract-assigned-names orelse)))]
    [If (t body orelse)
        (append (append-all (map extract-assigned-names body))
                (append-all (map extract-assigned-names orelse)))]
    [With (ce ov body)
          (append-all (map extract-assigned-names body))]
    [TryExcept (body handlers orelse)
               (append (append-all (map extract-assigned-names body))
                       (append (append-all (map extract-assigned-names/handler handlers))
                               (append-all (map extract-assigned-names orelse))))]
    [TryFinally (body finalbody)
                (append (append-all (map extract-assigned-names body))
                        (append-all (map extract-assigned-names finalbody)))]
    [else empty]))

(define (extract-assigned-names/handler [handler : excepthandler]) : (listof symbol)
  (if (some? (Excepthandler-name handler))
      (cons (some-v (Excepthandler-name handler))
            (append-all (map extract-assigned-names (Excepthandler-body handler))))
      (append-all (map extract-assigned-names (Excepthandler-body handler)))))

(define (extract-all-names [stmt : StmtP]) : (listof symbol)
  (type-case StmtP stmt
    [FunctionDef (n a b d r) (list n)]
    [ClassDef (n ba kw sa kwa b d) (list n)]
    [Assign (targets v) (append (append-all (map extract-all-names/expr targets))
                                (extract-all-names/expr v))]
    [AugAssign (t o v)  (append (extract-all-names/expr t)
                                (extract-all-names/expr v))]
    [For (t i body orelse)
         (append (extract-all-names/expr t)
                 (append (extract-all-names/expr i)
                         (append (append-all (map extract-all-names body))
                                 (append-all (map extract-all-names orelse)))))]
    [While (t body orelse)
           (append (extract-all-names/expr t)
                   (append (append-all (map extract-all-names body))
                           (append-all (map extract-all-names orelse))))]
    [If (t body orelse)
        (append (extract-all-names/expr t)
                (append (append-all (map extract-all-names body))
                        (append-all (map extract-all-names orelse))))]
    [With (ce ov body)
          (append (extract-all-names/expr ce)
                  (append (if (none? ov)
                              empty
                              (extract-all-names/expr (some-v ov)))
                          (append-all (map extract-all-names body))))]
    [TryExcept (body handlers orelse)
               (append (append-all (map extract-all-names body))
                       (append (append-all (map extract-all-names/handler handlers))
                               (append-all (map extract-all-names orelse))))]
    [TryFinally (body finalbody)
                (append (append-all (map extract-all-names body))
                        (append-all (map extract-all-names finalbody)))]
    [Return (e) (extract-all-names/expr e)]
    [else empty]))

(define (extract-all-names/expr [expr : ExprP]) : (listof symbol)
  (type-case ExprP expr
    [BoolOp (o vs) (append-all (map extract-all-names/expr vs))]
    [BinOp (l o r) (append (extract-all-names/expr l)
                           (extract-all-names/expr r))]
    [UnaryOp (op a) (extract-all-names/expr a)]
    [IfExp (t b e) (append (extract-all-names/expr t)
                           (append (extract-all-names/expr b)
                                   (extract-all-names/expr e)))]
    [Dict (k v) (append (append-all (map extract-all-names/expr k))
                        (append-all (map extract-all-names/expr v)))]
    [Set (e) (append-all (map extract-all-names/expr e))]
    [Compare (l o co) (append (extract-all-names/expr l)
                              (append-all (map extract-all-names/expr co)))]
    [Attribute (v a ctx) (extract-all-names/expr v)]
    [Subscript (v s c) (extract-all-names/expr v)]
    [Starred (v c) (extract-all-names/expr v)]
    [Name (id c) (list id)]
    [List (e c) (append-all (map extract-all-names/expr e))]
    [Tuple (e c) (append-all (map extract-all-names/expr e))]
    [else empty]))

(define (extract-all-names/handler [handler : excepthandler]) : (listof symbol)
  (if (some? (Excepthandler-name handler))
      (cons (some-v (Excepthandler-name handler))
            (append-all (map extract-all-names (Excepthandler-body handler))))
      (append-all (map extract-all-names (Excepthandler-body handler)))))

(define (extract-all-globals [stmt : StmtP]) : (listof symbol)
  (type-case StmtP stmt
    [FunctionDef (n a b d r) (append-all (map extract-all-globals b) )]
    [ClassDef (n ba kw sa kwa b d) (append-all (map extract-all-globals b))]
    [For (t i body orelse)
         (append (append-all (map extract-all-globals body))
                 (append-all (map extract-all-globals orelse)))]
    [While (t body orelse)
           (append (append-all (map extract-all-globals body))
                   (append-all (map extract-all-globals orelse)))]
    [If (t body orelse)
        (append (append-all (map extract-all-globals body))
                (append-all (map extract-all-globals orelse)))]
    [With (ce ov body)
          (append-all (map extract-all-globals body))]

    [TryExcept (body handlers orelse)
               (append (append-all (map extract-all-globals body))
                       (append (append-all (map (lambda (h)
                                                  (append-all (map extract-all-globals
                                                                   (Excepthandler-body h))))
                                                handlers))
                               (append-all (map extract-all-globals orelse))))]

    [TryFinally (body finalbody)
                (append (append-all (map extract-all-globals body))
                        (append-all (map extract-all-globals finalbody)))]
    [Global (names) names]
    [else empty]))

(define (extract-assigned-names/expr [expr : ExprP]) : (listof symbol)
  (type-case ExprP expr
    [Name (n c) (list n)]
    [List (l c) (append-all (map extract-assigned-names/expr l))]
    [Tuple (l c) (append-all (map extract-assigned-names/expr l))]
    [else empty]))

(define (extract-names/arg args)
  (type-case arguments args
    [Arguments (a va van kwoa kwa kwan def kwdef)
               (append (map Arg-arg a)
                       (if (none? va)
                           empty
                           (list (some-v va))))]))

(define (extract-globals [stmt : StmtP])
  (if (Global? stmt) (Global-names stmt) empty))

(define (extract-nonlocals [stmt : StmtP])
  (if (Nonlocal? stmt) (Nonlocal-names stmt) empty))

(define (contains? lst item)
  (foldl (lambda (a res)
           (or res (symbol=? a item)))
         #f lst))

(define (dedup l)
  (if (empty? l)
      empty
      (if (contains? (rest l) (first l))
          (dedup (rest l))
          (cons (first l) (dedup (rest l))))))

(define (create-mappings [keys : (listof ExprP)] [values : (listof ExprP)])
  (if (empty? keys)
      empty
      (cons (entC (ds-expr (first keys)) (ds-expr (first values)))
            (create-mappings (rest keys) (rest values)))))

(define (seq-bool-op [op : symbol] [args : (listof ExprP)])
  (if (empty? (rest (rest args)))
      (Prim2C op (ds-expr (first args)) (ds-expr (second args)))
      (Prim2C op (ds-expr (first args)) (seq-bool-op op (rest args)))))

(define (seq-cmp-op [f : ExprP] [ops : (listof symbol)] [args : (listof ExprP)])
  (let ([f-sym (gen-sym 'first)]
        [s-sym (gen-sym 'second)])
    (local [(define (assume-first-cmp [ops : (listof symbol)] [args : (listof ExprP)])
              (if (empty? (rest ops))
                  (Prim2C (first ops) (IdC f-sym) (ds-expr (first args)))
                  (SeqC (Set!C s-sym (ds-expr (first args)))
                        (IfC (Prim2C (first ops) (IdC f-sym) (IdC s-sym))
                             (SeqC (Set!C f-sym (IdC s-sym))
                                   (assume-first-cmp (rest ops) (rest args)))
                             (FalseC)))))]
           (LetC f-sym (ds-expr f)
                 (LetC s-sym (UndefinedC)
                       (assume-first-cmp ops args))))))

(define (has-char? n ch)
  (foldl (lambda (c res)
           (or (char=? c ch) res))
         #f
         (string->list (to-string n))))
