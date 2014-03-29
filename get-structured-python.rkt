#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#

(define debug-parse #f)

(define (ph ht parser)
  (let* ([get-val (lambda (f)
                    (if (eq? #\nul (hash-ref ht f #\nul))
                        #f
                        (hash-ref ht f)))]
         [get-list (lambda (f)
                     (if (eq? #\nul (hash-ref ht f #\nul))
                         empty
                         (hash-ref ht f)))]
         [parse-option (lambda (f parser)
                         (if (get-val f)
                             (some (parser (get-val f)))
                             (none)))]
         [err (lambda (sym)
                (error sym (string-append "Failed "
                                          (string-append (symbol->string sym)
                                                         (string-append ": " (get-val 'nodetype))))))])
    (if debug-parse
        (begin
          (display ht)
          (display "\n")
          (parser get-val get-list parse-option err))
        (parser get-val get-list parse-option err))))

(define (get-structured-python json)
  (ph json
      (lambda (gv gl parse-option err)
        (match (gv 'nodetype)
          ["Module" (Module (map parse-stmt (gl 'body)))]
          ["Interactive" (Interactive (map parse-stmt (gl 'body)))]
          ["Expression" (Expression (parse-expr (gv 'e)))]
          [_ (err 'parse-mod)]))))

(define (parse-stmt pyjson)
  (ph pyjson
      (lambda (gv gl parse-option err)
        (match (gv 'nodetype)
          ["FunctionDef" (FunctionDef (string->symbol (gv 'name))
                                      (parse-arguments (gv 'args))
                                      (map parse-stmt (gl 'body))
                                      (map parse-expr (gl 'decorator_list))
                                      (parse-option 'returns parse-expr))]
          ["ClassDef" (ClassDef (string->symbol (gv 'name))
                                (map parse-expr (gl 'bases))
                                (map parse-keyword (gl 'keywords))
                                (parse-option 'starargs parse-expr)
                                (parse-option 'kwargs parse-expr)
                                (map parse-stmt (gl 'body))
                                (map parse-expr (gl 'decorator_list)))]
          ["Return" (Return (parse-expr (gv 'value)))]
          ["Delete" (Delete (map parse-expr (gl 'targets)))]
          ["Assign" (Assign (map parse-expr (gl 'targets))
                            (parse-expr (gv 'value)))]
          ["AugAssign" (AugAssign (parse-expr (gv 'target))
                                  (parse-operator (gv 'op))
                                  (parse-expr (gv 'value)))]
          ["For" (For (parse-expr (gv 'target))
                      (parse-expr (gv 'iter))
                      (map parse-stmt (gl 'body))
                      (map parse-stmt (gl 'orelse)))]
          ["While" (While (parse-expr (gv 'test))
                          (map parse-stmt (gl 'body))
                          (map parse-stmt (gl 'orelse)))]
          ["If" (If (parse-expr (gv 'test))
                    (map parse-stmt (gl 'body))
                    (map parse-stmt (gl 'orelse)))]
          ["With" (With (parse-expr (gv 'context_expr))
                        (parse-option 'optional_vars parse-expr)
                        (map parse-stmt (gl 'body)))]
          ["Raise" (Raise (parse-option 'exc parse-expr)
                          (parse-option 'cause parse-expr))]
          ["TryExcept" (TryExcept (map parse-stmt (gl 'body))
                                  (map parse-handler (gl 'handlers))
                                  (map parse-stmt (gl 'orelse)))]
          ["TryFinally" (TryFinally (map parse-stmt (gl 'body))
                                    (map parse-stmt (gl 'finalbody)))]
          ["Assert" (Assert (parse-expr (gv 'test))
                            (parse-option 'msg parse-expr))]
          ["Import" (Import (map parse-alias (gl 'names)))]
          ["ImportFrom" (ImportFrom (parse-option 'module string->symbol)
                                    (map parse-alias (gl 'names))
                                    (parse-option 'level string->number))]
          ["Global" (Global (map string->symbol (gl 'names)))]
          ["Nonlocal" (Nonlocal (map string->symbol (gl 'names)))]
          ["Expr" (Expr (parse-expr (gv 'value)))]
          ["Pass" (Pass)]
          ["Break" (Break)]
          ["Continue" (Continue)]
          [_ (err 'parse-stmt)]))))

(define (parse-expr pyjson)
  (ph pyjson
      (lambda (gv gl parse-option err)
        (match (gv 'nodetype)
          ["BoolOp" (BoolOp (parse-boolop (gv 'op))
                            (map parse-expr (gl 'values)))]
          ["BinOp" (BinOp (parse-expr (gv 'left))
                          (parse-operator (gv 'op))
                          (parse-expr (gv 'right)))]
          ["UnaryOp" (UnaryOp (parse-unaryop (gv 'op))
                              (parse-expr (gv 'operand)))]
          ["Lambda" (Lambda (parse-arguments (gv 'args))
                            (parse-expr (gv 'body)))]
          ["IfExp" (IfExp (parse-expr (gv 'test))
                          (parse-expr (gv 'body))
                          (parse-expr (gv 'orelse)))]
          ["Dict" (Dict (map parse-expr (gl 'keys))
                        (map parse-expr (gl 'values)))]
          ["Set" (Set (map parse-expr (gl 'elts)))]
          ["ListComp" (ListComp (parse-expr (gv 'elt))
                                (map parse-comprehension (gl 'generators)))]
          ["SetComp" (SetComp (parse-expr (gv 'elt))
                              (map parse-comprehension (gl 'generators)))]
          ["DictComp" (DictComp (parse-expr (gv 'elt))
                                (map parse-comprehension (gl 'generators)))]
          ["GeneratorExp" (GeneratorExp (parse-expr (gv 'elt))
                                        (map parse-comprehension (gl 'generators)))]
          ["Yield" (Yield (parse-option 'value parse-expr))]
          ["Compare" (Compare (parse-expr (gv 'left))
                              (map parse-cmpop (gl 'ops))
                              (map parse-expr (gl 'comparators)))]
          ["Call" (Call (parse-expr (gv 'func))
                        (map parse-expr (gl 'args))
                        (map parse-keyword (gl 'keywords))
                        (parse-option 'starargs parse-expr)
                        (parse-option 'kwargs parse-expr))]
          ["Num" (Num (gv 'n))]
          ["Str" (Str (gv 's))]
          ["Bytes" (Bytes (gv 's))]
          ["Ellipsis" (Ellipsis)]
          ["Attribute" (Attribute (parse-expr (gv 'value))
                                  (string->symbol (gv 'attr))
                                  (parse-expr_context (gv 'ctx)))]
          ["Subscript" (Subscript (parse-expr (gv 'value))
                                  (parse-slice (gv 'slice))
                                  (parse-expr_context (gv 'ctx)))]
          ["Starred" (Starred (parse-expr (gv 'value))
                              (parse-expr_context (gv 'ctx)))]
          ["Name" (Name (string->symbol (gv 'id))
                        (parse-expr_context (gv 'ctx)))]
          ["List" (List (map parse-expr (gl 'elts))
                        (parse-expr_context (gv 'ctx)))]
          ["Tuple" (Tuple (map parse-expr (gl 'elts))
                          (parse-expr_context (gv 'ctx)))]
          [_ (err 'parse-expr)]))))

(define (parse-expr_context ctx)
  (ph ctx
      (lambda (gv gl po err)
        (match (gv 'nodetype)
          ["Load" (Load)]
          ["Store" (Store)]
          ["Del" (Del)]
          ["AugLoad" (AugLoad)]
          ["AugStore" (AugStore)]
          ["Param" (Param)]
          [_ (err 'parse-expr_context)]))))

(define (parse-slice sl)
  (ph sl
      (lambda (gv gl parse-option err)
        (match (gv 'nodetype)
          ["Slice" (Slice (parse-option 'lower parse-expr)
                          (parse-option 'upper parse-expr)
                          (parse-option 'step parse-expr))]
          ["ExtSlice" (ExtSlice (map parse-slice (gl 'dims)))]
          ["Index" (Index (parse-expr (gv 'value)))]
          [_ (err 'parse-slice)]))))

(define (parse-boolop bo)
  (ph bo
      (lambda (gv gl po err)
        (match (gv 'nodetype)
          ["And" (And)]
          ["Or" (Or)]
          [_ (err 'parse-boolop)]))))

(define (parse-operator op)
  (ph op
      (lambda (gv gl po err)
        (match (gv 'nodetype)
          ["Add" (Add)]
          ["Sub" (Sub)]
          ["Mult" (Mult)]
          ["Div" (Div)]
          ["Mod" (Mod)]
          ["Pow" (Pow)]
          ["LShift" (LShift)]
          ["RShift" (RShift)]
          ["BitOr" (BitOr)]
          ["BitXor" (BitXor)]
          ["BitAnd" (BitAnd)]
          ["FloorDiv" (FloorDiv)]
          [_ (err 'parse-operator)]))))

(define (parse-unaryop uo)
  (ph uo
      (lambda (gv gl po err)
        (match (gv 'nodetype)
          ["Invert" (Invert)]
          ["Not" (Not)]
          ["UAdd" (UAdd)]
          ["USub" (USub)]
          [_ (err 'parse-unaryop)]))))

(define (parse-cmpop cmpop)
  (ph cmpop
      (lambda (gv gl po err)
        (match (gv 'nodetype)
          ["Eq" (Eq)]
          ["NotEq" (NotEq)]
          ["Lt" (Lt)]
          ["LtE" (LtE)]
          ["Gt" (Gt)]
          ["GtE" (GtE)]
          ["Is" (Is)]
          ["IsNot" (IsNot)]
          ["In" (In)]
          ["NotIn" (NotIn)]
          [_ (err 'parse-cmpop)]))))

(define (parse-comprehension comp)
  (ph comp
      (lambda (gv gl po err)
        (match (gv 'nodetype)
          ["comprehension" (Comprehension (parse-expr (gv 'target))
                                          (parse-expr (gv 'iter))
                                          (map parse-expr (gl 'ifs)))]
          [_ (err 'parse-comprehension)]))))

(define (parse-handler handler)
  (ph handler
      (lambda (gv gl parse-option err)
        (match (gv 'nodetype)
          ["ExceptHandler" (Excepthandler (parse-option 'type parse-expr)
                                          (parse-option 'name string->symbol)
                                          (map parse-stmt (gl 'body)))]
          [_ (err 'parse-handler)]))))

(define (parse-arguments args)
  (ph args
      (lambda (gv gl parse-option err)
        (match (gv 'nodetype)
          ["arguments" (Arguments (map parse-arg (gl 'args))
                                  (parse-option 'vararg string->symbol)
                                  (parse-option 'varargnotation parse-expr)
                                  (map parse-arg (gl 'kwonlyargs))
                                  (parse-option 'kwarg string->symbol)
                                  (parse-option 'kwargannotation parse-expr)
                                  (map parse-expr (gl 'defaults))
                                  (map parse-expr (gl 'kw_defaults)))]
          [_ (err 'parse-arguments)]))))

(define (parse-arg arg)
  (ph arg
      (lambda (gv gl parse-option err)
        (match (gv 'nodetype)
          ["arg" (Arg (string->symbol (gv 'arg))
                      (parse-option 'annotation parse-expr))]
          [_ (error 'parse-arg)]))))

(define (parse-keyword kw)
  (ph kw
      (lambda (gv gl po err)
        (match (gv 'nodetype)
          ["keyword" (Keyword (string->symbol (gv 'arg))
                              (parse-expr (gv 'value)))]
          [_ (err 'parse-keyword)]))))

(define (parse-alias alias)
  (ph alias
      (lambda (gv gl po err)
        (match (gv 'nodetype)
          ["Alias" (Alias (string->symbol (gv 'name))
                          (string->symbol (gv 'asname)))]
          [_ (err 'parse-alias)]))))
