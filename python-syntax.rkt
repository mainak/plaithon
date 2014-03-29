#lang plai-typed

(define-type (optionof 'a)
  [none]
  [some (v : 'a)])

(define-type ModP
  [Module (body : (listof StmtP))]
  [Interactive (body : (listof StmtP))]
  [Expression (e : ExprP)])

(define-type StmtP
  [FunctionDef (name : symbol)
               (args : arguments)
               (body : (listof StmtP))
               (decorator_list : (listof ExprP))
               (returns : (optionof ExprP))]
  [ClassDef (name : symbol)
            (bases : (listof ExprP))
            (keywords : (listof keyword))
            (starargs : (optionof ExprP))
            (kwargs : (optionof ExprP))
            (body : (listof StmtP))
            (decorator_list : (listof ExprP))]
  [Return (value : ExprP)]
  [Delete (targets : (listof ExprP))]
  [Assign (targets : (listof ExprP))
          (value : ExprP)]
  [AugAssign (target : ExprP)
             (op : operator)
             (value : ExprP)]
  [For (target : ExprP)
       (iter : ExprP)
       (body : (listof StmtP))
       (orelse : (listof StmtP))]
  [While (test : ExprP)
         (body : (listof StmtP))
         (orelse : (listof StmtP))]
  [If (test : ExprP)
      (body : (listof StmtP))
      (orelse : (listof StmtP))]
  [With (context_expr : ExprP)
        (optional_vars : (optionof ExprP))
        (body : (listof StmtP))]
  [Raise (exc : (optionof ExprP))
         (cause : (optionof ExprP))]
  [TryExcept (body : (listof StmtP))
             (handlers : (listof excepthandler))
             (orelse : (listof StmtP))]
  [TryFinally (body : (listof StmtP))
              (finalbody : (listof StmtP))]
  [Assert (test : ExprP)
          (msg : (optionof ExprP))]
  [Import (names : (listof alias))]
  [ImportFrom (module : (optionof symbol))
              (names : (listof alias))
              (level : (optionof number))]
  [Global (names : (listof symbol))]
  [Nonlocal (names : (listof symbol))]
  [Expr (value : ExprP)]
  [Pass]
  [Break]
  [Continue])

(define-type ExprP
  [BoolOp (op : boolop)
          (values : (listof ExprP))]
  [BinOp (left : ExprP)
         (op : operator)
         (right : ExprP)]
  [UnaryOp (op : unaryop)
           (operand : ExprP)]
  [Lambda (args : arguments)
          (body : ExprP)]
  [IfExp (test : ExprP)
         (body : ExprP)
         (orelse : ExprP)]
  [Dict (keys : (listof ExprP))
        (values : (listof ExprP))]
  [Set (elts : (listof ExprP))]
  [ListComp (elt : ExprP)
            (generators : (listof comprehension))]
  [SetComp (elt : ExprP)
           (generators : (listof comprehension))]
  [DictComp (elt : ExprP)
            (generators : (listof comprehension))]
  [GeneratorExp (elt : ExprP)
                (generators : (listof comprehension))]
  [Yield (value : (optionof ExprP))]
  [Compare (left : ExprP)
           (ops : (listof cmpop))
           (comparators : (listof ExprP))]
  [Call (func : ExprP)
        (args : (listof ExprP))
        (keywords : (listof keyword))
        (starargs : (optionof ExprP))
        (kwargs : (optionof ExprP))]
  [Num (n : number)]
  [Str (s : string)]
  [Bytes (s : string)]
  [Ellipsis]
  [Attribute (value : ExprP)
             (attr : symbol)
             (ctx : expr_context)]
  [Subscript (value : ExprP)
             (slice : slice)
             (ctx : expr_context)]
  [Starred (value : ExprP)
           (ctx : expr_context)]
  [Name (id : symbol)
        (ctx : expr_context)]
  [List (elts : (listof ExprP))
        (ctx : expr_context)]
  [Tuple (elts : (listof ExprP))
         (ctx : expr_context)])

(define-type expr_context
  [Load] [Store] [Del] [AugLoad] [AugStore] [Param])

(define-type slice
  [Slice (lower : (optionof ExprP))
         (upper : (optionof ExprP))
         (step : (optionof ExprP))]
  [ExtSlice (dims : (listof slice))]
  [Index (value : ExprP)])

(define-type boolop
  [And] [Or])

(define-type operator
  [Add] [Sub] [Mult] [Div] [Mod] [Pow] [LShift] [RShift] [BitOr] [BitXor] [BitAnd] [FloorDiv])

(define-type unaryop
  [Invert] [Not] [UAdd] [USub])

(define-type cmpop
  [Eq] [NotEq] [Lt] [LtE] [Gt] [GtE] [Is] [IsNot] [In] [NotIn])

(define-type comprehension
  [Comprehension (target : ExprP)
                 (iter : ExprP)
                 (ifs : (listof ExprP))])

(define-type excepthandler
  [Excepthandler (type : (optionof ExprP))
                 (name : (optionof symbol))
                 (body : (listof StmtP))])

(define-type arguments
  [Arguments (args : (listof arg))
             (vararg : (optionof symbol))
             (varargnotation : (optionof ExprP))
             (kwonlyargs : (listof arg))
             (kwarg : (optionof symbol))
             (kwargannotation : (optionof ExprP))
             (defaults : (listof ExprP))
             (kw_defaults : (listof ExprP))])

(define-type arg
  [Arg (arg : symbol)
       (annotation : (optionof ExprP))])

(define-type keyword
  [Keyword (arg : symbol)
           (value : ExprP)])

(define-type alias
  [Alias (name : symbol)
         (asname : symbol)])


(define (boolop->sym op)
  (cond
   [(And? op) 'and]
   [(Or? op) 'or]))

(define (op->sym op)
  (cond
   [(Add? op) 'add]
   [(Sub? op) 'sub]
   [(Mult? op) 'mul]
   [(Div? op) 'div]
   [(Mod? op) 'mod]
   [(Pow? op) 'pow]
   [(LShift? op) 'lshift]
   [(RShift? op) 'rshift]
   [(BitOr? op) 'bitor]
   [(BitXor? op) 'bitxor]
   [(BitAnd? op) 'bitand]
   [(FloorDiv? op) 'floordiv]))

(define (unaryop->sym op)
  (cond
   [(Invert? op) 'invert]
   [(Not? op) 'not]
   [(UAdd? op) 'uadd]
   [(USub? op) 'usub]))

(define (cmpop->sym op)
  (cond
   [(Eq? op) 'eq]
   [(NotEq? op) 'ne]
   [(Lt? op) 'lt]
   [(LtE? op) 'le]
   [(Gt? op) 'gt]
   [(GtE? op) 'ge]
   [(Is? op) 'is]
   [(IsNot? op) 'isnot]
   [(In? op) 'in]
   [(NotIn? op) 'notin]))
