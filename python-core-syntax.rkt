#lang plai-typed

#|

This is the core language; it is just borrowing a few things from
ParselTongue.

|#

(define-type-alias Location number)

(define-type MappingV
  [entV (key : Location) (val : Location)])

(define-type-alias Store (hashof Location ObjV))

(define-type Binding
  ;; use boxof. do not do the stupidity of overriding memory locations.
  ;; all hell will break loose
  [glbB (name : symbol)]
  [nlcB (name : symbol)]
  [lclB (name : symbol) (value : (boxof Location))]
  [tmpB (name : symbol) (value : (boxof Location))])


(define-type Env
  [nullE]
  [modlE (name : symbol) (outer : Env) (names : SymTab)]
  [funcE (name : symbol) (outer : Env) (names : SymTab)]
  [clssE (name : symbol) (outer : Env) (names : SymTab)])

(define-type-alias SymTab (listof Binding))

(define-type-alias Hash (boxof (listof MappingV)))

(define-type-alias Vector (boxof (listof Location)))

(define-type ObjV
  [UndefinedV]
  [EmptyV (cls : Location) (dict : Hash)]
  [StrV   (cls : Location) (dict : Hash) (data : string)]
  [NumV   (cls : Location) (dict : Hash) (data : number)]
  [ListV  (cls : Location) (dict : Hash) (frozen? : boolean) (data : Vector)]
  [DictV  (cls : Location) (dict : Hash) (frozen? : boolean) (data : Hash)]
  [ClosV  (cls : Location) (dict : Hash) (args : (listof symbol)) (body : ExprC) (env : Env)]
  [ExnV   (cls : Location) (dict : Hash) (data : Location)])

(define-type FieldC
  [fld (name : symbol) (value : ExprC)])

(define-type MappingC
  [entC (key : ExprC) (val : ExprC)])

(define-type ExprC
  [ObjC (cls : ExprC) (attrs : ExprC) (data : (listof ExprC))]
  [StrC (s : string)]
  [NumC (cls : symbol) (n : number)]

  [TupleC (tup : (listof ExprC))]
  [ListC  (lst : (listof ExprC))]
  #|
  [BytesC  (cls : symbol) (expandable? : boolean) (b : string)]
  [ByteArrayC  (cls : symbol) (expandable? : boolean) (b : string)]
  |#
  [DictC      (dict : (listof MappingC))]
  [SetC       (s : (listof ExprC))]
  [FrozenSetC (s : (listof ExprC))]

  [LamC   (args : (listof symbol)) (body : ExprC)]
  [ErrorC  (cls : symbol) (expr : ExprC)]
  [DynErrorC (exn : ExprC)]

  [SuperC (type : ExprC) (obj : ExprC)]
  
  [NoneC]
  [TrueC]
  [FalseC]
  [EllipsisC]

  [UndefinedC]

  [LenC (coll : ExprC)]
  [CopyObjC (obj : ExprC)]
  [ToStrC (obj : ExprC)]
  [GetCallableC (obj : ExprC)]
  [BindC (fun : ExprC) (obj : ExprC)]
  [GetAttrC  (obj : ExprC) (field : ExprC)]
  [SetAttr!C (obj : ExprC) (field : ExprC) (value : ExprC)]
  [DelAttr!C (obj : ExprC) (field : ExprC)]
  [SearchMroC (obj : ExprC) (field : ExprC)]

  [GetDataC     (coll : ExprC) (index : ExprC)]
  [GetItemAtIdxC (coll : ExprC) (index : ExprC)]
  [SetData!C    (coll : ExprC) (index : ExprC) (value : ExprC)]
  [DelData!C    (coll : ExprC) (index : ExprC)]
  [AppendData!C (vect : ExprC) (obj : ExprC)]

  [AppC (func : ExprC) (args : (listof ExprC))]
  [DynAppC (func : ExprC) (args : ExprC)]

  [ModuleC (name : symbol) (locals : (listof symbol))
           (exprC : ExprC)]
  ;; locals for fundefc includes formal function parameter names
  [FuncDefC (name : symbol) (globals : (listof symbol)) (nonlocals : (listof symbol))
	    (wr-locals : (listof symbol)) (rd-locals : (listof symbol))
            (args : (listof symbol)) (defaults : (listof ExprC)) (decorators : (listof symbol))
	    (exprC : ExprC)]
  [ClassDefC (name : symbol) (globals : (listof symbol)) (nonlocals : (listof symbol)) (locals : (listof symbol))
             (exprC : ExprC)]

  ;; strictly for tmp variables
  [LetC (id : symbol) (bind : ExprC) (body : ExprC)]
  [IdC (id : symbol)]
  [Set!C (id : symbol) (value : ExprC)]
  [Del!C (id : symbol)]

  [TypeC (value : ExprC)]

  [IfC (cond : ExprC) (then : ExprC) (else : ExprC)]
  [SeqC (e1 : ExprC) (e2 : ExprC)]
  [WhileC (test : ExprC) (body : ExprC) (orelse : ExprC)]

  [BreakC]
  [ReturnC (val : ExprC)]

  [TryCatchC (body : ExprC) (param : symbol) (catch : ExprC)]
  [TryFinallyC (body : ExprC) (finalBody : ExprC)]

  [Prim1C (op : symbol) (arg : ExprC)]
  [Prim2C (op : symbol) (arg1 : ExprC) (arg2 : ExprC)]

  [IsInstanceC (obj : ExprC) (cls : ExprC)]
  [IsSubclassC (cls : ExprC) (super : ExprC)]

  [RunCoreC (op : symbol) (args : (listof ExprC))])

(define-type AnswerC
  [ValueA (v : Location)]
  [ExceptionA (e : Location)]
  [ReturnA (v : Location)]
  [BreakA])

(define-type MultiValAnswerC
  [ValueListA (l : (listof Location))]
  [MVExceptionA (e : Location)]
  [MVReturnA (v : Location)]
  [MVBreakA])
