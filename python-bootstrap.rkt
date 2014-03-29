#lang plai-typed

(require "python-core-syntax.rkt"
         "python-base.rkt")

(define-type BindingC
  (bindc (id : symbol) (e : ExprC)))

(define (lst-let/c [lets : (listof BindingC)] [body : ExprC])
  (if (empty? lets)
      body
      (LetC (bindc-id (first lets)) (bindc-e (first lets))
            (lst-let/c (rest lets) body))))

(define (seq/c [exprs : (listof ExprC)]) : ExprC
  (if (empty? exprs)
      (NoneC)
      (if (empty? (rest exprs))
          (first exprs)
          (SeqC (first exprs) (seq/c (rest exprs))))))

(define (concat/c [lst : ExprC])
  (let ([ret 'ret]
        [iter 'iter]
        [exn 'exn])
    (LetC ret (StrC "")
          (LetC iter (call-nullary/c (get-attr/c lst '__iter__))
                (SeqC
                 (WhileC (TrueC)
                         (TryCatchC
                          (Set!C ret
                                 (Prim2C 'add
                                         (IdC ret)
                                         (call-nullary/c (get-attr/c (IdC iter) '__next__))))
                          exn
                          (IfC (IsInstanceC (IdC exn) (IdC 'StopIteration))
                               (BreakC)
                               (DynErrorC (UndefinedC))))
                         (NoneC))
                 (IdC ret))))))

(define (call-nullary/c [cble : ExprC])
  (AppC (GetCallableC cble) empty))

(define (is-callable/c [cble : ExprC])
  (TryCatchC
   (SeqC
    (GetCallableC cble)
    (TrueC))
   (gen-sym 'exn)
   (FalseC)))

(define (call-args/c [cble : ExprC] [args : (listof ExprC)])
  (AppC (GetCallableC cble) args))

(define (eq?/c [e1 : ExprC] [e2 : ExprC])
  (Prim2C 'eq e1 e2))

(define (empty?/c [col : ExprC])
  (eq?/c (LenC col) (NumC 'int 0)))

(define (get-item/c [col : ExprC] [idx : number])
  (GetDataC col (NumC 'int idx)))

(define (or/c [l : ExprC] [r : ExprC])
  (Prim2C 'or l r))

(define (and/c [l : ExprC] [r : ExprC])
  (Prim2C 'and l r))

(define (add/c [l : ExprC] [r : ExprC])
  (Prim2C 'add l r))

(define (sub/c [l : ExprC] [r : ExprC])
  (Prim2C 'sub l r))

(define (add1/c [e : ExprC])
  (add/c e (NumC 'int 1)))

(define (sub1/c [e : ExprC])
  (sub/c e (NumC 'int 1)))

(define (get-attr/c [obj : ExprC] [f : symbol])
  (GetAttrC obj (StrC (symbol->string f))))

(define (set-attr/c [obj : ExprC] [f : symbol] [v : ExprC])
  (SetAttr!C obj (StrC (symbol->string f)) v))

(define (del-attr/c [obj : ExprC] [f : symbol])
  (DelAttr!C obj (StrC (symbol->string f))))

(define (iterable?/c objC)
  (let ([exn 'exn])
    (TryCatchC
     (SeqC
      (GetCallableC (get-attr/c objC '__iter__))
      (TrueC))
     exn
     (IfC (or/c (IsInstanceC (IdC exn) (IdC 'AttributeError))
                (IsInstanceC (IdC exn) (IdC 'TypeError)))
          (FalseC)
          (DynErrorC (UndefinedC))))))

(define (cl-env name) (create-env name builtin-env 'class))

(define (init-memory) : void
  (begin
    (hash-set! memory (lookup/boot 'object)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "object"))
                         (entV (obj-str "__bases__")
                               (obj-tup empty))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object
                                (let ([type 'stype]
                                      [args 'args])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__new__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))

                                         (list type args)
                                         (ObjC (IdC type) (DictC empty) empty)
                                         (cl-env 'object)))))
                         (entV (obj-str "__init__")
                               (store-object
                                (let ([obj 'obj]
                                      [args 'args])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__init__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))

                                         (list obj args)
                                         (NoneC)
                                         (cl-env 'object)))))))))
    (hash-set! memory (lookup/boot 'type)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "type"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'type)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object
                                (let ([self 'self]
                                      [args 'args]
                                      [tpobj 'tpobj])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__new__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))

                                         (list self args)
                                         (IfC (eq?/c (LenC (IdC args)) (NumC 'int 1))
                                              (TypeC (get-item/c (IdC args) 1))
                                              (LetC tpobj (ObjC (IdC 'type)
                                                                (CopyObjC (get-item/c (IdC args) 2)) empty)
                                                    (seq/c
                                                     (list
                                                      (set-attr/c (IdC tpobj)
                                                                  '__name__
                                                                  (get-item/c (IdC args) 0))
                                                      (set-attr/c (IdC tpobj)
                                                                  '__bases__
                                                                  (RunCoreC 'compute-bases
                                                                            (list (get-item/c (IdC args) 1))))
                                                      (set-attr/c (IdC tpobj)
                                                                  '__mro__
                                                                  (RunCoreC 'compute-mro (list (IdC tpobj))))
                                                      (IdC tpobj)))))
                                         (cl-env 'type)))))
                         (entV (obj-str "__call__")
                               (store-object
                                ;;  (LamC (list 'tpname 'args 'kwds 'sa 'kwa))
                                (let ([self 'self]
                                      [args 'args]
                                      [new-func 'new-func]
                                      [init-func 'init-func]
                                      [new-obj 'new-obj])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__call__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))

                                         (list self args)
                                         (LetC new-func
                                               (TryCatchC
                                                (SearchMroC (IdC self) (StrC "__new__"))
                                                'exn
                                                (IfC (IsInstanceC (IdC 'exn) (IdC 'AttributeError))
                                                     (ErrorC 'TypeError (add/c (get-attr/c (IdC self) '__name__)
                                                                               (StrC " cannot create instances")))
                                                     (DynErrorC (UndefinedC))))
                                               (LetC new-obj
                                                     (DynAppC (IdC new-func)
                                                              (Prim2C 'add
                                                                      (TupleC (list (IdC self)))
                                                                      (IdC args)))
                                                     (IfC (IsInstanceC (IdC new-obj) (IdC 'type))
                                                          (IdC new-obj)
                                                          (LetC init-func (SearchMroC (IdC self) (StrC "__init__"))
                                                                (SeqC (DynAppC (IdC init-func)
                                                                               (Prim2C 'add
                                                                                       (TupleC (list (IdC new-obj)))
                                                                                       (IdC args)))
                                                                      (IdC new-obj))))))
                                         (cl-env 'type)))))))))
    (hash-set! memory (lookup/boot 'super)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "super"))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'super)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object
                                (let ([type 'stype]
                                      [args 'args])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__new__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))

                                         (list type args)
                                         (IfC (empty?/c (IdC args))
                                              (SuperC (UndefinedC) (UndefinedC))
                                              (IfC (eq?/c (LenC (IdC args)) (NumC 'int 1))
                                                   (SuperC (get-item/c (IdC args) 0)
                                                           (UndefinedC))
                                                   (SuperC (get-item/c (IdC args) 0)
                                                           (get-item/c (IdC args) 1))))
                                         (cl-env 'super)))))
                         (entV (obj-str "__getattr__")
                               (store-object
                                (let ([self 'self]
                                      [attr 'attr]
                                      [tmp-type 'tmp-type]
                                      [subtype 'subtype]
                                      [instance 'instance]
                                      [ret 'ret])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__getattr__"))))

                                         (list self attr)
                                         (lst-let/c
                                          (list
                                           (bindc tmp-type
                                                  (ObjC (IdC 'type)
                                                        (DictC
                                                         (list
                                                          (entC (StrC "__mro__")
                                                                (get-attr/c (IdC self) '__mro__))))
                                                        empty))
                                           (bindc instance
                                                  (get-attr/c (IdC self) '__instance__))
                                           (bindc subtype
                                                  (get-attr/c (IdC self) '__subtype__))
                                           (bindc ret (SearchMroC (IdC tmp-type) (IdC attr))))
                                          (IfC (is-callable/c (IdC ret))
                                               (IfC (eq?/c (UndefinedC) (IdC instance))
                                                    (IfC (eq?/c (UndefinedC) (IdC subtype))
                                                         (ReturnC (IdC ret))
                                                         (ReturnC (BindC (IdC ret) (IdC subtype))))
                                                    (ReturnC (BindC (IdC ret) (IdC instance))))
                                               (ReturnC (IdC ret))))
                                         (cl-env 'super)))))))))
    (hash-set! memory (lookup/boot 'function)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "funtion"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'function)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__call__")
                               (store-object
                                (let ([self 'self]
                                      [args 'args])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__call__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))

                                         (list self args)
                                         (DynAppC (IdC self)
                                                  (IdC args))
                                         (cl-env 'function)))))))))
    (hash-set! memory (lookup/boot 'int)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "int"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'int)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object
                                (let ([type 'stype]
                                      [args 'args]
                                      [num 'num])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__new__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))

                                         (list type args)
                                         (IfC (empty?/c (IdC args))
                                              (NumC 'int 0)
                                              (LetC num (get-item/c (IdC args) 0)
                                                    (IfC (Prim2C 'or
                                                                 (IsInstanceC (IdC num) (IdC 'float))
                                                                 (Prim2C 'or
                                                                         (IsInstanceC (IdC num) (IdC 'int))
                                                                         (IsInstanceC (IdC num) (IdC 'bool))))
                                                         (Prim2C 'floordiv
                                                                 (IdC num)
                                                                 (ObjC (IdC type)
                                                                       (DictC empty)
                                                                       (list (NumC 'int 1))))
                                                         (ErrorC 'TypeError (StrC "cannot cast non-numeric type to int")))))
                                         (cl-env 'int)))))))))
    (hash-set! memory (lookup/boot 'float)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "float"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'float)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object
                                (let ([type 'stype]
                                      [args 'args]
                                      [num 'num])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__new__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))

                                         (list type args)
                                         (IfC (empty?/c (IdC args))
                                              (NumC 'float 0.0)
                                              (LetC num (get-item/c (IdC args) 0)
                                                    (IfC (Prim2C 'or
                                                                 (IsInstanceC (IdC num) (IdC 'float))
                                                                 (Prim2C 'or
                                                                         (IsInstanceC (IdC num) (IdC 'int))
                                                                         (IsInstanceC (IdC num) (IdC 'bool))))
                                                         (Prim2C 'mul
                                                                 (ObjC (IdC type)
                                                                       (DictC empty)
                                                                       (list (NumC 'float 1.0)))
                                                                 (IdC num))
                                                         (ErrorC 'TypeError (StrC "cannot cast non-numeric type to float")))))
                                         (cl-env 'float)))))))))
    (hash-set! memory (lookup/boot 'iter)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "iter"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'iter)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__iter__")
                               (store-object
                                (let ([i-obj 'i-obj])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__iter__"))))

                                         (list i-obj)
                                         (IdC i-obj)
                                         (cl-env 'iter)))))
                         (entV (obj-str "__next/seq__")
                               (store-object
                                (let ([i-obj 'i-obj]
                                      [curr-idx 'curr-idx]
                                      [ret-item 'ret-item]
                                      [exn-sym 'exn-sym])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__next/seq__"))))

                                         (list i-obj)
                                         (TryCatchC
                                          (lst-let/c
                                           (list
                                            (bindc curr-idx
                                                   (get-attr/c (IdC i-obj) 'curr-idx))
                                            (bindc ret-item
                                                   (call-args/c
                                                    (get-attr/c
                                                     (get-attr/c (IdC i-obj) '__collection__)
                                                     '__getitem__)
                                                    (list (IdC curr-idx)))))
                                           (SeqC (set-attr/c (IdC i-obj) 'curr-idx (add1/c (IdC curr-idx)))
                                                 (IdC ret-item)))
                                          exn-sym
                                          (IfC (IsInstanceC (IdC exn-sym) (IdC 'IndexError))
                                               (SeqC
                                                (set-attr/c (IdC i-obj) '__stopped__ (TrueC))
                                                (ErrorC 'StopIteration (StrC "No more objects")))
                                               (DynErrorC (UndefinedC))))
                                         (cl-env 'iter)))))
                         (entV (obj-str "__next/callable__")
                               (store-object
                                (let ([i-obj 'i-obj]
                                      [ret-item 'ret-item])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__next/callable__"))))

                                         (list i-obj)
                                         (LetC ret-item
                                               (call-nullary/c
                                                (get-attr/c (IdC i-obj) '__collection__))
                                               (IfC (eq?/c (IdC ret-item)
                                                           (get-attr/c (IdC i-obj) 'sentinel))
                                                    (SeqC
                                                     (set-attr/c (IdC i-obj) '__stopped__ (TrueC))
                                                     (ErrorC 'StopIteration (StrC "no more objects")))
                                                    (IdC ret-item)))
                                         (cl-env 'iter)))))
                         (entV (obj-str "__next__")
                               (store-object
                                (let ([i-obj 'i-obj]
                                      [iter-type 'iter-type])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__next__"))))

                                         (list i-obj)
                                         (IfC (get-attr/c (IdC i-obj) '__stopped__)
                                              (ErrorC 'StopIteration (StrC "calling next after raised StopIteration"))
                                              (LetC iter-type
                                                    (get-attr/c (IdC i-obj) '__iter-type__)
                                                    (IfC (eq?/c (IdC iter-type) (StrC "seq"))
                                                         (call-nullary/c (get-attr/c (IdC i-obj) '__next/seq__))
                                                         (IfC (eq?/c (IdC iter-type) (StrC "callable"))
                                                              (call-nullary/c (get-attr/c (IdC i-obj) '__next/callable__))
                                                              (ErrorC 'TypeError (StrC "ill defined iter type"))))))
                                         (cl-env 'iter)))))
                         (entV (obj-str "__new__")
                               (store-object
                                (let ([type 'stype]
                                      [args 'args]
                                      [collection 'collection]
                                      [new-iter 'new-iter]
                                      [exn1 'exn1]
                                      [exn2 'exn2])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__new__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))
                                         (list type args)
                                         (IfC (empty?/c (IdC args))
                                              (ErrorC 'TypeError (StrC "iter needs one or two args"))
                                              (lst-let/c
                                               (list
                                                (bindc collection
                                                       (get-item/c (IdC args) 0))
                                                (bindc new-iter
                                                       (ObjC (IdC type)
                                                             (DictC
                                                              (list
                                                               (entC (StrC "__iter-type__") (StrC "undef"))
                                                               (entC (StrC "__collection__") (IdC collection))
                                                               (entC (StrC "__stopped__") (FalseC))))
                                                             empty)))
                                               (SeqC
                                                (IfC (eq?/c (LenC (IdC args)) (NumC 'int 1))
                                                     (IfC (IsInstanceC (get-item/c (IdC args) 0)
                                                                       (IdC 'iter))
                                                          (ReturnC (get-item/c (IdC args) 0))
                                                          (TryCatchC
                                                           (ReturnC
                                                            (call-nullary/c (get-attr/c (IdC collection) '__iter__)))
                                                           exn1
                                                           (IfC (IsInstanceC (IdC exn1) (IdC 'AttributeError))
                                                                (TryCatchC
                                                                 (IfC (call-args/c (IdC 'callable)
                                                                                   (list
                                                                                    (get-attr/c (IdC collection) '__getitem__)))
                                                                      (SeqC (set-attr/c (IdC new-iter) '__iter-type__ (StrC "seq"))
                                                                            (set-attr/c (IdC new-iter) 'curr-idx (NumC 'int 0)))
                                                                      (ErrorC 'TypeError (StrC "fake seq object passed to iter")))
                                                                 exn2
                                                                 (IfC (IsInstanceC (IdC exn2) (IdC 'AttributeError))
                                                                      (ErrorC 'TypeError (StrC "object passed to iter is neither iterable nor sequence"))
                                                                      (DynErrorC (UndefinedC))))
                                                                (DynErrorC (UndefinedC)))))
                                                     (IfC (call-args/c (IdC 'callable)
                                                                       (list (IdC collection)))
                                                          (SeqC (set-attr/c (IdC new-iter) '__iter-type__ (StrC "callable"))
                                                                (set-attr/c (IdC new-iter) 'sentinel (get-item/c (IdC args) 1)))
                                                          (ErrorC 'TypeError (StrC "first of two arguments to iter not a callable"))))
                                                (IdC new-iter))))
                                         (cl-env 'iter)))))))))
    (hash-set! memory (lookup/boot 'range)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "range"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'range)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__getitem__")
                               (store-object
                                (let ([self 'self]
                                      [idx 'idx]
                                      [start 'start]
                                      [stop 'stop]
                                      [step 'step]
                                      [ret 'ret])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__getitem__"))))

                                         (list self idx)
                                         (lst-let/c
                                          (list
                                           (bindc start (get-attr/c (IdC self) '__start__))
                                           (bindc stop (get-attr/c (IdC self) '__stop__))
                                           (bindc step (get-attr/c (IdC self) '__step__))
                                           (bindc ret
                                                  (Prim2C 'add
                                                          (get-attr/c (IdC self) '__start__)
                                                          (Prim2C 'mul
                                                                  (IdC idx)
                                                                  (get-attr/c (IdC self) '__step__)))))
                                          (IfC (Prim2C 'le
                                                       (IdC step)
                                                       (NumC 'int 0))
                                               (IfC (Prim2C 'le
                                                            (IdC ret)
                                                            (IdC stop))
                                                    (ErrorC 'IndexError (StrC "exhaisted range"))
                                                    (IdC ret))
                                               (IfC (Prim2C 'ge
                                                            (IdC ret)
                                                            (IdC stop))
                                                    (ErrorC 'IndexError (StrC "exhaisted range"))
                                                    (IdC ret))))
                                         (cl-env 'range)))))
                         (entV (obj-str "__iter__")
                               (store-object
                                (let ([self 'self]
                                      [idx 'idx])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__iter__"))))

                                         (list self)
                                         (call-args/c
                                          (IdC 'iter)
                                          (list
                                           (ObjC (IdC 'object)
                                                 (DictC
                                                  (list
                                                   (entC (StrC "__getitem__")
                                                         (LamC (list idx)
                                                               (call-args/c
                                                                (get-attr/c (IdC self) '__getitem__)
                                                                (list (IdC idx)))))))
                                                 empty)))
                                         (cl-env 'range)))))
                         (entV (obj-str "__new__")
                               (store-object
                                (let ([type 'stype]
                                      [args 'args]
                                      [new-trp 'new-trp]
                                      [start 'start]
                                      [stop 'stop]
                                      [step 'step])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__new__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))
                                         (list type args)
                                         (LetC new-trp
                                               (ObjC (IdC type)
                                                     (DictC
                                                      (list
                                                       (entC (StrC "__start__") (StrC "undef"))
                                                       (entC (StrC "__stop__") (StrC "undef"))
                                                       (entC (StrC "__step__") (StrC "undef"))))
                                                     empty)
                                               (SeqC
                                                (IfC (eq?/c (LenC (IdC args)) (NumC 'int 0))
                                                     (ErrorC 'TypeError (StrC "range needs at least one arg"))
                                                     (IfC (eq?/c (LenC (IdC args)) (NumC 'int 1))
                                                          (lst-let/c
                                                           (list
                                                            (bindc stop (get-item/c (IdC args) 0)))
                                                           (IfC (IsInstanceC (IdC stop) (IdC 'int))
                                                                (SeqC
                                                                 (set-attr/c (IdC new-trp)
                                                                             '__start__
                                                                             (NumC 'int 0))
                                                                 (SeqC
                                                                  (set-attr/c (IdC new-trp)
                                                                              '__stop__
                                                                              (IdC stop))
                                                                  (set-attr/c (IdC new-trp)
                                                                              '__step__
                                                                              (NumC 'int 1))))
                                                                (ErrorC 'TypeError (StrC "range: first arg not an int"))))
                                                          (IfC (eq?/c (LenC (IdC args)) (NumC 'int 2))
                                                               (lst-let/c
                                                                (list
                                                                 (bindc start (get-item/c (IdC args) 0))
                                                                 (bindc stop (get-item/c (IdC args) 1)))
                                                                (IfC (Prim2C 'and
                                                                             (IsInstanceC (IdC start) (IdC 'int))
                                                                             (IsInstanceC (IdC stop) (IdC 'int)))
                                                                     (SeqC
                                                                      (set-attr/c (IdC new-trp)
                                                                                  '__start__
                                                                                  (IdC start))
                                                                      (SeqC
                                                                       (set-attr/c (IdC new-trp)
                                                                                   '__stop__
                                                                                   (IdC stop))
                                                                       (set-attr/c (IdC new-trp)
                                                                                   '__step__
                                                                                   (NumC 'int 1))))
                                                                     (ErrorC 'TypeError (StrC "range: args are not ints"))))
                                                               (IfC (eq?/c (LenC (IdC args)) (NumC 'int 3))
                                                                    (lst-let/c
                                                                     (list
                                                                      (bindc start (get-item/c (IdC args) 0))
                                                                      (bindc stop (get-item/c (IdC args) 1))
                                                                      (bindc step (get-item/c (IdC args) 2)))
                                                                     (IfC (Prim2C 'and
                                                                                  (IsInstanceC (IdC start) (IdC 'int))
                                                                                  (Prim2C 'and
                                                                                          (IsInstanceC (IdC stop) (IdC 'int))
                                                                                          (IsInstanceC (IdC step) (IdC 'int))))
                                                                          (IfC (eq?/c (IdC step) (NumC 'int 0))
                                                                               (ErrorC 'ValueError (StrC "range: arg 3 must not be zero"))
                                                                               (SeqC
                                                                                (set-attr/c (IdC new-trp)
                                                                                            '__start__
                                                                                            (IdC start))
                                                                                (SeqC
                                                                                 (set-attr/c (IdC new-trp)
                                                                                             '__stop__
                                                                                             (IdC stop))
                                                                                 (set-attr/c (IdC new-trp)
                                                                                             '__step__
                                                                                             (IdC step)))))
                                                                          (ErrorC 'TypeError (StrC "range: args are not ints"))))
                                                                    (ErrorC 'TypeError (StrC "range takes at most 3 args"))))))
                                                (IdC new-trp)))
                                         (cl-env 'range)))))))))
    (hash-set! memory (lookup/boot 'filter)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "filter"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'filter)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__iter__")
                               (store-object
                                (let ([i-obj 'i-obj])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__iter__"))))

                                         (list i-obj)
                                         (IdC i-obj)
                                         (cl-env 'filter)))))
                         (entV (obj-str "__next__")
                               (store-object
                                (let ([i-obj 'i-obj]
                                      [get-next 'get-next]
                                      [get-next/impl 'get-next/impl]
                                      [ret-item 'ret-item])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__next__"))))

                                         (list i-obj)
                                         (lst-let/c
                                          (list
                                           (bindc get-next
                                                  (LamC empty (ErrorC 'Exception (StrC "not defined yet"))))
                                           (bindc get-next/impl
                                                  (LamC empty
                                                        (LetC ret-item
                                                              (call-nullary/c
                                                               (get-attr/c
                                                                (get-attr/c (IdC i-obj) 'iter)
                                                                '__next__))
                                                              (IfC (call-args/c (get-attr/c (IdC i-obj) 'check)
                                                                                (list (IdC ret-item)))
                                                                   (IdC ret-item)
                                                                   (AppC (IdC get-next) empty))))))
                                          (SeqC
                                           (Set!C get-next (IdC get-next/impl))
                                           (AppC (IdC get-next) empty)))
                                         (cl-env 'filter)))))
                         (entV (obj-str "__new__")
                               (store-object
                                (let ([type 'stype]
                                      [args 'args]
                                      [item 'item])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__new__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))
                                         (list type args)
                                         (ObjC (IdC type)
                                               (DictC
                                                (list
                                                 (entC (StrC "check")
                                                       (IfC (eq?/c (get-item/c (IdC args) 0)
                                                                   (NoneC))
                                                            (LamC (list item) (Prim1C 'truth (IdC item)))
                                                            (get-item/c (IdC args) 0)))
                                                 (entC (StrC "iter") (call-nullary/c
                                                                      (get-attr/c (get-item/c (IdC args) 1)
                                                                                  '__iter__)))))
                                               empty)
                                         (cl-env 'filter)))))))))
    (hash-set! memory (lookup/boot 'map)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "map"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'map)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__iter__")
                               (store-object
                                (let ([i-obj 'i-obj])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__iter__"))))

                                         (list i-obj)
                                         (IdC i-obj)
                                         (cl-env 'map)))))
                         (entV (obj-str "__next__")
                               (store-object
                                (let ([i-obj 'i-obj])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__next__"))))

                                         (list i-obj)
                                         (call-args/c (get-attr/c (IdC i-obj)
                                                                  'transform)
                                                      (list
                                                       (call-nullary/c (get-attr/c
                                                                        (get-attr/c (IdC i-obj) 'iter)
                                                                        '__next__))))
                                         (cl-env 'map)))))
                         (entV (obj-str "__new__")
                               (store-object
                                (let ([type 'stype]
                                      [args 'args]
                                      [item 'item])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__new__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))
                                         (list type args)
                                         (ObjC (IdC type)
                                               (DictC
                                                (list
                                                 (entC (StrC "transform")
                                                       (IfC (eq?/c (get-item/c (IdC args) 0)
                                                                   (NoneC))
                                                            (LamC (list item) (IdC item))
                                                            (get-item/c (IdC args) 0)))
                                                 (entC (StrC "iter") (call-nullary/c
                                                                      (get-attr/c (get-item/c (IdC args) 1)
                                                                                  '__iter__)))))
                                               empty)
                                         (cl-env 'map)))))))))
    (hash-set! memory (lookup/boot 'str)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "str"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'str)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object
                                (let ([type 'stype]
                                      [args 'args]
                                      [ret-str 'ret-str]
                                      [exn 'exn])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__new__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))
                                         (list type args)
                                         (LetC ret-str (get-item/c (IdC args) 0)
                                               (IfC (IsSubclassC (TypeC (IdC ret-str)) (IdC 'str))
                                                    (IdC ret-str)
                                                    (TryCatchC
                                                     (call-nullary/c
                                                      (get-attr/c (IdC ret-str) '__str__))
                                                     exn
                                                     (IfC (IsInstanceC (IdC exn)
                                                                       (IdC 'AttributeError))
                                                          (ToStrC (IdC ret-str))
                                                          (DynErrorC (UndefinedC))))))
                                         (cl-env 'str)))))
                         (entV (obj-str "__getitem__")
                               (store-object
                                (let ([self 'self]
                                      [idx 'idx])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__getitem__"))))

                                         (list self idx)
                                         (GetItemAtIdxC (IdC self) (IdC idx))
                                         (cl-env 'str)))))
                         (entV (obj-str "__iter__")
                               (store-object (attribute_iter_lambda 'str)))))))
    (hash-set! memory (lookup/boot 'tuple)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "tuple"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'tuple)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object (iterable_new_lambda 'tuple TupleC #f)))
                         (entV (obj-str "__getitem__")
                               (store-object
                                (let ([self 'self]
                                      [idx 'idx])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__getitem__"))))

                                         (list self idx)
                                         (GetItemAtIdxC (IdC self) (IdC idx))
                                         (cl-env 'tuple)))))
                         (entV (obj-str "__iter__")
                               (store-object (attribute_iter_lambda 'tuple)))))))
    (hash-set! memory (lookup/boot 'list)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "list"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'list)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object (iterable_new_lambda 'list ListC #t)))
                         (entV (obj-str "append")
                               (store-object
                                (let ([self 'self]
                                      [data 'data])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "append"))))

                                         (list self data)
                                         (AppendData!C (IdC self) (IdC data))
                                         (cl-env 'list)))))
                         (entV (obj-str "extend")
                               (store-object
                                (let ([self 'self]
                                      [data 'data]
                                      [iter 'iter]
                                      [exn 'exn])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "extend"))))

                                         (list self data)
                                         (LetC iter
                                               (call-nullary/c (get-attr/c (IdC data) '__iter__))
                                               (WhileC (TrueC)
                                                       (TryCatchC
                                                        (AppendData!C (IdC self)
                                                                      (call-nullary/c (get-attr/c (IdC iter)
                                                                                                  '__next__)))
                                                        exn
                                                        (IfC (IsInstanceC (IdC exn)
                                                                          (IdC 'StopIteration))
                                                             (BreakC)
                                                             (DynErrorC (UndefinedC))))
                                                       (NoneC)))
                                         (cl-env 'list)))))
                         (entV (obj-str "__getitem__")
                               (store-object
                                (let ([self 'self]
                                      [idx 'idx])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__getitem__"))))

                                         (list self idx)
                                         (GetItemAtIdxC (IdC self) (IdC idx))
                                         (cl-env 'list)))))
                         (entV (obj-str "__iter__")
                               (store-object (attribute_iter_lambda 'list)))))))
    (hash-set! memory (lookup/boot 'dict)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "dict"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'dict)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object
                                (let ([type 'stype]
                                      [args 'args])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__new__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))

                                         (list type args)
                                         (IfC (empty?/c (IdC args))
                                              (DictC empty)
                                              (IfC (IsInstanceC (get-item/c (IdC args) 0) (IdC type))
                                                   (CopyObjC (get-item/c (IdC args) 0))
                                                   (ErrorC 'TypeError (StrC "dict can only instantiate from dict objects"))))
                                         (cl-env 'dict)))))
                         (entV (obj-str "__iter__")
                               (store-object (attribute_iter_lambda 'dict)))
                         (entV (obj-str "keys")
                               (store-object
                                (let ([self 'self])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "keys"))))

                                         (list self)
                                         (call-args/c (IdC 'set)
                                                      (list
                                                       (call-nullary/c (get-attr/c (IdC self)
                                                                                   '__iter__))))
                                         (cl-env 'dict)))))
                         (entV (obj-str "items")
                               (store-object
                                (let ([self 'self]
                                      [key 'key])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "items"))))

                                         (list self)
                                         (call-args/c
                                          (IdC 'set)
                                          (list
                                           (call-args/c
                                            (IdC 'map)
                                            (list
                                             (LamC (list key)
                                                   (TupleC (list (IdC key)
                                                                 (call-args/c (get-attr/c (IdC self)
                                                                                          '__getitem__)
                                                                              (list (IdC key))))))
                                             (call-nullary/c (get-attr/c (IdC self)
                                                                         '__iter__))))))
                                         (cl-env 'dict)))))
                         (entV (obj-str "values")
                               (store-object
                                (let ([self 'self]
                                      [key 'key])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "values"))))

                                         (list self)
                                         (call-args/c
                                          (IdC 'set)
                                          (list
                                           (call-args/c
                                            (IdC 'map)
                                            (list
                                             (LamC (list key)
                                                   (call-args/c (get-attr/c (IdC self)
                                                                            '__getitem__)
                                                                (list (IdC key))))
                                             (call-nullary/c (get-attr/c (IdC self)
                                                                         '__iter__))))))
                                         (cl-env 'dict)))))
                         (entV (obj-str "update")
                               (store-object
                                (let ([self 'self]
                                      [arg-list 'arg-list]
                                      [dict/o 'dict/o]
                                      [item 'item]
                                      [iter 'iter]
                                      [add-one 'add-one]
                                      [add-one/impl 'add-one/impl]
                                      [exn-sym 'exn-sym])
                                  (ClosV (lookup/boot 'function)
                                         (box
                                          (list
                                           (entV (obj-str "__name__")
                                                 (obj-str "update"))
                                           (entV (obj-str "__has-vararg__")
                                                 (get-singleton-object 'True))))
                                         (list self arg-list)
                                         (IfC (empty?/c (IdC arg-list))
                                              (NoneC)
                                              (LetC dict/o (get-item/c (IdC arg-list) 0)
                                                    (IfC (IsInstanceC (IdC dict/o) (IdC 'dict))
                                                         (lst-let/c
                                                          (list
                                                           (bindc iter
                                                                  (call-nullary/c
                                                                   (get-attr/c
                                                                    (call-nullary/c
                                                                     (get-attr/c (IdC dict/o) 'items))
                                                                    '__iter__)))
                                                           (bindc add-one
                                                                  (LamC empty (ErrorC 'Exception (StrC "undefined add-one"))))
                                                           (bindc add-one/impl
                                                                  (LamC empty
                                                                        (TryCatchC
                                                                         (SeqC
                                                                          (LetC item
                                                                                (call-nullary/c (get-attr/c (IdC iter) '__next__))
                                                                                (SetData!C (IdC self)
                                                                                           (get-item/c (IdC item) 0)
                                                                                           (get-item/c (IdC item) 1)))
                                                                          (AppC (IdC add-one) empty))
                                                                         exn-sym
                                                                         (IfC (IsInstanceC (IdC exn-sym) (IdC 'StopIteration))
                                                                              (NoneC)
                                                                              (DynErrorC (UndefinedC)))))))
                                                          (seq/c
                                                           (list
                                                            (Set!C add-one (IdC add-one/impl))
                                                            (AppC (IdC add-one) empty))))
                                                         (ErrorC 'TypeError (StrC "can't update with non-dict objects")))))
                                         (cl-env 'dict)))))
                         (entV (obj-str "clear")
                               (store-object
                                (let ([self 'self]
                                      [remove-first 'remove-first]
                                      [remove-first/impl 'remove-first/impl]
                                      [exn-sym 'exn-sym])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "clear"))))

                                         (list self)
                                         (lst-let/c
                                          (list
                                           (bindc remove-first
                                                  (LamC empty (ErrorC 'Exception (StrC "undefined remove-first"))))
                                           (bindc remove-first/impl
                                                  (LamC empty
                                                        (TryCatchC
                                                         (SeqC (DelData!C (IdC self)
                                                                          (GetItemAtIdxC (IdC self)
                                                                                         (NumC 'int 0)))
                                                               (AppC (IdC remove-first) empty))
                                                         exn-sym
                                                         (IfC (IsInstanceC (IdC exn-sym) (IdC 'IndexError))
                                                              (NoneC)
                                                              (DynErrorC (UndefinedC)))))))
                                          (seq/c
                                           (list
                                            (Set!C remove-first (IdC remove-first/impl))
                                            (AppC (IdC remove-first) empty))))
                                         (cl-env 'dict)))))
                         (entV (obj-str "__getitem__")
                               (store-object
                                (let ([self 'self]
                                      [key 'key])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__getitem__"))))

                                         (list self key)
                                         (GetDataC (IdC self) (IdC key))
                                         (cl-env 'dict)))))
                         (entV (obj-str "get")
                               (store-object
                                (let ([self 'self]
                                      [args 'args]
                                      [exn-sym 'exn-sym])
                                  (ClosV (lookup/boot 'function)
                                         (box
                                          (list
                                           (entV (obj-str "__name__")
                                                 (obj-str "get"))
                                           (entV (obj-str "__has-vararg__")
                                                 (get-singleton-object 'True))))
                                         (list self args)
                                         (IfC (empty?/c (IdC args))
                                              (ErrorC 'TypeError (StrC "not enough arguments to get of dict type"))
                                              (TryCatchC
                                               (GetDataC (IdC self) (get-item/c (IdC args) 0))
                                               exn-sym
                                               (IfC (IsInstanceC (IdC exn-sym) (IdC 'KeyError))
                                                    (IfC (eq?/c (LenC (IdC args)) (NumC 'int 1))
                                                         (NoneC)
                                                         (get-item/c (IdC args) 1))
                                                    (DynErrorC (UndefinedC)))))
                                         (cl-env 'dict)))))
                         (entV (obj-str "__setitem__")
                               (store-object
                                (let ([self 'self]
                                      [key 'key]
                                      [val 'val])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__setitem__"))))

                                         (list self key val)
                                         (SetData!C (IdC self) (IdC key) (IdC val))
                                         (cl-env 'dict)))))))))
    (hash-set! memory (lookup/boot 'set)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "set"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'set)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object (iterable_new_lambda 'set SetC #t)))
                         (entV (obj-str "__iter__")
                               (store-object (attribute_iter_lambda 'set)))))))
    (hash-set! memory (lookup/boot 'frozenset)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "frozenset"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'frozenset)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object (iterable_new_lambda 'frozenset FrozenSetC #f)))
                         (entV (obj-str "__iter__")
                               (store-object (attribute_iter_lambda 'frozenset)))))))
    (hash-set! memory (lookup/boot 'slice)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "slice"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'slice)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object
                                (let ([type 'stype]
                                      [args 'args]
                                      [new-trp 'new-trp]
                                      [start 'start]
                                      [stop 'stop]
                                      [step 'step])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__new__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))
                                         (list type args)
                                         (LetC new-trp
                                               (ObjC (IdC type)
                                                     (DictC
                                                      (list
                                                       (entC (StrC "__start__") (NoneC))
                                                       (entC (StrC "__stop__") (NoneC))
                                                       (entC (StrC "__step__") (NoneC))))
                                                     empty)
                                               (SeqC
                                                (IfC (eq?/c (LenC (IdC args)) (NumC 'int 0))
                                                     (ErrorC 'TypeError (StrC "slice needs at least one arg"))
                                                     (IfC (eq?/c (LenC (IdC args)) (NumC 'int 1))
                                                          (lst-let/c
                                                           (list
                                                            (bindc stop (get-item/c (IdC args) 0)))
                                                           (IfC (or/c (IsInstanceC (IdC stop) (IdC 'int))
                                                                      (eq?/c (IdC stop) (IdC 'None)))
                                                                (set-attr/c (IdC new-trp)
                                                                            '__stop__
                                                                            (IdC stop))
                                                                (ErrorC 'TypeError (StrC "slice: first arg not an int or none"))))
                                                          (IfC (eq?/c (LenC (IdC args)) (NumC 'int 2))
                                                               (lst-let/c
                                                                (list
                                                                 (bindc start (get-item/c (IdC args) 0))
                                                                 (bindc stop (get-item/c (IdC args) 1)))
                                                                (IfC (and/c (or/c (IsInstanceC (IdC start) (IdC 'int))
                                                                                  (eq?/c (IdC start) (IdC 'None)))
                                                                            (or/c (IsInstanceC (IdC stop) (IdC 'int))
                                                                                  (eq?/c (IdC stop) (IdC 'None))))
                                                                     (SeqC
                                                                      (set-attr/c (IdC new-trp)
                                                                                  '__start__
                                                                                  (IdC start))
                                                                      (set-attr/c (IdC new-trp)
                                                                                  '__stop__
                                                                                  (IdC stop)))
                                                                     (ErrorC 'TypeError (StrC "slice: args are not ints or none"))))
                                                               (IfC (eq?/c (LenC (IdC args)) (NumC 'int 3))
                                                                    (lst-let/c
                                                                     (list
                                                                      (bindc start (get-item/c (IdC args) 0))
                                                                      (bindc stop (get-item/c (IdC args) 1))
                                                                      (bindc step (get-item/c (IdC args) 2)))
                                                                     (IfC (and/c (or/c (IsInstanceC (IdC start) (IdC 'int))
                                                                                       (eq?/c (IdC start) (IdC 'None)))
                                                                                 (and/c (or/c (IsInstanceC (IdC stop) (IdC 'int))
                                                                                              (eq?/c (IdC stop) (IdC 'None)))
                                                                                        (or/c (IsInstanceC (IdC step) (IdC 'int))
                                                                                              (eq?/c (IdC step) (IdC 'None)))))
                                                                          (SeqC
                                                                           (set-attr/c (IdC new-trp)
                                                                                       '__start__
                                                                                       (IdC start))
                                                                           (SeqC
                                                                            (set-attr/c (IdC new-trp)
                                                                                        '__stop__
                                                                                        (IdC stop))
                                                                            (set-attr/c (IdC new-trp)
                                                                                        '__step__
                                                                                        (IdC step))))
                                                                          (ErrorC 'TypeError (StrC "slice: args are not ints"))))
                                                                    (ErrorC 'TypeError (StrC "slice takes at most 3 args"))))))
                                                (IdC new-trp)))
                                         (cl-env 'slice)))))))))
    (hash-set! memory (lookup/boot 'Exception)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "Exception"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'Exception)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object
                                (let ([type 'stype]
                                      [args 'args])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__new__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))
                                         (list type args)
                                         (IfC (empty?/c (IdC args))
                                              (ObjC (IdC type) (DictC empty)
                                                    (list (StrC "")))
                                              (ObjC (IdC type) (DictC empty)
                                                    (list (concat/c (IdC args)))))
                                         (cl-env 'Exception)))))))))
    (hash-set! memory (lookup/boot 'bool)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "bool"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'int))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'bool)
                                              (lookup/boot 'int)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object
                                (let ([type 'stype]
                                      [args 'args])
                                  (ClosV (lookup/boot 'function)
                                         (box (list (entV (obj-str "__name__")
                                                          (obj-str "__new__"))
                                                    (entV (obj-str "__has-vararg__")
                                                          (get-singleton-object 'True))))
                                         (list type args)
                                         (IfC (empty?/c (IdC args))
                                              (FalseC)
                                              (Prim1C 'truth (get-item/c (IdC args) 0)))
                                         (cl-env 'bool)))))))))
    (hash-set! memory (lookup/boot 'NoneType)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "NoneType"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'NoneType)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object (singleton_new_lambda 'NoneType)))))))
    (hash-set! memory (lookup/boot 'ellipsis)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "ellipsis"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'object))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'ellipsis)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__new__")
                               (store-object (singleton_new_lambda 'ellipsis)))))))
    (hash-set! memory (lookup/boot 'NameError)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "NameError"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'Exception))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'Exception)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'NameError)
                                              (lookup/boot 'Exception)
                                              (lookup/boot 'object))))))))
    (hash-set! memory (lookup/boot 'RuntimeError)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "RuntimeError"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'Exception))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'Exception)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'RuntimeError)
                                              (lookup/boot 'Exception)
                                              (lookup/boot 'object))))))))
    (hash-set! memory (lookup/boot 'AssertionError)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "AssertionError"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'Exception))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'Exception)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'AssertionError)
                                              (lookup/boot 'Exception)
                                              (lookup/boot 'object))))))))
    (hash-set! memory (lookup/boot 'UnboundLocalError)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "UnboundLocalError"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'Exception))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'Exception)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'UnboundLocalError)
                                              (lookup/boot 'Exception)
                                              (lookup/boot 'object))))))))
    (hash-set! memory (lookup/boot 'TypeError)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "TypeError"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'Exception))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'Exception)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'TypeError)
                                              (lookup/boot 'Exception)
                                              (lookup/boot 'object))))))))
    (hash-set! memory (lookup/boot 'AttributeError)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "AttributeError"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'Exception))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'Exception)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'AttributeError)
                                              (lookup/boot 'Exception)
                                              (lookup/boot 'object))))))))
    (hash-set! memory (lookup/boot 'KeyError)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "KeyError"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'Exception))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'Exception)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'KeyError)
                                              (lookup/boot 'Exception)
                                              (lookup/boot 'object))))))))
    (hash-set! memory (lookup/boot 'IndexError)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "IndexError"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'Exception))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'Exception)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'IndexError)
                                              (lookup/boot 'Exception)
                                              (lookup/boot 'object))))))))
    (hash-set! memory (lookup/boot 'ZeroDivisionError)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "ZeroDivisionError"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'Exception))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'Exception)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'ZeroDivisionError)
                                              (lookup/boot 'Exception)
                                              (lookup/boot 'object))))))))
    (hash-set! memory (lookup/boot 'ValueError)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "ValueError"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'Exception))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'Exception)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'ValueError)
                                              (lookup/boot 'Exception)
                                              (lookup/boot 'object))))))))
    (hash-set! memory (lookup/boot 'StopIteration)
               (EmptyV (lookup/boot 'type)
                       (box
                        (list
                         (entV (obj-str "__name__")
                               (obj-str "StopIteration"))
                         (entV (obj-str "__base__")
                               (lookup/boot 'Exception))
                         (entV (obj-str "__bases__")
                               (obj-tup (list (lookup/boot 'Exception)
                                              (lookup/boot 'object))))
                         (entV (obj-str "__mro__")
                               (obj-tup (list (lookup/boot 'StopIteration)
                                              (lookup/boot 'Exception)
                                              (lookup/boot 'object))))))))
    (hash-set! memory (lookup/boot 'Undefined)
               (UndefinedV))
    (hash-set! memory (lookup/boot 'None)
               (StrV (lookup/boot 'NoneType) (box empty) "None"))
    (hash-set! memory (lookup/boot 'True)
               (NumV (lookup/boot 'bool) (box empty) 1))
    (hash-set! memory (lookup/boot 'False)
               (NumV (lookup/boot 'bool) (box empty) 0))
    (hash-set! memory (lookup/boot 'Ellipsis)
               (StrV (lookup/boot 'ellipsis) (box empty) "Ellipsis"))
    (hash-set! memory (lookup/boot 'print)
               (let ([args 'args]
                     [curr-idx 'curr-idx])
                 (ClosV (lookup/boot 'function)
                        (box
                         (list
                          (entV (obj-str "__name__") (obj-str "print"))
                          (entV (obj-str "__has-vararg__") (get-singleton-object 'True))))
                        (list args)
                        (LetC curr-idx (NumC 'int 0)
                              (WhileC (Prim2C 'lt
                                              (IdC curr-idx)
                                              (LenC (IdC args)))
                                      (SeqC
                                       (Prim1C 'print
                                               (GetDataC (IdC args) (IdC curr-idx)))
                                       (Set!C curr-idx
                                              (add1/c (IdC curr-idx))))
                                      (NoneC)))
                        builtin-env)))
    (hash-set! memory (lookup/boot 'min)
               (let ([args 'args])
                 (ClosV (lookup/boot 'function)
                        (box
                         (list
                          (entV (obj-str "__name__") (obj-str "min"))
                          (entV (obj-str "__has-vararg__") (get-singleton-object 'True))))
                        (list args)
                        (IfC (empty?/c (IdC args))
                             (ErrorC 'TypeError (StrC "min got 0 args"))
                             (IfC (eq?/c (LenC (IdC args))
                                         (NumC 'int 1))
                                  (Prim1C 'min (get-item/c (IdC args) 0))
                                  (Prim1C 'min (IdC args))))
                        builtin-env)))
    (hash-set! memory (lookup/boot 'max)
               (let ([args 'args])
                 (ClosV (lookup/boot 'function)
                        (box
                         (list
                          (entV (obj-str "__name__") (obj-str "max"))
                          (entV (obj-str "__has-vararg__") (get-singleton-object 'True))))
                        (list args)
                        (IfC (empty?/c (IdC args))
                             (ErrorC 'TypeError (StrC "min got 0 args"))
                             (IfC (eq?/c (LenC (IdC args))
                                         (NumC 'int 1))
                                  (Prim1C 'max (get-item/c (IdC args) 0))
                                  (Prim1C 'max (IdC args))))
                        builtin-env)))
    (hash-set! memory (lookup/boot '___assertFail)
               (ClosV (lookup/boot 'function)
                      (box (list (entV (obj-str "__name__")
                                       (obj-str "___assertFail"))))
                      empty
                      (ErrorC 'AssertionError (StrC "assert failed"))
                      builtin-env))
    (hash-set! memory (lookup/boot '___assertFalse)
               (let ([check-true 'check-true])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "___assertFalse"))))
                        (list check-true)
                        (IfC (IdC check-true) (ErrorC 'AssertionError (StrC "Assert False failed")) (TrueC))
                        builtin-env)))
    (hash-set! memory (lookup/boot '___assertIn)
               (let ([coll 'coll]
                     [data 'data])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "___assertIn"))))
                        (list coll data)
                        (IfC (Prim2C 'in (IdC coll) (IdC data))
                             (TrueC)
                             (ErrorC 'AssertionError (StrC "Assert in failed")))
                        builtin-env)))
    (hash-set! memory (lookup/boot '___assertNotIn)
               (let ([coll 'coll]
                     [data 'data])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "___assertNotIn"))))
                        (list coll data)
                        (IfC (Prim2C 'notin (IdC coll) (IdC data))
                             (TrueC)
                             (ErrorC 'AssertionError (StrC "Assert not in failed")))
                        builtin-env)))
    (hash-set! memory (lookup/boot '___assertEqual)
               (let ([l 'l]
                     [r 'r])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "___assertEqual"))))
                        (list l r)
                        (IfC (Prim2C 'eq (IdC l) (IdC r))
                             (TrueC)
                             (ErrorC 'AssertionError
                                     (concat/c
                                      (TupleC
                                       (list (StrC "Assert equal failed. left: ")
                                             (IdC l)
                                             (StrC ", right: ")
                                             (IdC r))))))
                        builtin-env)))
    (hash-set! memory (lookup/boot '___assertNotEqual)
               (let ([l 'l]
                     [r 'r])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "___assertNotEqual"))))
                        (list l r)
                        (IfC (Prim2C 'ne (IdC 'l) (IdC 'r))
                             (TrueC)
                             (ErrorC 'AssertionError (StrC "Assert not equal failed")))
                        builtin-env)))
    (hash-set! memory (lookup/boot 'abs)
               (let ([arg 'arg])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "abs"))))
                        (list arg)
                        (Prim1C 'abs (IdC arg))
                        builtin-env)))
    (hash-set! memory (lookup/boot '___assertIs)
               (let ([l 'l]
                     [r 'r])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "___assertIs"))))
                        (list l r)
                        (IfC (Prim2C 'is (IdC l) (IdC r)) (TrueC) (ErrorC 'AssertionError (StrC "Assert is failed")))
                        builtin-env)))
    (hash-set! memory (lookup/boot '___assertIsNot)
               (let ([l 'l]
                     [r 'r])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "___assertIsNot"))))
                        (list l r)
                        (IfC (Prim2C 'isnot (IdC l) (IdC r)) (TrueC) (ErrorC 'AssertionError (StrC "Assert is not failed")))
                        builtin-env)))
    (hash-set! memory (lookup/boot '___assertTrue)
               (let ([check-true 'check-true])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "___assertTrue"))))
                        (list check-true)
                        (IfC (IdC check-true) (TrueC) (ErrorC 'AssertionError (StrC "Assert True failed")))
                        builtin-env)))
    (hash-set! memory (lookup/boot '___assertRaises)
               (let ([args 'args]
                     [exn 'exn]
                     [candidates 'candidates])
                 (ClosV (lookup/boot 'function)
                        (box
                         (list
                          (entV (obj-str "__name__") (obj-str "___assertRaises"))
                          (entV (obj-str "__has-vararg__") (get-singleton-object 'True))))
                        (list args)
                        (TryCatchC
                         (DynAppC (GetCallableC (get-item/c (IdC args) 1))
                                  (GetDataC (IdC args)
                                            (call-args/c (IdC 'slice)
                                                         (list (NumC 'int 2) (NoneC) (NoneC)))))
                         exn
                         (LetC candidates (get-item/c (IdC args) 0)
                               (IfC (eq?/c (TypeC (IdC candidates)) (IdC 'type))
                                    (IfC (IsInstanceC (IdC exn)
                                                      (IdC candidates))
                                         (TrueC)
                                         (FalseC))
                                    (IfC (Prim2C 'in (TypeC (IdC exn)) (IdC candidates))
                                         (TrueC)
                                         (FalseC)))))
                        builtin-env)))
    (hash-set! memory (lookup/boot 'issubclass)
               (let ([cls 'cls]
                     [super 'super])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "issubclass"))))
                        (list cls super)
                        (IsSubclassC (IdC cls) (IdC super))
                        builtin-env)))
    (hash-set! memory (lookup/boot 'isinstance)
               (let ([cls 'cls]
                     [obj 'obj])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "isinstance"))))
                        (list obj cls)
                        (IsInstanceC (IdC obj) (IdC cls))
                        builtin-env)))
    (hash-set! memory (lookup/boot 'len)
               (let ([obj 'obj])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "len"))))
                        (list obj)
                        (LenC (IdC obj))
                        builtin-env)))
    (hash-set! memory (lookup/boot 'globals)
               (ClosV (lookup/boot 'function)
                      (box (list (entV (obj-str "__name__")
                                       (obj-str "globals"))))
                      empty
                      (RunCoreC 'compute-globals empty)
                      builtin-env))
    (hash-set! memory (lookup/boot 'locals)
               (ClosV (lookup/boot 'function)
                      (box (list (entV (obj-str "__name__")
                                       (obj-str "locals"))))
                      empty
                      (RunCoreC 'compute-locals empty)
                      builtin-env))
    (hash-set! memory (lookup/boot 'callable)
               (let ([obj 'obj]
                     [exn 'exn])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "callable"))))

                        (list obj)
                        (TryCatchC
                         (SeqC
                          (SearchMroC (TypeC (IdC obj)) (StrC "__call__"))
                          (TrueC))
                         exn
                         (IfC (IsInstanceC (IdC exn) (IdC 'AttributeError))
                              (FalseC)
                              (DynErrorC (UndefinedC))))
                        builtin-env)))
    (hash-set! memory (lookup/boot 'any)
               (let ([iterable 'iterable]
                     [exn 'exn]
		     [iter (gen-sym 'iter)])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "any"))))
                        (list iterable)
			(LetC iter
			      (call-args/c (IdC 'iter)
					   (list (IdC iterable)))
			      (SeqC 
			       (WhileC (TrueC)
				       (TryCatchC
					(IfC (call-nullary/c (get-attr/c (IdC iter)
									 '__next__))
					     (ReturnC (TrueC))
					     (NoneC))
					exn
					(IfC (IsInstanceC (IdC exn)
							  (IdC 'StopIteration))
					     (BreakC)
					     (DynErrorC (UndefinedC))))
				       (NoneC))
			       (ReturnC (FalseC))))
                        builtin-env)))
    (hash-set! memory (lookup/boot 'all)
               (let ([iterable 'iterable]
                     [exn 'exn]
		     [iter (gen-sym 'iter)])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "all"))))
                        (list iterable)
			(LetC iter
			      (call-args/c (IdC 'iter)
					   (list (IdC iterable)))
			      (SeqC 
			       (WhileC (TrueC)
				       (TryCatchC
					(IfC (call-nullary/c (get-attr/c (IdC iter)
									 '__next__))
					     (NoneC)
					     (ReturnC (FalseC)))
					exn
					(IfC (IsInstanceC (IdC exn)
							  (IdC 'StopIteration))
					     (BreakC)
					     (DynErrorC (UndefinedC))))
				       (NoneC))
			       (ReturnC (TrueC))))
                        builtin-env)))
    (hash-set! memory (lookup/boot 'next)
               (let ([args 'args]
                     [exn 'exn])
                 (ClosV (lookup/boot 'function)
                        (box (list (entV (obj-str "__name__")
                                         (obj-str "next"))
                                   (entV (obj-str "__has-vararg__")
                                         (get-singleton-object 'True))))

                        (list args)
                        (TryCatchC
                         (call-nullary/c (get-attr/c (get-item/c (IdC args) 0)
                                                     '__next__))
                         exn
                         (IfC (Prim2C 'and
                                      (IsInstanceC (IdC exn) (IdC 'StopIteration))
                                      (eq?/c (LenC (IdC args)) (NumC 'int 2)))
                              (get-item/c (IdC args) 1)
                              (DynErrorC (UndefinedC))))
                        builtin-env)))))

(define tmp-undef
  (UndefinedV))

(define (singleton_new_lambda cls)
  (let ([self 'self]
        [args 'args])
    (ClosV (lookup/boot 'function)
           (box (list (entV (obj-str "__name__")
                            (obj-str "__new__"))
                      (entV (obj-str "__has-vararg__")
                            (get-singleton-object 'True))))
           (list self args)
           (ErrorC 'TypeError (StrC "class can't create objects"))
           (cl-env cls))))

(define (attribute_iter_lambda cls)
  (let ([self 'self]
        [idx 'idx])
    (ClosV (lookup/boot 'function)
           (box (list (entV (obj-str "__name__")
                            (obj-str "__iter__"))))

           (list self)
           (call-args/c
            (IdC 'iter)
            (list
             (ObjC (IdC 'object)
                   (DictC
                    (list
                     (entC (StrC "__getitem__")
                           (LamC (list idx)
                                 (GetItemAtIdxC (IdC self)
                                                (IdC idx))))))
                   empty)))
           (cl-env cls))))

(define (iterable_new_lambda cls typeC do-copy?)
  (let ([type 'stype]
        [args 'args]
        [iter 'iter]
        [add-one 'add-one]
        [add-one/impl 'add-one/impl]
        [till-now 'till-now]
        [exn-sym 'exn-sym])
    (ClosV (lookup/boot 'function)
           (box (list (entV (obj-str "__name__")
                            (obj-str "__new__"))
                      (entV (obj-str "__has-vararg__"
                                     )
                            (get-singleton-object 'True))))
           (list type args)
           (IfC (empty?/c (IdC args))
                (typeC empty)
                (IfC (IsInstanceC (get-item/c (IdC args) 0) (IdC cls))
                     (if do-copy?
                         (CopyObjC (get-item/c (IdC args) 0))
                         (get-item/c (IdC args) 0))
                     (lst-let/c
                      (list
                       (bindc iter (NoneC))
                       (bindc add-one
                              (LamC (list till-now) (ErrorC 'Exception (StrC "undefined add-one"))))
                       (bindc add-one/impl
                              (LamC (list till-now)
                                    (TryCatchC
                                     (AppC (IdC add-one)
                                           (list (add/c (IdC till-now)
                                                        (typeC (list (AppC (get-attr/c (IdC iter) '__next__) empty))))))
                                     exn-sym
                                     (IfC (IsInstanceC (IdC exn-sym) (IdC 'StopIteration))
                                          (IdC till-now)
                                          (DynErrorC (UndefinedC)))))))
                      (seq/c
                       (list
                        (Set!C add-one (IdC add-one/impl))
                        (TryCatchC
                         (Set!C iter
                                (call-nullary/c (get-attr/c (get-item/c (IdC args) 0) '__iter__)))
                         exn-sym
                         (IfC (IsInstanceC (IdC exn-sym) (IdC 'AttributeError))
                              (ErrorC 'TypeError (StrC "did not get an iterable type"))
                              (DynErrorC (UndefinedC))))
                        (AppC (IdC add-one) (list (typeC empty))))))))
           (cl-env cls))))
