;;; Pass 35: generate-MSIL
;;; September 2001
;===============================================================================

;; Note: this depends on "emission.ss"

(define generate-MSIL
  (let ()
    (import emission-lib)
    ;=======================================
    (define breakdown-old
      (lambda (name sym)
        (let loop ([symls (string->list (symbol->string name))]
                   [varls '()]
                   [buf '()])
          (cond
            [(null? symls)
             (reverse (cons (string->symbol (list->string (reverse buf)))
                            varls))]
            [(eqv? (car symls) sym)
             (loop (cdr symls)
                   (cons (string->symbol (list->string (reverse buf)))
                         varls)
                   '())]
            [else
              (loop (cdr symls)
                    varls
                    (cons (car symls) buf))]))))
    (define breakdown
      (lambda (name sym)
        (let ([namels (breakdown-old name sym)])
          (let ([path (rdc namels)]
                [meth (rac namels)])
            (values
              (let loop ([foo path])
                (cond
                  [(null? (cdr foo)) (symbol->string (car foo))]
                  [else (string-append
                          (symbol->string (car foo)) "." (loop (cdr foo)))]))
              meth)))))
    (define emit-args
      (lambda (args)
        (for-each Command (reverse args))))
    ;=======================================
    (define Command
      (lambda (expr)
        (match expr
          [(nop) (emit "nop")]
          [(return) (emit "ret")]
          [(branch ,label)
           (emit "br ~a" label)]
          [(branch-point ,label)
           (unindent!)
           (emit (format "    ~a:" label))
           (indent!)]
          [(ifnot (branch ,label))
           (emit "brfalse ~a" label)]
          [(store! ,var)
           (emit "stloc ~a" var)]
          ;; Grr, it's so awkward to flip the top two things on the stack:
          ;; This will NOT be thread or engine safe.  This should be in a
          ;; critical section, it depends on using the SchemeObjectLevel
          ;; field "flipper" as a temp variable to flip the top two
          ;; stack objects.
          [(toplvl-store! ,var)
           (emit "stsfld object ~a::flipper" MSIL-emission-namespace)
           (emit "ldsfld class [SchemeRuntime]ScmSymbol ~a/~a::~a"
                 MSIL-emission-namespace symref-subclass var)
           (emit "ldsfld object ~a::flipper" MSIL-emission-namespace)
           (emit "call instance void ~a"
                 "class [SchemeRuntime]ScmSymbol::Set( object )")]
          
          [(primcall ,prim ,numrands ,tag* ...)
           (guard (extended-scheme-primitive? prim))
           (emit-primitive prim tag* numrands)
           ;(emit-primitive '+ '() '())
           ]
          ;---------------------------------------
          [(code ,exp* ...)
           (for-each Command exp*)]
          [(let-class (,class-defn* ...) ,body)
           (for-each emit-class-defn class-defn*)
           (Command body)]
          [(new ,class-name ,arg* ...)
           (emit-args arg*)
           (emit "newobj instance void ~a/~a::.ctor(~a)"
                 MSIL-emission-namespace
                 class-name
                 (cond
                   [(zero? (length arg*)) ""]
                   [(= 1 (length arg*)) "object"]
                   [else (string-append
                           (map (lambda (x) "object,")
                                (iota (sub1 (length arg*))))
                           "object")]))]
          [(open-instance ,obj ,class-name ,[body])
           (void)]
          [(object-reference ,obj ,class ,field)
           (emit "ldloc ~a" obj)
           (emit "ldfld object ~s::~s" class field)]
          [(invoke-method ,obj ,class ,method (,arg* ...))
           (emit-args arg*)
           (emit "ldloc ~a" obj)
           (emit "castclass ~a" class)
           (emit "call instance object ~s::~s(~s)"
                 class
                 method
                 (string->symbol
                   (cond
                     [(zero? (length arg*)) ""]
                     [(= 1 (length arg*)) "object"]
                     [else (string-append
                             (map (lambda (x) "object,")
                                  (iota (sub1 (length arg*))))
                             "object")])))]
          [(foreign-call ,name ,type-sig (,arg* ...))
           (emit-args arg*)
           (mvlet ([(path meth) (breakdown name #\.)])
             (emit "call void [mscorlib]~s::~s(~s)"
                   (string->symbol path)
                   meth
                   (string->symbol
                     (cond
                       [(zero? (length arg*)) ""]
                       [(= 1 (length arg*)) "object"]
                       [else (string-append
                               (map (lambda (x) "object,")
                                    (iota (sub1 (length arg*))))
                               "object")]))))
           ;;; hack:
           (when (equal? type-sig '(quote ()))
             (emit-primitive 'void))]
          [(static-ref ,class ,field)
           (emit "ldsfld object ~s::~s"
                 class field)]
          [(this-ref ,class ,field)
           (emit "ldfld object ~s::~s" class field)]
          [(invoke-static-method ,class ,method-name ,type-sig (,arg* ...))
           (emit-args arg*)
           (emit "call static object ~s::~s(~s)"
                 class
                 method-name
                 (string->symbol
                   (cond
                     [(zero? (length arg*)) ""]
                     [(= 1 (length arg*)) "object"]
                     [else (string-append
                             (map (lambda (x) "object,")
                                  (iota (sub1 (length arg*))))
                             "object")])))]
          ;---------------------------------------
          [(call overflowed)
           (emit
             (string-append
               "callvirt instance object [SchemeRuntime]"
               "ScmClosure::applyn( ~a )")
             (apply string-append
                    (insert-between
                      ", "
                      (make-list SETTINGS:ARGS_HARDCODED "object"))))]
          [(tailcall overflowed)
           (emit
             (string-append
               "tail. callvirt instance object [SchemeRuntime]"
               "ScmClosure::applyn( ~a )")
             (apply string-append
                    (insert-between
                      ", "
                      (make-list SETTINGS:ARGS_HARDCODED "object"))))]
          ;---------------------------------------
          [(call ,num)
           (emit
             (string-append
               "callvirt instance object [SchemeRuntime]"
               "ScmClosure::apply~a( ~a )")
             num (apply string-append
                        (insert-between ", " (make-list num "object"))))]
          [(tailcall ,num)
           (emit
             (string-append
               "tail. callvirt instance object [SchemeRuntime]"
               "ScmClosure::apply~a( ~a )")
             num (apply string-append
                        (insert-between ", " (make-list num "object"))))]
          ;---------------------------------------
          [(call ',class ,num)
           (emit
             "call instance object ~a/~a::direct~a( ~a )"
             MSIL-emission-namespace
             class num
             (apply string-append
                    (insert-between ", " (make-list num "object"))))]
          [(tailcall ',class ,num)
           (emit (string-append
                   "tail. call instance object ~a/~a::direct~a( ~a )")
                 MSIL-emission-namespace
                 class num
                 (apply string-append
                        (insert-between ", " (make-list num "object"))))]
          ;---------------------------------------
          [(ldc ,imm) (emit-ldc imm)]
          [(ldstr ,str)
           ;; Must be careful to not have the format inside emit
           ;; muss up our string, emit doesn't use format unless
           ;; it's given multiple arguments.
           (emit (format "ldstr \"~a\"" str))
           (emit "newobj instance void [mscorlib]~a"
                 "System.Text.StringBuilder::.ctor(string)")]
          [(ldarg ,v) (emit "ldarg ~a" v)]
          [(ldloc ,v) (emit "ldloc ~a" v)]
          [(ldfld (quote ,class) ,v)
           (emit "ldarg.0")
           (emit "ldfld object ~a/~a::~a" MSIL-emission-namespace class v)]
          [(toplvl-ldfld ,v)
           (emit "ldsfld class [SchemeRuntime]ScmSymbol ~a/~a::~a"
                 MSIL-emission-namespace symref-subclass v)
           (emit "call instance object ~a"
                 "class [SchemeRuntime]ScmSymbol::Ref( )")]
          [(toplvl-ldfld-quoted ,v)
           (emit "ldsfld class [SchemeRuntime]ScmSymbol ~a/~a::~a"
                 MSIL-emission-namespace symref-subclass v)]
          [(pop) (emit "pop")]
          [(box ,type)
           (case type
             [(Int32) (emit "box int32")]
             [(Float64) (emit "box float64")]
             [(Char) (emit "box char")]
             [else (error 'generate-MSIL
                          "trying to box this type: ~s" type)])]
          [(unbox ,type)
           (case type
             [(Int32) (emit "unbox int32") (emit "ldind.i4")]
             [(Float64) (emit "unbox float64") (emit "ldind.r8")]
             [(Char) (emit "unbox char") (emit "ldind.u2")]
             [else (error 'generate-MSIL
                          "trying to unbox this type: ~s" type)])]
          ;This is added only by internal means:
          #;[(print)
             (emit "call string [SchemeRuntime]Runtime::scmToString( object )")
             (emit "call void [mscorlib]System.Console::WriteLine( string )")]
          [,expr (error 'generate-MSILw
                        "invalid Command expression: ~s"
                        expr)])))
    ;=======================================
    ;; Variable arity procedures must have _all_ applyX methods overridden.
    ;; This emits ONE applyX method where X is a concrete number.
    (define VariableArity_ApplyFixedMethod
      (lambda (n name formal* spillarg)
        (let ([numformals (length formal*)])
          ;; For this method to have been called, n >= (length formal*)
          (emit ".method public virtual object apply~a (~a) {"
                n
                (apply string-append
                       (insert-between
                         ", "
                         (map (lambda (formal) (format "object arg~a" formal))
                              (cdr (iota (add1 n)))))))
          (indent!)
          (emit ".maxstack ~a " (+ 2 n)) ;; Need a space for the null list.
          ;; Load all actual parameters directly on the stack:
          ;(for-each emit-ldarg (iota (+ 1 numformals)))
          (for-each emit-ldarg (iota (+ 1 n)))
          ;; The latter portion of these must be put in a list.
          (emit-ldc '())
          (rep (- n numformals) (emit-primitive 'cons))
          ;; Now call the direct method:
          (emit "tail. call instance object ~a/~a::direct~a( ~a )"
                MSIL-emission-namespace
                name (add1 numformals)
                (apply string-append
                       (insert-between
                         ", "
                         (make-list (add1 numformals) "object"))))
          (emit "ret")
          (unindent!) (emit "}"))))
    ;=======================================
    ;; Variable arity procedures must have _all_ ApplyX methods overridden.
    ;; This emits the ApplyN method, which has to deal with the problem of
    ;; fitting one variable arg pattern to another, for example feeding
    ;; arguments of the format (a b . c) into (v w x y . z).  This amounts
    ;; to packing or unpacking the spill parameter.
    (define VariableArity_ApplyNMethod
      (lambda (name formal* spillarg)
        (let ([numformals (length formal*)])
          (emit ".method public virtual object applyn (~a) {"
                (apply
                  string-append
                  (insert-between
                    ", "
                    (snoc "object argls"
                          (map (lambda (n)
                                 (format "object arg~a" n))
                               (cdr (iota SETTINGS:ARGS_HARDCODED)))))))
          (indent!)
          ;(emit ".maxstack ~a " (+ 2 SETTINGS:ARGS_HARDCODED))
          (emit ".maxstack 128 ")
          
          ;; Load all actual parameters directly on the stack:
          (for-each emit-ldarg (iota (+ 1 SETTINGS:ARGS_HARDCODED)))
          
          ;; Now either more arguments must be unpacked from the spill
          ;; parameter, or more arguments must be packed into it, depending
          ;; on the relative size of numformals vs. SETTINGS:ARGS_HARDCODED.
          
          ;; If numformals is larger or equal, the incoming spill arg must
          ;; be unpacked:
          ;; NOTE: if not enough arguments are provided to the procedure,
          ;; one of the cdr's here will throw an exception. For good error
          ;; messages, code should be added to catch this:
          (when (>= numformals SETTINGS:ARGS_HARDCODED)
            ;; Load the argls:
            (emit-ldarg SETTINGS:ARGS_HARDCODED)
            ;; We know there's at least one:
            (emit-primitive 'car)
            ;; Then unpack all but the last:
            (rep (- numformals SETTINGS:ARGS_HARDCODED)
                 (emit-ldarg SETTINGS:ARGS_HARDCODED)
                 (emit-primitive 'cdr)
                 (emit "dup")
                 (emit-starg SETTINGS:ARGS_HARDCODED)
                 (emit-primitive 'car))
            ;; With the car's unpacked, leave the rest
            (emit-ldarg SETTINGS:ARGS_HARDCODED)
            (emit-primitive 'cdr))
          
          ;; If numformals is one smaller, things match up.
          
          ;; Otherwise, if numformals is more than one smaller, then
          ;; more args must be packed into the incoming spillarg:
          (rep (- SETTINGS:ARGS_HARDCODED numformals 1)
               (emit-primitive 'cons))
          
          ;; Now call the direct method:
          (emit "tail. call instance object ~a/~a::direct~a( ~a )"
                MSIL-emission-namespace
                name (add1 numformals)
                (apply string-append
                       (insert-between
                         ", "
                         (make-list (add1 numformals) "object"))))
          (emit "ret")
          (unindent!) (emit "}"))))
    ;=======================================
    (define VariableArity_ApplyMethods
      (lambda (name formal* spillarg)
        (let ([numformals (length formal*)])
          (do ([i numformals (+ 1 i)])
              ([> i SETTINGS:ARGS_HARDCODED])
              (VariableArity_ApplyFixedMethod i name formal* spillarg))
          (VariableArity_ApplyNMethod name formal* spillarg)
          )))
    ;=======================================
    ;; This generates the single apply method for a fixed arity procedure,
    ;; whether it's parameter count excedes SETTINGS:ARGS_HARDCODED or not.
    (define ApplyMethod
      (lambda (name numargs formal*)
        (emit ".method public virtual object apply~a (~a) {"
              (if (> numargs SETTINGS:ARGS_HARDCODED)
                  "n" numargs)
              (apply
                string-append
                (insert-between
                  ", "
                  (if (> numargs SETTINGS:ARGS_HARDCODED)
                      (snoc "object argls"
                            (map (lambda (n )
                                   (format "object arg~a" (add1 n)))
                                 (cdr (iota SETTINGS:ARGS_HARDCODED))))
                      (map (lambda (formal) (format "object ~a" formal))
                           formal*)))))
        (indent!)
        ;; Extra stack spaces are needed
        ;; for the closure itself
        ;; and sometimes for space to do vector references:
        (if (>= numargs SETTINGS:ARGS_HARDCODED)
            (emit ".maxstack ~a " (+ 2 numargs))
            (emit ".maxstack ~a " (+ 1 numargs)))
        
        ;;; Now to get those actual parameters on
        ;;; the stack to make the direct call:
        (if (> numargs SETTINGS:ARGS_HARDCODED)
            (begin
              (for-each emit-ldarg (iota SETTINGS:ARGS_HARDCODED))
              ;; Load the argls:
              (emit-ldarg SETTINGS:ARGS_HARDCODED)
              ;; We know there's at least one:
              (emit-primitive 'car)
              ;; Then unpack all but the last:
              (rep (- numargs SETTINGS:ARGS_HARDCODED 1)
                   (emit-ldarg SETTINGS:ARGS_HARDCODED)
                   (emit-primitive 'cdr)
                   (emit "dup")
                   (emit-starg SETTINGS:ARGS_HARDCODED)
                   (emit-primitive 'car))
              ;; Last time through don't need to STARG:
              (when (> numargs SETTINGS:ARGS_HARDCODED)
                (emit-ldarg SETTINGS:ARGS_HARDCODED)
                (emit-primitive 'cdr)
                (emit-primitive 'car)))
            (for-each emit-ldarg (iota (add1 numargs))))
        
        (emit "tail. call instance object ~a/~a::direct~a( ~a )"
              MSIL-emission-namespace
              name numargs
              (apply string-append
                     (insert-between
                       ", "
                       (make-list numargs "object"))))
        (emit "ret")
        (unindent!) (emit "}")))
    
    ;=======================================
    (define LambdaClass
      (lambda (name expr)
        (load-list-emission)
        (match expr
          [(class-def (,free* ...)
                      (lambda ,formalexp
                        (maxstack ,maxstack
                                  (local ,local*
                                         (code ,cmd* ...)))))
           (let* ([formal* (get-formals formalexp)]
                  [numargs (length formal*)])
             (clear-buffer!)
             (emit (string-append
                     ".class nested public ~a extends "
                     " [SchemeRuntime]ScmClosure {")
                   name)
             (indent!)
             ;--emit fields--------
             (for-each (lambda (fv)
                         (emit ".field public object ~a" fv))
                       free*)
             ;--emit constructor---
             (emit ".method public instance void .ctor() {")
             (indent!)
             (emit ".maxstack 1")
             (emit "ldarg.0")
             (emit "call instance void [mscorlib]System.Object::.ctor()")
             (emit "ret")
             (unindent!) (emit "}")
             
             ;--emit apply method-------------------
             (match formalexp
               [,v (guard (symbol? v))
                 (VariableArity_ApplyMethods name '() v)]
               [(,v* ...) (ApplyMethod name numargs v*)]
               [(,v* ... . ,extra)
                (VariableArity_ApplyMethods name v* extra)]
               )
             
             ;--emit direct method------------------
             (emit ;".method public object apply (~a) {"
               (string-append
                 ".method public object direct~a (~a) {")
               numargs
               (apply string-append
                      (insert-between
                        ", "
                        (map (lambda (formal) (format "object ~a" formal))
                             formal*))))
             (indent!)
             (emit ".maxstack ~a" maxstack)
             (emit-locals local*)
             (for-each Command cmd*)
             (unindent!) (emit "}")
             
             
             ;---------------------------------------
             (unindent!) (emit "}")
             (let ([ret (get-buffer)])
               (load-string-emission)
               ret))])))
    
    ;=======================================
    (define Letrec
      (lambda (expr sym*)
        (match expr
          [(letrec ([,lhs* ,rhs*] ...)
             (entry-point ,main))
           (values
             (map LambdaClass lhs* rhs*)
             (let () (load-list-emission)
               ;;; First wire up the toplvl symbol references:
               (emit "// Now point symbol fields at the real symbols:")
               (for-each
                 (lambda (sym)
                   (emit "ldarg theruntime")
                   (emit "ldfld class [SchemeRuntime]Symboltable ~a"
                         "[SchemeRuntime]Runtime::symtable")
                   (emit "call instance object ~a"
                         "class [SchemeRuntime]Symboltable::Ref( string )")
                   (emit "stsfld object ~a/~a::~a"
                         MSIL-emission-namespace symref-subclass sym))
                 sym*)
               (emit "")
               (clear-buffer!) ;; check the global parameter:
               (when (timing_info)
                 (emit "call int32 [mscorlib]System.Environment~a"
                       "::get_TickCount()")
                 (emit "stloc time"))
               (emit "newobj instance void ~a/~a::.ctor()"
                     MSIL-emission-namespace main)
               (emit "call instance object ~a/~a::direct0( )"
                     MSIL-emission-namespace main)
               (when (timing_info)
                 (emit "ldstr \"; Running-time: {2}     Ran-from: {0}  {1} \"")
                 (emit "call string [mscorlib]System.Environment~a"
                       "::get_MachineName()")
                 (emit "call valuetype [mscorlib]System.DateTime ~a"
                       "[mscorlib]System.DateTime::get_Now()")
                 (emit "box [mscorlib]System.DateTime")
                 (emit "call int32 [mscorlib]System.Environment~a"
                       "::get_TickCount()")
                 (emit "ldloc time")
                 (emit "sub")
                 (emit "box int32")
                 (emit "call void [mscorlib]System.Console::~a"
                       "WriteLine( string, object, object, object )"))
               (let ([ret (get-buffer)])
                 (load-list-emission)
                 ret)))])))
    
    ;=======================================
    (lambda (prog)
      (match prog
        [(,input-language
           (quote (program ,sym* ,pkg* ,class-defns* ... ,body)))
         (load-string-emission)  ;; This will be our default emission mode.
         ;;                         (but right now we don't use it at all).
         (mvlet ([(lambda-classes code) (Letrec body sym*)])
           `(generate-MSIL-language ',sym*
                                    ',pkg*
                                    ',class-defns*
                                    ',lambda-classes
                                    ',code))]))
    ))



























#!eof
#;(append
;; check the global parameter:
(if (timing_info)
    (list ".locals( int32 time )") '())
(list
  "newobj instance void [SchemeRuntime]Runtime::.ctor()"
  (format "stsfld class [SchemeRuntime]Runtime ~a::scmruntime"
          MSIL-emission-namespace)
  "")
(if (timing_info)
    (list
      (format "call int32 [mscorlib]System.Environment~a"
              "::get_TickCount()")
      "stloc time") '())
(list
  (format "newobj instance void ~a/~a::.ctor()"
          MSIL-emission-namespace main)
  (format "call instance object ~a/~a::direct0( )"
          MSIL-emission-namespace main))
(if (timing_info)
    (list
      "ldstr \"; Running-time: {2}     Ran-from: {0}  {1} \""
      (format "call string [mscorlib]System.Environment~a"
              "::get_MachineName()")
      (format "call valuetype [mscorlib]System.DateTime ~a"
              "[mscorlib]System.DateTime::get_Now()")
      "box [mscorlib]System.DateTime"
      (format "call int32 [mscorlib]System.Environment~a"
              "::get_TickCount()")
      "ldloc time"
      "sub"
      "box int32"
      (format "call void [mscorlib]System.Console::~a"
              "WriteLine( string, object, object, object )"))
    '()))


