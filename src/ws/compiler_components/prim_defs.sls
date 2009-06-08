#!r6rs

;;;; The type-bindings for all Regiment and TML primitives.
;;;; .author Ryan Newton

;;;;
;;;; [2004.07.28] Introducing 'Area'.  Note that a Region is also an Area.
;;;; Ok, redoing primitive listings with type information:
;;;; The types I'm using right now are:                               <br>
;;;;   Anchor, Area, Region, Stream, Event, Node, Location, Reading
;;;;   Number, Int, Float, Bool, Void
;;;;   List, Array, Tuple
;;;;
;;;; <br><br>
;;;; Then some types that are used only in the local language are:
;;;;   Token NodeID
;;;;
;;;; <br><br>
;;;; TODO TODO TODO: Need to phase out the old type defs and make this
;;;; the only one.
;;;; 
;;;; The lists of primitives here have entries of the form: <br><br>
;;;;
;;;;   [PrimName Type]                   -- For constants    <br>
;;;;   [PrimName ArgTypess ReturnType]   -- For functions   <br>


(library (ws compiler_components prim_defs) 
  (export 
           num-types ;; List of the numeric types
	   truncate-integer
	   refcount-form?

           regiment-type-aliases
	   regiment-basic-primitives
	   local-node-primitives
	   meta-only-primitives
	   higher-order-primitives
	   library-primitives
	   regiment-constants
	   regiment-distributed-primitives
	   regiment-primitives
	   wavescript-primitives
	   wavescript-effectful-primitives
	   wavescript-stream-primitives
	   stream-primitive?
	   temp-hack-stream-primitives 
	   temp-hack-stream-primitive?
	   infix-arith-prims

	   built-in-type-constructors
	   built-in-atomic-types
	   generic-arith-primitives
	   token-machine-primitives

	   regiment-keyword?
 	   token-machine-keyword?
 	   get-primitive-entry
	   get-primitive-type
	   get-primitive-return-type
 	   map-prim-w-types
 	   regiment-primitive?
 	   regiment-constant?
 	   basic-primitive?
 	   distributed-primitive?
 	   token-machine-primitive?

	   ;special-rewrite-libfuns
	   )
  (import (except (rnrs (6)) error) 
	  (except (rnrs r5rs (6)) delay force) 
	  (ws compat compat)
	  (ws globals)
	  (ws util iu-match)
	  (ws util hashtab)
	  (ws util helpers))

;;; Misc global definitions.

;; These are type aliases that are defined by default.
(define regiment-type-aliases
  '(

    [Region (Area Node)]
    [Anchor (Stream Node)]
    [NetDist   Float] ;; Network distance.  Depends on gradient implementation.
    ; [(Area 'a) (Stream (Space 'a))]
    ))

;; Added a subkind for numbers, here are the types in that subkind.
(define num-types '(Int Float Double Complex 
		    Int16 Int32 Int64
		    ;; Eventually:
		    ;; Int8 Int16 Int64 Double Complex64
		    Uint8 Uint16
		    ))

(define-syntax refcount-form? 
  (syntax-rules ()
    [(_ x) (memq x '(incr-local-refcount decr-local-refcount 
		     incr-heap-refcount decr-heap-refcount 
		     incr-queue-refcount decr-queue-refcount))]))

;; Force a number into a smaller integer size:
(define (truncate-integer ty n)
  (match ty
    ;[Int8  (- (modulo (+ n (expt 2 7))  (expt 2 8))  (expt 2 7))]
    [Int16 (- (modulo (+ n (expt 2 15)) (expt 2 16)) (expt 2 15))]
    [Int32 (- (modulo (+ n (expt 2 31)) (expt 2 32)) (expt 2 31))]
    [Int64 (- (modulo (+ n (expt 2 63)) (expt 2 64)) (expt 2 63))]
    [Uint8  (modulo n (expt 2 8))]
    [Uint16 (modulo n (expt 2 16))]
    [Int (error 'truncate-integer "can't truncate to type Int -- platform dependent!")]
    ))

;=============================================================

;;; Type signatures for primitives that are singled out in some way.

;; Anything not in this list is either a user-defined type constructor, or invalid.
(define built-in-type-constructors 
  '(List Array Sigseg Stream HashTable
	 Ref
	 Record Row
	 Pointer
	 ))

(define built-in-atomic-types
  (append '(Bool Char String)
	  num-types))

;; Hierarchy:
;; Int8
;; Int16
;; Int    Float
;; Int64  Double  Complex
;;                Complex64
(define generic-arith-primitives
  '(
    (gint (Int) (NUM a))

    ;; Generalized zero.
    ;;(gzero ()   'a)  ;; Not yet.


    (g+ ((NUM a) (NUM a)) (NUM a))
    (g- ((NUM a) (NUM a)) (NUM a)) 
    (g* ((NUM a) (NUM a)) (NUM a)) 
    (g/ ((NUM a) (NUM a)) (NUM a)) 
    (g^ ((NUM a) (NUM a)) (NUM a)) ;; exponentiation

    ;; TODO: Add more generic operations.  UNFINISHED:

    ;; Considering this instead of all the separate conversions.
    (cast_num ((NUM a)) (NUM b))

    ;; Bitwise operations:
    ;(lshift ((NUM a) Int) (NUM a))
    ;(rshift ((NUM a) Int) (NUM a))
    ;(logor  ((NUM a) (NUM a)) (NUM a))
    ;(logand ((NUM a) (NUM a)) (NUM a))
    ;(logxor ((NUM a) (NUM a)) (NUM a))
    
    ; Here are some "upcasts".
    ; Throws an error if you try to downcast??
    ; (The error behavior is not decided yet.)
    ;(toInt     ((NUM a)) Int)
    ;(toFloat   ((NUM a)) Float)
    ;(toComplex ((NUM a)) Complex)

    ;;  Int -> Int
    ;;  Float -> Float
    ;;  Complex -> Float
    ;(abs ((NUM a)) (NUM a)) ;; This subsumes cnorm.

    ;(sqrt ((NUM a)) (NUM a)) 

))


;; [2007.10.20]
;; Beginning to lay down the infrastructure for doing rewrite optimizitions.
#;
(define special-rewrite-primitives
  '([fft]
    [ifft]))
;; These don't need types because they're defined in WS types.  The
;; inferencer infers types normally.
#;
(define special-rewrite-libfuns
  '(window dewindow rewindow 
	   
    ))


;; These are (or will be) allowed in both the meta and the object
;; language.  They're higher order, but in the object language the
;; lambda expression parameterizing each of these primitives must be
;; *known* so that first-order code may be generated.
(define higher-order-primitives
  '(
    (map (('a -> 'b) (List 'a)) (List 'b))
    (fold (('acc 'b -> 'acc) 'acc (List 'b)) 'acc)

    ;; These should be defined IN the language, but they're not right now:
    ;; [2007.07.13] We have while loops and second class refs... should put these in.
;    (List:map (('a -> 'b) (List 'a)) (List 'b))
    (List:zip ((List 'a) (List 'b)) (List #('a 'b)))
;    (List:fold (('acc 'b -> 'acc) 'acc (List 'b)) 'acc)
    ;;(List:filter (('a -> Bool) (List 'a)) (List 'a))
    ;(List:map2 (('a, 'b -> 'c) (List 'a) (List 'b)) (List 'c))

    ;(List:build        (Int (Int -> 'a)) (List 'a))

    ;; A lot of these can be defined in the language once we figure
    ;; out a story for "library" (non-inlined) procedures and
    ;; second-class references.

    ;; [2007.07.13] Actually... second class references are allowed
    ;; now... I don't see why we'd need non-inlined procedures.
    (Array:map         (('a -> 'b) (Array 'a))              (Array 'b))
    (Array:fold        (('acc 'b -> 'acc) 'acc (Array 'b))  'acc)
    (Array:andmap      (('a -> Bool) (Array 'a))            Bool)
    ;; This uses an initialization function to fill in an array:
    ;; It's zero-based!
    (Array:build       (Int (Int -> 'a)) (Array 'a))

    (HashTable:foreach (('key 'val -> #()) (HashTable 'key 'val)) #())
    
    ))


;; These are primitives that are defined in a library.  Currently they
;; actually aren't considered primitive by "regiment-primitive?", but
;; the names are recognized as special.
;;
;; There are many other possible conventions here... if we wanted we
;; could extract the special names from internal.ws and avoid the
;; extra overhead of a table-entry here...
;; 
;; The primitives in this table, together with those names used in
;; rewrite-optimizations (special-rewrite-libfuns) make up all the
;; names that are treated as special.  In the future, we may require
;; that special-rewrite-libfuns symbols are a subset of those in this
;; table.
;;
;; [2008.08.20] Upgrading to a parameter:
(define library-primitives
 (reg:make-parameter 
  '(
    ;; Takes (dst, dstpos, src, srcpos, len), like memcpy
    [Array:blit   ((Array 't) Int (Array 't) Int Int) (Array 't)]
    [List:make    (Int 't) (List 't)]
    [List:length ((List 'a)) Int]
    [List:ref     ((List 't) Int) 't]
    [List:append  ((List 'a) (List 'a)) (List 'a)]
    [List:reverse ((List 'a)) (List 'a)]
    (List:map (('a -> 'b) (List 'a)) (List 'b))
    (List:fold (('acc 'b -> 'acc) 'acc (List 'b)) 'acc)
    (List:build        (Int (Int -> 'a)) (List 'a))

    ;; 

    ;[wsexit (Int) a]
    )))


;; Only for the meta-language, shouldn't exist after static-elaborate.
;; TODO: FIXME: Don't think I enforce this right now!!
(define meta-only-primitives
  `(
    (GETENV (String) String) ; Returns "" if the env var is unbound.
    (FILE_EXISTS (String) Bool) ; Returns "" if the env var is unbound.
    ;(FILECONTENTS (String) String)

    (SHELL (String) String)  ; Returns a string containing the output of the subprocess.

    ;; [2007.11.07] These are hacks to manually parallelize programs.
    (SETCPU           (Int (Stream 't)) (Stream 't))
    (SETCPUDEEP       (Int (Stream 't)) (Stream 't))
    ))


;; TODO: NOT IMPLEMENTED YET: [2006.09.01]
;; 
;; This list of primitives determines which library routines are
;; implemented in "native code" (C++).  Any primitives in this list
;; will NOT have their wavescript library definitions inlined, rather
;; they will link against their native versions.
;;   IF this is enabled, we will lose the simplicity of the query
;; graph (not just iterates/unions).
(IFWAVESCOPE 
 (define wavescript-native-primitives
   '(
     )))
  ;sync4?

(IFWAVESCOPE
 ;; All side-effecting primitives MUST go here and must return UNIT:
 (define wavescript-effectful-primitives 
   '(
     (Array:set         ((Array 'a) Int 'a)          #())

     ;; [2006.11.28] Giving these void types.
     (HashTable:set_BANG ((HashTable 'key 'val) 'key 'val) #())
     (HashTable:rem_BANG ((HashTable 'key 'val) 'key)      #())
     (print            ('a) #())
     ;(__print            ((Array Char)) #())

     ;; I just use a virtual "Queue" to make the type-checking work for emits:
     (emit           ((VQueue 'a) 'a)                #())
     ;; This is an internal version that emits to a single downstream op.
     (_emit_to     (Symbol (List Symbol) (VQueue 'a) 'a)   #())

     ;; This isn't a primitive, but it's nice to pretend it is so not all passes have to treat it.
     (break            ()                      'a)

     ;; Signals an error, has any return type:
     (wserror         (String)                  'a)
     (__wserror_ARRAY ((Array Char))            'a) ;; Internal

     (inspect         ('a)                      'a)

     ;;(string-append!    (String String) String) ;; Internal -- destructive version.

     ;; This writes a scalar value into a byte array using a little endian convention.
     ;; It can't accept just a NUM type because it must also handle Chars and Bools.
     (__type_unsafe_write ('a (Array Uint8) Int) #())

     )))

;=============================================================

;;; General Regiment/WaveScript primitives.

;; These count as primitives also.
;; All regiment constants are presumed to be "slow prims" for
;; now. (see add-heartbeats)
(define regiment-constants
  '(
    (world          (Area Node))
    (anchor         (Stream Node))

;    (pi             Float)

    (IS_SIM         Bool)

    ;; Adding Wavescope-related primitives:
    (nullseg        (Sigseg 'a))
    (Array:null     (Array 'a))

    (nulltimebase  Timebase)

    ;; Adding this for uniformity of normal forms in later passes:
    ;(BOTTOM        'a)
    
    ))

(define infix-arith-prims
  '(

    (_+_ (Int Int) Int)
    (_-_ (Int Int) Int) 
    (*_ (Int Int) Int) 
    (/_ (Int Int) Int) 
    (^_ (Int Int) Int) ;; exponentiation
    ;(lshift_ (Int Int) Int) ;; bitwise shift
    ;(rshift_ (Int Int) Int) 

    (_+I16 (Int16 Int16) Int16)
    (_-I16 (Int16 Int16) Int16) 
    (*I16 (Int16 Int16) Int16) 
    (/I16 (Int16 Int16) Int16) 
    (^I16 (Int16 Int16) Int16) 
    ;(lshiftI16 (Int16 Int) Int16)
    ;(rshiftI16 (Int16 Int) Int16) 

    (_+I32 (Int32 Int32) Int32)
    (_-I32 (Int32 Int32) Int32) 
    (*I32 (Int32 Int32) Int32) 
    (/I32 (Int32 Int32) Int32) 
    (^I32 (Int32 Int32) Int32) 
    ;(lshiftI32 (Int32 Int) Int32)
    ;(rshiftI32 (Int32 Int) Int32)

    (_+I64 (Int64 Int64) Int64)
    (_-I64 (Int64 Int64) Int64) 
    (*I64 (Int64 Int64) Int64) 
    (/I64 (Int64 Int64) Int64) 
    (^I64 (Int64 Int64) Int64) 
    ;(lshiftI64 (Int64 Int) Int64)
    ;(rshiftI64 (Int64 Int) Int64)

    (_+U16 (Uint16 Uint16) Uint16)
    (_-U16 (Uint16 Uint16) Uint16) 
    (*U16 (Uint16 Uint16) Uint16) 
    (/U16 (Uint16 Uint16) Uint16) 
    (^U16 (Uint16 Uint16) Uint16) 
    ;(lshiftU16 (Uint16 Int) Uint16)
    ;(rshiftU16 (Uint16 Int) Uint16)

    (_+U8 (Uint8 Uint8) Uint8)
    (_-U8 (Uint8 Uint8) Uint8) 
    (*U8 (Uint8 Uint8) Uint8) 
    (/U8 (Uint8 Uint8) Uint8) 
    (^U8 (Uint8 Uint8) Uint8) 
    ;(lshiftU8 (Uint8 Int) Uint8)
    ;(rshiftU8 (Uint8 Int) Uint8)



    ;; Not currently supporting bitwise ops for non-integers:

    (_+. (Float Float) Float)
    (_-. (Float Float) Float)
    (*. (Float Float) Float)
    (/. (Float Float) Float)
    (^. (Float Float) Float) 

    (_+D (Double Double) Double)
    (_-D (Double Double) Double)
    (*D (Double Double) Double)
    (/D (Double Double) Double)
    (^D (Double Double) Double) 

    (_+: (Complex Complex) Complex)
    (_-: (Complex Complex) Complex) 
    (*: (Complex Complex) Complex) 
    (/: (Complex Complex) Complex)
    (^: (Complex Complex) Complex)


    ; predicates
    ;; Restricted version for numbers:
    ;(=  ((NUM a) (NUM a)) Bool)
    (=  ('a 'a) Bool)     ;; <- PHASE OUT
    (<  ('a 'a) Bool)
    (>  ('a 'a) Bool)
    (<=  ('a 'a) Bool)
    (>=  ('a 'a) Bool)

))

#;
(define numeric-conversion-prims
  (apply append 
	      (map (lambda (ty1)
		     (filter id 
		       (map (lambda (ty2)
			      (and (not (eq? ty1 ty2))
				   `(,(string->symbol 
				       (** (lowercase (symbol->string ty1))
					   "To" (symbol->string ty2)))
				     (,ty1) ,ty2)))
			 num-types))) num-types)))

;; These are the basic (non-distributed) primitives supported by the Regiment language.
(define regiment-basic-primitives 
    ; value primitives
  `(
    
    ;; These three are legacy Regiment names, don't use in WaveScript!:
    (cons ('a (List 'a)) (List 'a))
    (car ((List 'a)) 'a)
    (cdr ((List 'a)) (List 'a))
    
    ;; Some list primitives are exposed in the "Prelude".  
    ;; These are just aliases for their more verbosely named counterparts:
    (head   ((List 'a)) 'a)      ;; Should remove car/cdr at some point.
    (tail   ((List 'a)) (List 'a))
    (append ((List 'a) (List 'a)) (List 'a))

    ;; The List namespace:
    (List:head ((List 'a)) 'a)
    (List:tail ((List 'a)) (List 'a))

;    (List:make   (Int 'a) (List 'a))
;    (List:length ((List 'a)) Int)
;    (List:reverse ((List 'a)) (List 'a))
;    (List:ref  ((List 'a) Int) 'a)
;    (List:append ((List 'a) (List 'a)) (List 'a))    

    ;(reverse_BANG ((List 'a)) (List 'a))

    (List:is_null  ((List 'a)) Bool)
  
    ;; [2008.08.20] This is ambiguous.  It's primitive in wsmlton, and library in wsc2:
    (List:toArray ((List 'a)) (Array 'a))
    
    ;; Should be maybe type!  For now returns list with match at head. Null otherwise.
    (List:assoc        ((List #('a 'b)) 'a) (List #('a 'b)))
    (List:assoc_update ((List #('a 'b)) 'a 'b) (List #('a 'b)))

;    (list ('a ...) (List 'a))
;    (cons (Object List) List) 
;    (cdr (List) List)
;    (car (List) Object)
;; [2005.10.20] Allowing improper lists for the moment ^^^

    ,@generic-arith-primitives
    
    (makeComplex (Float Float) Complex)

    ;; We should follow SML's system of "toLarge" and "fromLarge"
    ;; Alternatively, We should at least programmatically generate these specs:
    
    ;,@numeric-conversion-prims

    ;; This is a hack, it's not really a primitive, and the type checker treats it specially:
    (__cast_num (Symbol Symbol (NUM a)) (NUM b))

;    (int16ToUint16  (Int16)   Uint16)
    (int16ToInt     (Int16)   Int)
    (int16ToInt64   (Int16)   Int64)
    (int16ToFloat   (Int16)   Float)
    (int16ToDouble  (Int16)   Double)
    (int16ToComplex (Int16)   Complex)

;    (int64ToUint16  (Int64)   Uint16)
    (int64ToInt     (Int64)   Int)
    (int64ToInt16   (Int64)   Int16)
    (int64ToFloat   (Int64)   Float)
    (int64ToDouble  (Int64)   Double)
    (int64ToComplex (Int64)   Complex)

;     (uint16ToInt     (Uint16)   Int)
;     (uint16ToInt64   (Uint16)   Int64)
;     (uint16ToFloat   (Uint16)   Float)
;     (uint16ToDouble  (Uint16)   Double)
;     (uint16ToComplex (Uint16)   Complex)

    (intToInt16     (Int)   Int16)
    (intToInt64     (Int)   Int64)
    (intToFloat     (Int)   Float)
    (intToDouble    (Int)   Double)
    (intToComplex   (Int)   Complex)

    (floatToInt16   (Float)   Int16)
    (floatToInt64   (Float)   Int64)
    (floatToInt     (Float)   Int)
    (floatToDouble  (Float) Double)
    (floatToComplex (Float) Complex)

    (doubleToInt16   (Double)  Int16)
    (doubleToInt64   (Double)  Int64)
    (doubleToInt     (Double)  Int)
    (doubleToFloat   (Double)  Float)
    (doubleToComplex (Double)  Complex)

    (complexToInt16  (Complex) Int16)
    (complexToInt64  (Complex) Int64)
    (complexToInt    (Complex) Int)
    (complexToDouble (Complex) Double)
    (complexToFloat  (Complex) Float)


    ;(stringToInt64  (String)  Int64)
    (stringToInt     (String)  Int)
    (stringToFloat   (String)  Float)
    (stringToDouble  (String)  Double)
    (stringToComplex (String)  Complex)

    (__stringToInt_ARRAY     ((Array Char))  Int)
    (__stringToFloat_ARRAY   ((Array Char))  Float)
    (__stringToDouble_ARRAY  ((Array Char))  Double)
    (__stringToComplex_ARRAY ((Array Char))  Complex)

    ;(intToString     (Int)  String)
    
    ;; Rounding instead of truncation:    
    (roundF         (Float)   Float)
    (roundD         (Double)  Double)

    ,@infix-arith-prims

    (realpart (Complex) Float)
    (imagpart (Complex) Float)

    (sqrtI (Int)     Int)
    (sqrtF (Float)   Float)
    (sqrtD (Double)  Double)
    (sqrtC (Complex) Complex)    

    (moduloI (Int Int) Int)

    ;(absU8  (Uint8)  Uint8)
    ;(absU16 (Uint16) Unt16)    
    (absI16 (Int16) Int16)
    ;(absI32 (Int32) Int32)
    (absI64 (Int64) Int64)
    (absI (Int) Int)
    (absF (Float) Float)
    (absD (Double) Double)
    (absC (Complex) Float)

    (randomI (Int) Int)

;    (logI (Int) Int)
    (logF (Float) Float)
    (logD (Double) Double)
    
    (exptI (Int Int) Int)

;    (modF (Float Float) Float)
    
    (cos (Float) Float)
    (sin (Float) Float)
    (tan (Float) Float)
    (acos (Float) Float)
    (asin (Float) Float)
    (atan (Float) Float)

;    (sqr (Float) Float)

    ;; These should take NUM inputs too, as should < <= etc.
    (max ('a 'a) 'a)
    (min ('a 'a) 'a)

;    (eq? (Object Object) Bool)
    (wsequal? ('a 'a) Bool)
    ;; [2007.07.01] Finally removing this dated construct:
;    (eq? ('a 'a) Bool)  ;; This should just be '=' when it comes down to it.
    (null? ((List 'a)) Bool)

    ;; Bitwise operations:
    (lshiftI16 (Int16 Int)   Int16)
    (rshiftI16 (Int16 Int)   Int16)
    (logorI16  (Int16 Int16) Int16)
    (logandI16 (Int16 Int16) Int16)
    (logxorI16 (Int16 Int16) Int16)

    (lshiftU16 (Uint16 Int)   Uint16)
    (rshiftU16 (Uint16 Int)   Uint16)
    (logorU16  (Uint16 Uint16) Uint16)
    (logandU16 (Uint16 Uint16) Uint16)
    (logxorU16 (Uint16 Uint16) Uint16)

    (lshiftI32 (Int32 Int)   Int32)
    (rshiftI32 (Int32 Int)   Int32)
    (logorI32  (Int32 Int32) Int32)
    (logandI32 (Int32 Int32) Int32)
    (logxorI32 (Int32 Int32) Int32)

    ;; Written &&, ||, and not(b).
    (not (Bool) Bool)
    ;; These are actually SYNTAX, because short-circuiting is expected:
    (ws:or  (Bool Bool) Bool)
    (ws:and (Bool Bool) Bool)
    
    ;; These are in here so that various passes can treat them as
    ;; primitives rather than special forms.  (Just for the purpose of
    ;; eliminating repetitive code.) However, they don't have valid
    ;; types under our type system. 
    ;; TODO: [2006.09.01] I should probably take these out:
    (tuple Object Tuple)
    (tupref (Int Int Tuple) Object) ;; takes ind, len, tuple

    (empty-wsrecord () (Record #()))
    ;; NOTE: These are overly weak types, this cannot be used for
    ;; typechecking.  It's a hack to reduce the burden of handling
    ;; record ops explicitly in some places.
    (wsrecord-select (Symbol UnspecifiedRecordType) UnspecifiedRecordType)
    (wsrecord-extend (Symbol 'a UnspecifiedRecordType) UnspecifiedRecordType)
    (wsrecord-restrict (Symbol UnspecifiedRecordType) UnspecifiedRecordType)

    ;; Here we pretend similarly:
    ;; Foreign takes: 
    ;;  (1) function name 
    ;;  (2) foreign files (.c .h .so etc) to load function from
    (foreign        (String (List String)) 'any)
    ;; Takes (export) function name, file list:
    ;; Note: there's also a hack for wstiny that involves sticking a *rate* into the file list.
    (foreign_source (String (List String)) (Stream 'any))

    ;; (Internal) This is the same but has the type tagged on the end:
    (__foreign        (String (List String) (List Symbol)) 'any)
    (__foreign_source (String (List String) (List Symbol)) (Stream 'any))
    
    ;; ExclusivePointers are pointers that WS has sole ownership of,
    ;; and can "free" when finished with.
    (exclusivePtr   ((Pointer 'name)) (ExclusivePointer 'name))
    (getPtr         ((ExclusivePointer 'name)) (Pointer 'name))

    ;; This allows us to unpack a foreign pointer into an array,
    ;; copying the storage.  Second argument is size of array.
    ;; Must be wrapped in an assert-type of course:
    (ptrToArray     ((Pointer 'name) Int) (Array 'a))
    (ptrIsNull      ((Pointer 'name)) Bool)
    ;; YUCK: adding this also for initialization.
    (ptrMakeNull    () (Pointer "void*"))

    ;; This is down-and-dirty.  Inline C code.
    ;;
    ;; There's no good place to call it from either... thus it simply returns an empty stream.
    ;; We need a "pull and ignore" to invoke it but ignore it.
    ;;
    ;; The first argument is a set of top-level definitions to add to the linked C-code.
    ;; The second argument is either the null string, or the name of
    ;; an initialization function to call from main().
    (inline_C       (String String) (Stream 'a))

    ;; This is much more complicated in the TinyOS context because
    ;; there are several different places that one needs to inject code:
    ;;  (1) top level code
    ;;  (2) configuration block 
    ;;  (3) configuration/implementation block 
    ;;  (4) module block
    ;;  (5) module/implementation block
    ;;  (6) Boot-time code    
    ;;  (7) Cleanup code for when each traversal ends (at which point no tuples are in-flight)
    (inline_TOS     (String String String String String String String) (Stream 'a))

    ;; Null characters make things difficult, thus this uses Arrays:
    ;; Of course, these must have appropriate user type annotations.
    (marshal        ('a) (Array Uint8))
    (unmarshal      ((Array Uint8) Int) 'a) ;; Takes an offset also.
    ;; This will append a header with metadata to the start of the stream.
    ;(marshal_stream ((Stream 'a)) (Stream (Array Uint8)))
    
    (__type_unsafe_read ((Array Uint8) Int)     'a)
    
    ;; System CPU time
    ;; This returns milleseconds, but we needa better system for cross-platform support.
    (clock () Double)
    (realtime () Int64) ;; EXPERIMENTAL: Realtime since process started ms (or virtual time).
    
    ;; It would be nice to use the same primitives for time, but
    ;; TinyOS needs very particular notions of timers.
    ;; Also, I wanted to do this as a foreign function, but the compiler needs 
    ;(TOS:clock32khz () Uint16)

    ;(exit () 'any)
    
    (locdiff (Location Location) Float)

    ;; Shouldn't this be local??
    ;; I'm not sure...
;    (sense         (Symbol Node) Int)
    (sense         (String Node) Int)
    
    (nodeid        (Node) Int)

    ;; [2007.01.30] These can't currently be used from WS... no '?' character.
    (even? (Int) Bool)
    (odd?  (Int) Bool)

    ))


(IFWAVESCOPE
;; Adding Wavescope-related primitives:   
(define wavescript-primitives
  `( 


    (static          ('a)         (Static 'a))
    (statref         (Static 'a)  'a)

    ;; These are for second-class references (iterator state variables)
    (Mutable:ref     ('a)         (Ref 'a))
;    (ref             ('a)         (Ref 'a))
    (deref            ((Ref 'a))   'a)

    ;; Stream Sources:

    ;; This doesn't carry a time value, it just "fires" every so often.
    ;; Takes as input a frequency in hertz.
    ;; [2008.08.21] As a new convention, timers with zero rate fire exactly once.
    ;; Timers with positive rate start counting at time zero, but don't fire at time zero.
    ;; Thus it may make sense to merge() a zero-timer with a positive-timer.
    (timer            ((List Annotation) Float) (Stream #()))

    (prim_window           ((Stream 'a) Int) (Stream (Sigseg 'a)))

    ;; Ok, this uses Lewis's code... single channel sources:
    (ensBoxAudio      (Int ) (Stream (Sigseg Int16)))
    (ensBoxAudioF     (Int ) (Stream (Sigseg Float)))
    (ensBoxAudioAll   ()     (Stream (Sigseg Int16)))

    ;; Generic data-file reader.
    ;; The arguments are described in the manual.
    (readFile ((List Annotation) String String (Stream 'a)) (Stream 'b))
    ;; [2008.04.06] Should really tighten this up:
    ;(readFile ((List Annotation) String String (Stream #())) (Stream 'b))

    ;(HACK_O_RAMA (String) (Stream #(Int (List (Sigseg Int16)))))

    ;; Internal:
    ;(readFile   ((List Annotation) String (Stream 'a) String Int Int Int Int (List Symbol)) (Stream 'a))
    (__readFile ((List Annotation) String (Stream 'a) String Int Int Int Int (List Symbol)) (Stream 'a))
    (readFile-wsearly  (String (Stream 'a) String Int Int Int Int (List Symbol)) (Stream 'a))
    
    ;; These are simpler interface that desugar into dataFile:
    ;; They use defaults rather than exposing as many parameters.
    ;;
    ;; The reason they're not defined in a .ws file is that the
    ;; enclosing 'assert-type' must be handled properly.
    ;;
    ;(textFile (String) (Stream 'a))
    ;(binFile  (String) (Stream 'a))
    

    ;; This internal version works only in the emulator.
    ;(__syncN ((Stream #(Bool Int Int)) (List (Stream (Sigseg 't)))) (Stream (List (Sigseg 't))))

    ;; Fabricates stock ticks and splits.  For benchmark.
    ;; Tuple is of one of two forms:
    ;;   Tick:  #(sym,t,vol,price)
    ;;   Split: #(sym,t,-1,factor)
    ;(stockStream      ()  (Stream #(String Float Int Float)))

    (fftC              ((Array Complex))  (Array Complex))
    (ifftC             ((Array Complex))  (Array Complex))
    (fftR2C            ((Array Float))    (Array Complex))
    (ifftC2R           ((Array Complex))  (Array Float))

    ;; Takes input size, and a flag indicating "MEASURE" or "ESTIMATE"
    ;(fftw_R2Cplan      (Int String)       FFTWplan) ;; Should the output be a sum type?
    ;; This will be implemented in the standard library when I expose operators for manipulating FFTW plans:
    ;(stream_fftR2C     ((Stream (Sigseg Float))) (Stream (Sigseg Complex)))
    (memoized_fftR2C    ((Array Float))    (Array Complex))


;    (hanning          ((Sigseg Float))  (Sigseg Float))

    ;; This unions N streams of the same type, it returns a sample and
    ;; the index (in the original list) of the stream that produced it.
    ;;
    ;; [2007.06.04] Thinking of renaming these "MERGE"
    ;;
    ;; [2007.08.01] For the moment, keeping unionList AND merge.  One
    ;; provides indices for which input port the input came from, the
    ;; other doesn't.  Either one of these can be implemented with the
    ;; other, but for efficiency, we're keeping them separate.
    (unionList        ((List (Stream 'a))) (Stream #(Int 'a)))    
    ;; Two streams of the same type:
    (_merge            ((List Annotation) (Stream 'a) (Stream 'a)) (Stream 'a))

    ;; To the compiler, this looks like a merge:
    (IFPROFILE         ((Stream 'a) (Stream 'a)) (Stream 'a))

    ;; Currently, because of letrec semantics, an explicit operator
    ;; must be used to create a feedback loop.

    ;; It takes an input stream, and a stream transformer (ST).  The
    ;; input stream is fed into the ST, and the output of the ST is
    ;; sent downstream AND fed back to itself.
    ;;
    ;; Uniontypes need to be used or this will result in an infinite loop.
    (feedbackloop     ((Stream 't) ((Stream 't) -> (Stream 't)))  (Stream 't))

    (gnuplot_array    ((Array (NUM a))) #())
    ;; Takes an (X,Y) pair.
    (gnuplot_array2d  ((Array #((NUM a) (NUM b))))  #())  
    
    ;; Starts up a gnuplot process.
    ;; Feeds it data on one channel, and control messages on the other.
    (gnuplot_process  ((Stream String) (Stream String)) (Stream 'any))
    (spawnprocess  (String (Stream String)) (Stream String))



    ;; This synchronously joins two signals.
    ;(zip2           ((Stream 'a) (Stream 'b)) (Stream #('a 'b)))

    ;; We just pretend this is a primitive.  It does nothing.
    ;(return           ('a) 'a) ;; Don't need this yet.

    ;; Array handling:
    (Array:make        (Int 'a) (Array 'a))
    (Array:makeUNSAFE  (Int)    (Array 'a))
    (Array:ref         ((Array 'a) Int) 'a)
    (Array:length      ((Array 'a)) Int)
    (Array:toList      ((Array 'a))                         (List 'a))

    ;; Temporary:
    ;; Oops, need to be sure this doesn't change the numeric type in the output:
    (m_invert   ((Array (Array (NUM a))))  (Array (Array (NUM a))))

    ;; These can be useful for working with hash-tables indexed by strings.
    (internString (String) Symbol)
    (uninternString (Symbol) String)

    ;; TODO, this should probably be defined in terms of hash_param
    ;; which has depth-caps like Ocaml's version.
    (Internal:hash ('a) Int)

    (HashTable:make (Int) (HashTable 'key 'val))
    (HashTable:contains  ((HashTable 'key 'val) 'key) Bool)
    ;; TODO: need to change this to take a default value instead.
    (HashTable:get       ((HashTable 'key 'val) 'key) 'val)
    ;; This is the *pure* version, to be useful at all need to use the
    ;; destructive version.
    (HashTable:set       ((HashTable 'key 'val) 'key 'val) (HashTable 'key 'val))
    (HashTable:rem       ((HashTable 'key 'val) 'key) (HashTable 'key 'val))
   
    ;----------------------------------------------------------------------

    ;; I'm moving some of these higher order ops to "stdlib.ws" for now:

    ;; Only one-to-one output for now (Don't have a Maybe type!).
;    (iterate        (('in 'state -> #('out 'state)) 'state (Stream 'in)) (Stream 'out))
;    (deep-iterate   (('in 'state -> #('out 'state)) 'state (Stream (Sigseg 'in)))  (Stream (Sigseg 'out)))
;    (emit             ('a) #())

    ;; FIXME: putting the type as (List Annotation) is a bit of a hack
    (iterate        ((List Annotation) ('in (VQueue 'out) -> (VQueue 'out)) (Stream 'in))      (Stream 'out))
    ;(deep-iterate   (('in (VQueue 'out) -> (VQUeue 'out)) (Stream (Sigseg 'in)))  (Stream (Sigseg 'out)))
    ;(window-iterate (('in (VQueue 'out) -> (VQUeue 'out)) (Sigseg 'in))           (Sigseg 'out))
    
    ;(sigseg_foreach (('a -> 'b) (Sigseg 'a)) #())

    ;----------------------------------------------------------------------

    (show             ('a) String)
    (__show_ARRAY     ('a) (Array Char)) ;; Internal

    ;; These keep a gnuplot window open and repeatedly update it.
    ;; Shouldn't need both of these, one should be defined in wavescript:
    (gnuplot_array_stream   ((Stream (Array (NUM a)))) (Stream (Array (NUM a))))
    (gnuplot_sigseg_stream  ((Stream (Sigseg (NUM t)))) (Stream (Sigseg (NUM t))))

    ;; These take (X,Y) pairs.
    (gnuplot_array_stream2d  ((Stream (Array #((NUM a) (NUM b))))) (Stream (Array #((NUM a) (NUM b)))))
    (gnuplot_sigseg_stream2d ((Stream (Sigseg #((NUM a) (NUM b))))) (Stream (Sigseg #((NUM a) (NUM b)))))

    (string-append    (String String) String) ;; Rename String:append!!

    (String:length    (String) Int)
    (String:ref       (String Int) Char)
    (String:toList    (String) (List Char))
    (String:fromList  ((List Char)) String)
    (String:toArray   (String) (Array Char))
    (String:fromArray ((Array Char)) String)
    
    (intToChar (Int) Char)
    (charToInt (Char) Int)

    ;(String:sub      (String Int Int) String) ;; TOIMPLEMENT

    (toArray         ((Sigseg 'a))  (Array 'a))
    ;; TODO: This needs to take Int64's....
    ;; Takes array, start-sample-number, timebase.
    (toSigseg        ((Array 'a) Int64 Timebase)  (Sigseg 'a))

    ;; Can only append two subrefs that are part of the same physical timeseries.
    (joinsegs       ((Sigseg 'a) (Sigseg 'a)) (Sigseg 'a))

    ;; Takes a start sample # and a Length to copy.
    ;; Can produce nullseg if len=0.
    (subseg          ((Sigseg 'a) Int64 Int) (Sigseg 'a))

    ;; This is now zero indexed:
    (seg_get      ((Sigseg 'a) Int) 'a)
    (width        ((Sigseg 'a)) Int)
    
    ;; Returns absolute sample indexes.
    (start        ((Sigseg 'a)) Int64)  ;; inclusive start of range
    (end          ((Sigseg 'a)) Int64)  ;; inclusive end of range
    ;; Returns timebase:
    (timebase     ((Sigseg 'a)) Timebase)
    
    ;; INTERNAL!!!
    ;; (Should rename Internal:newTimebase)
    (Secret:newTimebase (Int)   Timebase)

    ;; Not exposing this polymorphic hash function to users right now.
    (Internal:hash ('a) Int)

    ;; This is for testing only... it's a multithreaded version:
    ;(parmap         (('in -> 'out) (Stream 'in))           (Stream 'out))

    ;; The WS version of nodeid is impure:
    (getID         () Int)

    ,@wavescript-effectful-primitives

)))




;; These are the distributed primitives.  The real Regiment combinators.   <br><br>
;; These pretty much apply only to Regiment 1.0 and not WaveScript.
(define regiment-distributed-primitives 
  `(
    
    (rmap           (('a -> 'b) (Area 'a)) (Area 'b))

    (rfold          (('a 'b -> 'b) 'b (Area 'a)) (Stream 'b))
    ;; This is similar to rfold, but simply streams out all the
    ;; contents of the region in no particular order:
    (rdump          ((Area 'a)) (Stream 'a))

    (smap           (('a -> 'b) (Stream 'a)) (Stream 'b))
    ;; Well, this is the nail in the coffin in any idea of a continuous signal model:
    (sfilter        (('a -> Bool) (Stream 'a)) (Stream 'a))
    
    ;; This joins two signals in the network.
    (smap2          (('a 'b -> 'c) (Stream 'a) (Stream 'b)) (Stream 'c))

    ;; This is the identity function on regions.  
    ;; However it also lights an LED.
    ;; [2006.04.04] MODIFIED it to also produce debug messages.
    (light-up ((Area 'a)) (Area 'a))
    ;; [2006.04.04] This is a similar thing for signals.
    (slight-up ((Stream 'a)) (Stream 'a))

    (anchor-at      (Int Int) (Stream Node))
    (anchor-dist    ((Stream Node) (Stream Node)) Float)

    ;(anchor-optimizing ((Node -> Int) Region) (Stream Node))

    ;; Takes a function to optimize, and an Area within which to 
    (anchor-maximizing ((Node -> Int) (Area 'a)) (Stream Node))

    (circle         ((Stream Node) Float)   (Area Node))
    (circle-at      (Int Int Float)  (Area Node))
    (k-neighborhood ((Stream Node) Int)     (Area Node))
    ;; Shorthand: 
    (khood          ((Stream Node) Int)      (Area Node))
    (khood-at       (Float Float Int) (Area Node))

    ;; This lifts a node value into the Stream monad:
    (node->anchor   (Node) (Stream Node))
    (node_to_anchor (Node) (Stream Node)) ;; Wavescript syntax.

    ;; This eliminates duplicate Stream types, by lifting out the inner one and canceling:
    ;; Has no operational meaning.
    (liftsig ((Area (Stream 'a))) (Area 'a))

    (rfilter         (('a -> Bool) (Area 'a)) (Area 'a))
    
    (rintersect       ((Area 'a) (Area 'a)) (Area 'a))
    (runion           ((Area 'a) (Area 'a)) (Area 'a))
    (rrflatten        ((Area (Area 'a)))    (Area 'a))

    ;; This one returns a region of regions:
    (rrcluster        ((Area 'a)) (Area (Area 'a)))
    
    ;;======================================================================
    ;; EXPERIMENTAL:

    ;; These don't a lot of sense yet:
    (sparsify       ((Area 'a)) (Area 'a))
    (border         ((Area 'a)) (Area 'a))

;    (planarize      (Area) Area)
;    (treeize        (Area) Area)

    ;; A "transpose" operations for looking at regions as local streams--from the "node's perspective"
    (rmap_localstreams (((Stream #(Node 'a)) -> (Stream 'b)) (Area 'a)) (Area 'b))
    ;; This version currently uses a default time-out for when to
    ;; consider a node having "left" the Region.  (When that occurs,
    ;; any state having to do with the signal transformer is
    ;; discarded, it will start up afresh if the node re-joins the
    ;; region.)
    ;;
    ;; This is an alternative that implements an integrate over a region:
    ;; The function also takes the Node that it's running on:
    (rintegrate      ((Node 'in 'state -> #('out 'state)) 'state (Area 'in))  (Area 'out))

    ;; Here's the pure version.
    ;(integrate      (('in 'state -> #((List 'out) 'state)) 'state (Stream 'in))  (Stream 'out))
    ;; Restricting to have only one output per firing:
    (integrate      ((Node 'in 'state -> #('out 'state)) 'state (Stream 'in))  (Stream 'out))

    ;; A communication primitive that gossips local values using broadcast:
    (gossip ((Area 'a)) (Area 'a))

    ;; Prolly not the right type:
    ;; Currently this ignores the value carried on the event:
    (until          ((Event 'a) (Stream 'b) (Stream 'b)) (Stream 'b))
    (runtil         ((Event 'a) (Area 'b) (Area 'b)) (Area 'b))
    (areaWhen       ((Event 'a) (Area 'b)) (Area 'b))

    ;; The float is a time in seconds.
    ;(constEvent     ('a Float) (Event 'a))

    ;; What was this one supposed to do and what was it's type?
;    (when           (Event Stream) Stream)
    (rwhen-any        (('a -> Bool) (Area 'a))       (Event #())) ; Could carry the winning value.
    (swhen-any        (('a -> Bool) (Stream 'a))     (Event #()))
    (when-percentage  (Float ('a -> Bool) (Area 'a)) (Event #()))

;    ,@(IFWAVESCOPE wavescript-primitives ())

;     neighbors 
;    time-of
;    (time (Node) Time)
     ))


;======================================================================
;;; Aggregates of the above sets of primitives.

;; This private state keeps a hash-table of all regiment primitives for fast lookup:
(define primitives-hash 'primitives-hash-uninitialized)  

; [2004.03.31]
;; [2007.01.30] Upgrading this to a parameter. (A box might have better performance.)
(define regiment-primitives
  (reg:make-parameter   
   (append regiment-basic-primitives
	   regiment-distributed-primitives
	   wavescript-primitives
	   meta-only-primitives
	   higher-order-primitives
	   regiment-constants)
   ;; Update the hash table when we change this parameter:
   (lambda (ls)
     ;; [2007.07.10] Hmm... didn't set the size before.  Setting now:
     (set! primitives-hash (make-default-hash-table 500))
     (for-each (lambda (entry)
		 (hashtab-set! primitives-hash (car entry) (cdr entry)))
       ls)
     ls)))

;; These are the ones that take or return Stream values:
;; Be wary that this is only computed once while "regiment-primitives" might change.
(define wavescript-stream-primitives
  (filter (lambda (x) (deep-assq 'Stream x))
    (filter (lambda (pr)
	      (not (memq (car pr) '(world anchor))))
      (difference (regiment-primitives) 
		  regiment-distributed-primitives))))

(define (stream-primitive? sym)
  (and (not (memq sym '(world anchor)))
       (let ([entry (regiment-primitive? sym)])
	 (and entry
	      (deep-assq 'Stream entry)))))

;; [2007.11.05] These are the primitives that currently carry an
;; "annotation" field after their 
(define temp-hack-stream-primitives 
  '(iterate unionN _merge readFile __readFile readFile-wsearly timer))
(define (temp-hack-stream-primitive? p) (memq p temp-hack-stream-primitives))

;======================================================================
;;; Type signatures for TML/Node-local primitives.

;; These are pretty much compiler-internal primitives which can
;; operate on the local node.
(define local-node-primitives 
  '(
    (my-id () NodeID)
    ;(gdist (Token) Int) ;; Phase this out "dist" is wrong.
    ;(ghopcount (Token) Int)
    ;(gparent (Token) NodeID)
    ;(gorigin (Token) NodeID)
    ;(gversion (Token) Int)
    
    ;; An internal primitive for sanity-checking.
    (check-tok (Token) Bool) 

    ;; Gotta have a way to remove tokens also!
    ;; Not to mention expiration dates!!

    (list ('a ...) (List 'a))
    (append List List)

    (rfoldwith (Token ('a 'b -> 'a) 'b (Area 'a)) (Stream 'b))
    ))


;; [2004.06.09]<br>  Many of these are actually language forms.  I gotta
;; get this sorted out eventually.
;; 
;; <br>[2004.10.22]  For now everything that handles tokens is a syntax not a prim.
;; <br>[2005.05] I revoked that.  Basically everything is a prim now.
(define token-machine-primitives
    ; Arithmetic prims:
  '(
    
    (+ (Int Int) Int) ;; TODO: Phase these out:
    (- (Int Int) Int) 
    (* (Int Int) Int) 
    (/ (Int Int) Int) 
    (^ (Int Int) Int)

    (_+_ (Int Int) Int) 
    (_-_ (Int Int) Int) 
    (*_ (Int Int) Int) 
    (/_ (Int Int) Int) 
    (^_ (Int Int) Int)

    (_+. (Float Float) Float) 
    (_-. (Float Float) Float) 
    (*. (Float Float) Float) 
    (/. (Float Float) Float) 
    (^. (Float Float) Float) 

    (int->float (Int) Float)
    (float->int (Float) Int)

    (max (Number Number) Number)
    (min (Number Number) Number)
    (abs (Number) Number)
    (cos (Float) Float)
    (sin (Float) Float)
    (tan (Float) Float)
    (acos (Float) Float)
    (asin (Float) Float)
    (atan (Float) Float)

    (not (Bool) Bool)
    ; predicates
    (=  (Number Number) Bool)
    (<  (Number Number) Bool)
    (>  (Number Number) Bool)
    (<=  (Number Number) Bool)
    (>=  (Number Number) Bool)
;    (eq? (Object Object) Bool)
    (equal? (Object Object) Bool)
    (eq? (Object Object) Bool)
    (even? (Int) Bool)
    (odd? (Int) Bool)
    (random (Int) Int)
    
    ;; Takes an optional second argument, that's the fun to optimize.
    (elect-leader Token . Token)
    (flood Token) ;; These are actually macros, but what the heck

;     (greturn)
;     (emit)
;     (relay)
;     (call)
;     (timed-call)
;     (activate)
;     (dist) 
     (light-node (Int Int Int) Void)
;     (sense)
     (my-id  () Int)
     (my-clock () Int)

     (linkqual-from (Int) Int) ;; Approximate percentage 0-100.
     (linkqual-to   (Int) Int) ;; Approximate percentage 0-100.

     (loc () List) ;(loc () Location)
     (locdiff (List List) Float) ;(locdiff (Location Location) Float)

     (dbg (String . Object) Void)

     (call (Token . Object) Void)
     ;; This is a rough attempt at a "high priority" scheduling.
     (call-fast (Token . Object) Void)
     (timed-call (Int Token . Object) Void)

     (subcall (Token . Object) Object)
     ;; This one happens immediately, possibly by inlining, but in any
     ;; case it does not represent a yield.
     (direct-subcall (Token . Object) Object)

     (bcast (Token . Object) Void)
     ;; This takes a node ID:
     (ucast (Int Token . Object) Void)
     ;; This is a synchronous command that returns the success or failure of the ucast.
     (ucast-wack (Int Token . Object) Bool)

     (return (Object) Void)
     ;(greturn (Object) Void) ;; This is a syntax, not a primitive.
     
     (token-scheduled? (Token) Bool)
     (token-deschedule (Token) Void)
     (token-present? (Token) Bool)
     (evict (Token) Void)
     ;; This one ignores subid and evicts all instances sharing the token name.
     (evict-all (Token) Void)

     (token->subid (Token) Int)

     (void () Object)

     (this () Object)
     ;; TODO: add different kinds of sensors.
     ;; This desugars into (subcall (tok SenseTok 0)):
;     (sync-sense Symbol Number)
     (sync-sense String Number)
     (soc-return (Number) Void)
     (soc-return-finished (Number) Void)
     
     ;; LISTS: TEMPORARY, just for debugging/testing:
     (cons (Object List) List) 
     (car (List) Object)
     (cdr (List) List)
     (cadr (List) Object)
     (null? (List) Bool)
     (list Object List)
     (List:length (List) Int)
     (append List List)
     (reverse (List) List)
     (map (Function List) List)
     (filter (Function List) List)
     (foldl (Function Object List) Object)
     (fold (Function Object List) Object)
     (alist_lookup (List Object) List)
     (alist_update (List Object Object) List)
     ;; This especially doesn't make too much sense.
     (tokmap (Token List) List)
    

     (vector Object Array)
     (make-vector (Int Object) Array)
     (vector-ref (Array Int) Object)
     (vector-set! (Array Int Object) Void)
     (vector-length (Array) Int)

     ;; For debugging only:
     (sim-print-queue Number Void)
     (error (Object String . Object) Void)
     (printf (String . Object) Void)
     (procedure? (Object) Bool)
     (pad-width (Number String) Void)
     ;; For simulator only:

     ;; This just displays text by a nodes' icon.
     (setlabel (String . Object) Void)
     (highlight-edge (Int) Void)
     ; [2005.04.30] Disabling these for now, will get them back up later.
     (draw-mark (List) Void)
     (rgb (Int Int Int) Object)
     ))


;======================================================================

;;; Various small functions, related to primitive tables.

;; Keywords allowed in the restricted token machine language.
(define token-machine-keyword?
  (lambda (x)
    (and (memq x '(quote set! if begin let let-stored)) #t)))


(define regiment-keyword?
  (lambda (x)
    (and (memq x '(quote set! if letrec lazy-letrec lambda)) #t)))

;; [2004.06.24]<br> This is for the regiment primitives:
(define get-primitive-entry
  (lambda (prim)
    (or 
     (let ([entry (regiment-primitive? prim)])
       (if entry (cons prim entry) #f))     
     (assq prim token-machine-primitives)
     (error 'get-primitive-entry
	    "no entry for this primitive: ~a" prim))))

(define (get-primitive-type prim) 
  (match (regiment-primitive? prim)
    [(,args ,ret) `(,@args -> ,ret)]
    [(,ret) ret]))

;; Gotta remember to update this if I change the format of prim entries..
(define (get-primitive-return-type prim)
  (last (get-primitive-entry prim)))

;; This lets you easily line up a primitives arguments with the expected types.
(define map-prim-w-types
  (lambda (f prim origargs)
    (let loop ([args origargs] [types (cadr (get-primitive-entry prim))])
      (cond
       [(null? args) '()]
       [(null? types)
	(error 'map-prim-w-types "too many arguments to prim ~a: ~a" prim origargs)]
       [(pair? types) 
	(cons (f (car args) (car types))
	      (loop (cdr args) (cdr types)))]
       [else (cons (f (car args) types)
		   (loop (cdr args) types))]))))

;; Is it a regiment primitive?
(define regiment-primitive? 
  ;; TOGGLE: list or hashtab based implementaition:
#;
  (lambda (x) (hashtab-get primitives-hash x))
  (lambda (x) ;; Slower association list version:
    (let ([entry (assq x (regiment-primitives))])
      (and entry (cdr entry)))))

;; Is it a regiment constant?
(define (regiment-constant? x)
  (if (assq x regiment-constants) #t #f))

;; More specific classification of Regiment primitives.
(define (basic-primitive? x) 
  (if (assq x regiment-basic-primitives) #t #f))
;; More specific classification of Regiment primitives.
(define (distributed-primitive? x) 
  (if (assq x regiment-distributed-primitives) #t #f))




;; Predicate for token machine primitives.
(define (token-machine-primitive? x)
  (if (assq x token-machine-primitives) #t #f))



;; Assert that the return value is void, or that it is forall a.a for
;; all effectful prims:
(for-each (lambda (x)	    
	    (DEBUGASSERT (match (last x)
			   [(quote ,v) (symbol? v)]
			   [#() #t]
			   [,else #f])))
  wavescript-effectful-primitives)

) ;; End library
