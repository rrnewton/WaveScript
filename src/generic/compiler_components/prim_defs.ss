;;;; The type-bindings for all Regiment and TML primitives.
;;;; .author Ryan Newton

;;;; TODO TODO TODO: Need to phase out the old type defs and make this
;;;; the only one.

;=============================================================

;;; Primitive type definitions, Regiment.
;;;
;;; [2004.07.28] Introducing 'Area'.  Note that a Region is also an Area.
;;; Ok, redoing primitive listings with type information:
;;; The types I'm using right now are:
;;;   Anchor, Area, Region, Stream, Event, Node, Location, Reading
;;;   Number, Int, Float, Bool, Void
;;;   List, Array, Tuple
;;;
;;; Then some types that are used only in the local language are:
;;;   Token NodeID

(module prim_defs mzscheme
  (require (lib "include.ss")
           "../../plt/iu-match.ss"
           "../../plt/hashtab.ss"
           "../constants.ss"
           "../util/helpers.ss"
           )
  (provide 
           regiment-type-aliases
	   regiment-basic-primitives
	   local-node-primitives
	   meta-only-primitives
	   higher-order-primitives
	   regiment-constants
	   regiment-distributed-primitives
	   regiment-primitives
	   wavescript-primitives
	   wavescript-effectful-primitives
	   wavescript-stream-primitives

	   generic-arith-primitives
	   token-machine-primitives

	   regiment-keyword?
 	   token-machine-keyword?
 	   get-primitive-entry
	   get-primitive-return-type
 	   map-prim-w-types
 	   regiment-primitive?
 	   regiment-constant?
 	   basic-primitive?
 	   distributed-primitive?
 	   token-machine-primitive?
	   )

  (chezimports )

;=============================================================

;;; The lists of primitives here have entries of the form: 
;;;
;;;   [PrimName Type]                   -- For constants   <br>
;;;   [PrimName ArgTypess ReturnType]   -- For functions   <br>

;; These are type aliases that are defined by default.
(define regiment-type-aliases
  '(

    [Region (Area Node)]
    [Anchor (Stream Node)]
    [NetDist   Float] ;; Network distance.  Depends on gradient implementation.
    ; [(Area 'a) (Stream (Space 'a))]
    ))

; ;; These are sum-types that are defined by default:
; (define regiment-sum-types
;   '([(Union2 'a 'b)          (Oneof2 'a) (Twoof2 'b)]
;     [(Union3 'a 'b 'c)       (Oneof3 'a) (Twoof3 'b) (Threeof3 'c)]
;     [(Union4 'a 'b 'c 'd)    (Oneof4 'a) (Twoof4 'b) (Threeof4 'c) (Fourof4 'c)]
;     [(Union5 'a 'b 'c 'd 'e) (Oneof5 'a) (Twoof5 'b) (Threeof5 'c) (Fourof5 'c) (Fiveof5 'c)]
;     ))

; fun union2(s1,s2) {
;   unionList([stream_map(Oneof2,s1),
; 	     stream_map(Twoof2,s1),])
; }


;; Hierarchy:
;; Int8
;; Int16
;; Int    Float
;; Int64  Double  Complex32
;;                Complex64
(define generic-arith-primitives
  '(
    (gint (Int) (NUM a))

    (g+ ((NUM a) (NUM a)) (NUM a))
    (g- ((NUM a) (NUM a)) (NUM a)) 
    (g* ((NUM a) (NUM a)) (NUM a)) 
    (g/ ((NUM a) (NUM a)) (NUM a)) 
    (g^ ((NUM a) (NUM a)) (NUM a)) ;; exponentiation

    ;; TODO: Add more generic operations.  UNFINISHED:
    
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



;; These are the basic (non-distributed) primitives supported by the Regiment language.
(define regiment-basic-primitives 
    ; value primitives
  `((cons ('a (List 'a)) (List 'a))
    (car ((List 'a)) 'a)
    (cdr ((List 'a)) (List 'a))
    
    ;; Some list primitives are exposed in the "Prelude".  These are just aliases:
    (head   ((List 'a)) 'a)      ;; Should remove car/cdr at some point.
    (tail   ((List 'a)) (List 'a))
    (append ((List 'a) (List 'a)) (List 'a))

    ;; The List namespace:
    (List:head ((List 'a)) 'a)
    (List:tail ((List 'a)) (List 'a))
    (List:ref  ((List 'a) Int) 'a)
    (List:append ((List 'a) (List 'a)) (List 'a))
    (List:make   (Int 'a) (List 'a))
    (List:length ((List 'a)) Int)
    (List:reverse ((List 'a)) (List 'a))
    ;(reverse_BANG ((List 'a)) (List 'a))

    ;; Should be maybe type!  For now returns list with match at head. Null otherwise.
    (List:assoc        ((List #('a 'b)) 'a) (List #('a 'b)))
    (List:assoc_update ((List #('a 'b)) 'a 'b) (List #('a 'b)))

;    (list ('a ...) (List 'a))
;    (cons (Object List) List) 
;    (cdr (List) List)
;    (car (List) Object)
;; [2005.10.20] Allowing improper lists for the moment ^^^

    ,@generic-arith-primitives
    
    (int16ToInt     (Int16)   Int)
    (int16ToFloat   (Int16)   Float)
    (int16ToDouble  (Int16)   Double)
    (int16ToComplex (Int16)   Complex)

    (intToInt16     (Int)   Int16)
    (intToFloat     (Int)   Float)
    (intToDouble    (Int)   Double)
    (intToComplex   (Int)   Complex)

    (floatToInt16   (Float)   Int16)
    (floatToInt     (Float)   Int)
    (floatToDouble  (Float) Double)
    (floatToComplex (Float) Complex)

    (doubleToInt16   (Double)  Int16)
    (doubleToInt     (Double)  Int)
    (doubleToFloat   (Double)  Float)
    (doubleToComplex (Double)  Complex)

    (complexToInt16  (Complex) Int16)
    (complexToInt    (Complex) Int)
    (complexToDouble (Complex) Double)
    (complexToFloat  (Complex) Float)

    (stringToInt     (String)  Int)
    (stringToFloat   (String)  Float)
    (stringToDouble  (String)  Float)
    (stringToComplex (String)  Complex)
    
    ;; Rounding instead of truncation:    
    (roundF         (Float)   Float)

    (+_ (Int Int) Int)
    (-_ (Int Int) Int) 
    (*_ (Int Int) Int) 
    (/_ (Int Int) Int) 
    (^_ (Int Int) Int) ;; exponentiation

    (+I16 (Int16 Int16) Int16)
    (-I16 (Int16 Int16) Int16) 
    (*I16 (Int16 Int16) Int16) 
    (/I16 (Int16 Int16) Int16) 
    (^I16 (Int16 Int16) Int16) ;; exponentiation

    (+. (Float Float) Float)
    (-. (Float Float) Float)
    (*. (Float Float) Float)
    (/. (Float Float) Float)
    (^. (Float Float) Float) ;; exponentiation

    (+: (Complex Complex) Complex)
    (-: (Complex Complex) Complex) 
    (*: (Complex Complex) Complex) 
    (/: (Complex Complex) Complex)
    (^: (Complex Complex) Complex)

    (realpart (Complex) Float)
    (imagpart (Complex) Float)

    (sqrtI (Int)     Int)
    (sqrtF (Float)   Float)
    (sqrtC (Complex) Complex)    
    
    (absI16 (Int16) Int16)
    (absI (Int) Int)
    (absF (Float) Float)
    (absC (Complex) Float)
    
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

    ; predicates
    ;; Restricted version for numbers:
    ;(=  ((NUM a) (NUM a)) Bool)
    (=  ('a 'a) Bool)     ;; <- PHASE OUT
    (<  ('a 'a) Bool)
    (>  ('a 'a) Bool)
    (<=  ('a 'a) Bool)
    (>=  ('a 'a) Bool)

;    (eq? (Object Object) Bool)
    (equal? ('a 'a) Bool)
    (eq? ('a 'a) Bool)  ;; This should just be '=' when it comes down to it.
    (null? ((List 'a)) Bool)

    ;; Written &&, ||, and not(b).
    (not (Bool) Bool)
    (or  (Bool Bool) Bool)
    (and (Bool Bool) Bool)
    (show-and-string-append ('a 'b) String)
    
    ;; These are in here so that various passes can treat them as
    ;; primitives rather than special forms.  (Just for the purpose of
    ;; eliminating repetitive code.) However, they don't have valid
    ;; types under our type system. 
    ;; TODO: [2006.09.01] I should probably take these out:
    (tuple Object Tuple)
    (tupref (Tuple Int Int) Object)

    ;; Here we pretend similarly:
    ;; Foreign takes: 
    ;;  (1) function name 
    ;;  (2) foreign files (.c .h .so etc) to load function from
    ;;  (3) list of C pointer types for any Pointer arguments to the function
    (foreign        (String (List String) (List String)) 'any)
    ;; (Internal) This is the same but has the type tagged on the end:
    (__foreign      (String (List String) (List String) (List Symbol)) 'any)

    (exclusivePtr   ((Pointer 'name)) (ExclusivePointer 'name))
    (getPtr         ((ExclusivePointer 'name)) (Pointer 'name))

    ;; Not implemented yet:
    ;(marshal        ('a) String)
    ;(unmarshal        (String) 'a)
    ;; Null characters make things difficult, this should probably use Arrays:
    ;(marshal        ('a) (Array Byte))
    ;(unmarshal      ((Array Byte)) 'a)
    
    ;; Not implemented yet:
    ;(foreign_box    (String (List String)) 'any)
    ;; This won't work in the schem backend...
    ;(foreign_source (String (List String)) 'any)


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



;; These are (or will be) allowed in both the meta and the object
;; language.  They're higher order, but in the object language the
;; lambda expression parameterizing each of these primitives must be
;; *known* so that first-order code may be generated.
(define higher-order-primitives
  '(
    (map (('a -> 'b) (List 'a)) (List 'b))
    (fold (('acc 'b -> 'acc) 'acc (List 'b)) 'acc)

    ;; These should be defined IN the language, but they're not right now:
    (List:map (('a -> 'b) (List 'a)) (List 'b))
    (List:fold (('acc 'b -> 'acc) 'acc (List 'b)) 'acc)
    ;;(List:filter (('a -> Bool) (List 'a)) (List 'a))

    ;; A lot of these can be defined in the language once we figure
    ;; out a story for "library" (non-inlined) procedures and
    ;; second-class references.
    (Array:map         (('a -> 'b) (Array 'a))              (Array 'b))
    (Array:fold        (('acc 'b -> 'acc) 'acc (Array 'b))  'acc)
    (Array:andmap      (('a -> Bool) (Array 'a))            Bool)
    ;; This uses an initialization function to fill in an array:
    ;; It's zero-based!
    (Array:build       (Int (Int -> 'a)) (Array 'a))
    ))


;; Only for the meta-language, shouldn't exist after static-elaborate.
;; TODO: FIXME: Don't think I enforce this right now!!
(define meta-only-primitives
  `(
    (GETENV (String) String) ; Returns "" if the env var is unbound.
    (FILE_EXISTS (String) Bool) ; Returns "" if the env var is unbound.
   
    ))

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

     (gnuplot_array    ((Array (NUM a))) #())
     ;; Takes an (X,Y) pair.
     (gnuplot_array2d  ((Array #((NUM a) (NUM b))))  #())  

     ;; I just use a virtual "Queue" to make the type-checking work for emits:
     (emit           ((VQueue 'a) 'a)                #())

     ;; This isn't a primitive, but it's nice to pretend it is so not all passes have to treat it.
     (break            ()                      'a)

     ;; Signals an error, has any return type:
     (wserror         (String)                  'a)
     (inspect         ('a)                      'a)

     )))



(IFWAVESCOPE
;; Adding Wavescope-related primitives:   
(define wavescript-primitives
  `( 


    (static          ('a)         (Static 'a))
    (statref         (Static 'a)  'a)

    ;; These are for second-class references (iterator state variables)
    (Mutable:ref     ('a)         (Ref 'a))
    (ref             ('a)         (Ref 'a))
    (deref            ((Ref 'a))   'a)

    ;; Stream Sources:

    ;; This doesn't carry a time value, it just "fires" every so often.
    ;; Takes as input a frequency in hertz.
    (timer            (Float) (Stream #()))

    (prim_window           ((Stream 'a) Int) (Stream (Sigseg 'a)))

    ;; Takes channel, window size, overlap, sampling rate:
    ;; This reads a hard-wired file of marmot-data.
    ;; The format is four interleaved channels of 16-bit signed ints.
    (ENSBoxAudio      (Int Int Int Int) (Stream (Sigseg Int16)))
    (ENSBoxAudioF     (Int Int Int Int) (Stream (Sigseg Float)))

    ;; Could think about a generic (HardwareSource "ENSBoxAudio(4096,)")
    ;; that provides a hack into the C++ generation.  Nasty and
    ;; backend-dependent, but really, how are hardware data sources
    ;; not going to be backend dependent?

    ;(audio      (Int Int Int Int) (Stream (Sigseg Float)))

    ;; Generic data-file reader.
    ;; Usage: datafile(fileName, mode, rate, repeats)
    ;;  Where mode is "text" "text-comma" or "binary".
    ;;    "text" has whitespace-separated values.
    ;;  Repeats is an integer representing the number of times to
    ;;    repeat the files data.  -1 encodes "indefinitely"
    ;;  Rate is simulated samples/second to read the file's tuples.
    ;; 
    ;; dataFile must occur directly within a ( :: T) construct.
    (dataFile (String String Int Int) (Stream 'a))
    ;; Internal compiler construct:
    ;(__dataFile (String String Int Int (List Symbol)) (Stream 'a))

    ;; This version reads blocks of tuples at a time:
;    (dataFileWindowed (String String Int Int Int) (Stream (Sigseg 'a)))

    ;; TODO: The number of options this should take is way too large.
    ;; Let's make it a list of optional arguments:
    ;;(dataFile (String (List #(String String))) (Stream 'a))
    ;dataFile("foo.txt", [("mode","text"), ("repeats","-1"), ("rate","44000"), ("advance", "8")]);
    ;dataFile("foo.txt", "mode: text  repeats: -1  rate: 44000  advance: 8");
    ;dataFile("foo.raw", "mode= binary  repeats= -1");

    ; file, fileStream, streamFile, fileSource, readFile, openFile???
    ;; Maybe we could even use the same prim for windowed or unwindowed... it just depends on the type ascription.
    (readFile (String String) (Stream 'a))
    (__readFile (String String Int Int (List Symbol)) (Stream 'a))

    
    ;; These are simpler interface that desugar into dataFile:
    ;; They use defaults rather than exposing as many parameters.
    ;;
    ;; The reason they're not defined in a .ws file is that the
    ;; enclosing 'assert-type' must be handled properly.
    ;;
    ;(textFile (String) (Stream 'a))
    ;(binFile  (String) (Stream 'a))
    

    ;; Takes a file to read from, window size, overlap, sampling rate:
    ;; Reads a stream of Uint16's from the file.
    (audioFile        (String Int Int Int)  (Stream (Sigseg Int)))


    ;; This internal version works only in the emulator.
    ;(__syncN ((Stream #(Bool Int Int)) (List (Stream (Sigseg 't)))) (Stream (List (Sigseg 't))))

    ;; Fabricates stock ticks and splits.  For benchmark.
    ;; Tuple is of one of two forms:
    ;;   Tick:  #(sym,t,vol,price)
    ;;   Split: #(sym,t,-1,factor)
    ;(stockStream      ()  (Stream #(String Float Int Float)))

    ;; This version is to read a file containing doubles.
    ;; HACK: Currently it expects a text file rather than a binary file for the 
    ;; interpreted version of the system.
    ;(doubleFile        (String Int Int)  (Stream (Sigseg Float)))


    (fftC              ((Array Complex))  (Array Complex))
    (ifftC             ((Array Complex))  (Array Complex))
    (fftR2C            ((Array Float))    (Array Complex))
    (ifftC2R           ((Array Complex))  (Array Float))

;    (hanning          ((Sigseg Float))  (Sigseg Float))

    ;; This unions N streams of the same type, it returns a sample and
    ;; the index (in the original list) of the stream that produced it.
    (unionList        ((List (Stream 'a))) (Stream #(Int 'a)))

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

    (HashTable:make (Int) (HashTable 'key 'val))
    (HashTable:contains  ((HashTable 'key 'val) 'key) Bool)
    (HashTable:get       ((HashTable 'key 'val) 'key) 'val)
    ;; This is the *pure* version, to be useful at all need to use the
    ;; destructive version.
    ;(hashset ((HashTable #('key 'val)) 'key 'val) (HashTable #('key 'val)))
    (HashTable:set       ((HashTable 'key 'val) 'key 'val) (HashTable 'key 'val))
    (HashTable:rem       ((HashTable 'key 'val) 'key) (HashTable 'key 'val))
   
    ;----------------------------------------------------------------------

    ;; I'm moving some of these higher order ops to "stdlib.ws" for now:

    ;; Only one-to-one output for now (Don't have a Maybe type!).
;    (iterate        (('in 'state -> #('out 'state)) 'state (Stream 'in)) (Stream 'out))
;    (deep-iterate   (('in 'state -> #('out 'state)) 'state (Stream (Sigseg 'in)))  (Stream (Sigseg 'out)))
;    (emit             ('a) #())

    (iterate        (('in (VQueue 'out) -> (VQueue 'out)) (Stream 'in))           (Stream 'out))
    ;(deep-iterate   (('in (VQueue 'out) -> (VQUeue 'out)) (Stream (Sigseg 'in)))  (Stream (Sigseg 'out)))
    ;(window-iterate (('in (VQueue 'out) -> (VQUeue 'out)) (Sigseg 'in))           (Sigseg 'out))
    
    ;(sigseg_foreach (('a -> 'b) (Sigseg 'a)) #())

    ;----------------------------------------------------------------------

    (show             ('a) String)

    ;; Shouldn't need both of these, one should be defined in wavescript:
    (gnuplot_array_stream   ((Stream (Array (NUM a)))) (Stream (Array (NUM a))))
    (gnuplot_sigseg_stream  ((Stream (Sigseg (NUM a)))) (Stream (Sigseg (NUM a))))

    ;; These take (X,Y) pairs.
    (gnuplot_array_stream2d  ((Stream (Array #((NUM a) (NUM b))))) (Stream (Array #((NUM a) (NUM b)))))
    (gnuplot_sigseg_stream2d ((Stream (Sigseg #((NUM a) (NUM b))))) (Stream (Sigseg #((NUM a) (NUM b)))))

    (string-append    (String String) String) ;; Rename String:append!!
    ;(String:sub      (String Int Int) String) ;; TOIMPLEMENT

    (toArray         ((Sigseg 'a))  (Array 'a))
    ;; TODO: This needs to take Int64's....
    ;; Takes array, start-sample-number, timebase.
    (toSigseg        ((Array 'a) Int Timebase)  (Sigseg 'a))

    ;; Can only append two subrefs that are part of the same physical timeseries.
    (joinsegs       ((Sigseg 'a) (Sigseg 'a)) (Sigseg 'a))

    ;; Takes a start sample # and a Length to copy.
    ;; Can produce nullseg if len=0.
    (subseg          ((Sigseg 'a) Int Int) (Sigseg 'a))

    ;; This is now zero indexed:
    (seg-get      ((Sigseg 'a) Int) 'a)
    (width        ((Sigseg 'a)) Int)
    
    ;; Returns absolute sample indexes.
    (start        ((Sigseg 'a)) Int)  ;; inclusive start of range
    (end          ((Sigseg 'a)) Int)  ;; inclusive end of range
    ;; Returns timebase:
    (timebase     ((Sigseg 'a)) Timebase)

    ;; This is for testing only... it's a multithreaded version:
    ;(parmap         (('in -> 'out) (Stream 'in))           (Stream 'out))


    ,@wavescript-effectful-primitives

)))






;; These are the distributed primitives.  The real Regiment combinators.
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
     (set! primitives-hash (make-default-hash-table))
     (for-each (lambda (entry)
		 (hashtab-set! primitives-hash (car entry) (cdr entry)))
       ls)
     ls)))


;; These are the ones that take or return Stream values:
;; Be wary that this is only computed once while "regiment-primitives" might change.
(define wavescript-stream-primitives
  (filter (lambda (x) (deep-assq 'Stream x))
    (difference (regiment-primitives) 
		regiment-distributed-primitives)))

;======================================================================
;;; Primitive type definitions, TML/Node-local.


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

    (+_ (Int Int) Int) 
    (-_ (Int Int) Int) 
    (*_ (Int Int) Int) 
    (/_ (Int Int) Int) 
    (^_ (Int Int) Int)

    (+. (Float Float) Float) 
    (-. (Float Float) Float) 
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

;;; Various small, related functions.

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
    (or (let ([entry (hashtab-get primitives-hash prim)])
	  (if entry (cons prim entry) #f))
	(assq prim token-machine-primitives)
        (error 'get-primitive-entry
               "no entry for this primitive: ~a" prim))))

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

#;
(define (get-primitive-arity prim)
  (let* ([entry (get-primitive-entry prim)]
	 [args (cadr entry)])
    (cond
     [(eq? 'Constant args) #f]
     [else (length args)])))

;; Is it a regiment primitive?
(define (regiment-primitive? x)
  (hashtab-get primitives-hash x))

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

) ;; End module
