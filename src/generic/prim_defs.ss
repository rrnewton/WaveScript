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
;;;   Anchor, Area, Region, Signal, Event, Node, Location, Reading
;;;   Number, Integer, Float, Bool, Void
;;;   List, Array, Tuple
;;;
;;; Then some types that are used only in the local language are:
;;;   Token NodeID


(module prim_defs mzscheme
  (require "../plt/iu-match.ss"
           "../generic/constants.ss"
           "../plt/helpers.ss"
           (lib "include.ss")
           )
  (provide 
           regiment-type-aliases
	   regiment-basic-primitives
	   local-node-primitives
	   regiment-constants
	   regiment-distributed-primitives
	   regiment-primitives
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

;; These are type aliases that are defined by default.
(define regiment-type-aliases
  '([Region (Area Node)]
    [Anchor (Signal Node)]
    ;[Dist   Float]
    ; [(Area 'a) (Signal (Space 'a))]
    ))

;; These are the basic (non-distributed) primitives supported by the Regiment language.
(define regiment-basic-primitives 
    ; value primitives
  '((cons ('a (List 'a)) (List 'a))
    (cdr ((List 'a)) (List 'a))
    (car ((List 'a)) 'a)
    (append ((List 'a) (List 'a)) (List 'a))
;    (list ('a ...) (List 'a))
;    (cons (Object List) List) 
;    (cdr (List) List)
;    (car (List) Object)
;; [2005.10.20] Allowing improper lists for the moment ^^^

    (+ (Integer Integer) Integer)
    (- (Integer Integer) Integer) 
    (* (Integer Integer) Integer) 
    (/ (Integer Integer) Integer) 

    (+. (Float Float) Float)
    (-. (Float Float) Float)
    (*. (Float Float) Float)
    (/. (Float Float) Float)
    (max ('a 'a) 'a)
    (min ('a 'a) 'a)
    (abs (Integer Integer) Integer)

    ;(vector ('a ...) (Array 'a))
    ;(make-vector (Object Integer) Array)
    ;(vector-ref ((Array 'a) Integer) 'a)
    ;(vector-set! (Array Integer Object) Void)
    
    ;; These are in here so that various passes can treat them as
    ;; primitives rather than special forms.  (Just for the purpose of
    ;; eliminating repetitive code.) However, they don't have valid
    ;; types under our type system. 
    (tuple Object Tuple)
    (tupref Integer Integer Object)

    (locdiff (Location Location) Float)

    (not (Bool) Bool)
    (or (Bool Bool) Bool)
    (and (Bool Bool) Bool)

    ; predicates
    (=  ('a 'a) Bool)
    (<  ('a 'a) Bool)
    (>  ('a 'a) Bool)
    (<=  ('a 'a) Bool)
    (>=  ('a 'a) Bool)
;    (eq? (Object Object) Bool)
    (equal? ('a 'a) Bool)
    (eq? ('a 'a) Bool)  ;; This should just be = when it comes down to it.
    (null? ((List 'a)) Bool)

    ;; These are dynamically typed primitives: 
    ;(pair? (Object) Bool)
    ;(number? (Object) Bool)
    (even? (Integer) Bool)
    (odd? (Integer) Bool)

    ;; Shouldn't this be local??
    ;; I'm not sure...
    (sense         (Symbol Node) Integer)
    (nodeid        (Node) Integer)

    ))


;; These count as primitives also.
;; All regiment constants are presumed to be "slow prims" for
;; now. (see add-heartbeats)
(define regiment-constants
  '(
    (world          Region)
    (anchor         Anchor)
    ))

;; These are the distributed primitives.  The real Regiment combinators.
(define regiment-distributed-primitives 
  '(
    
    (rmap           (('a -> 'b) (Area 'a)) (Area 'b))

    (rfold          (('a 'b -> 'b) 'b (Area 'a)) (Signal 'b))
    (smap           (('a -> 'b) (Signal 'a)) (Signal 'b))
    
    ;; This joins two signals in the network.
    (smap2          (('a 'b -> 'c) (Signal 'a) (Signal 'b)) (Signal 'c))

    ;; This is the identity function on regions.  
    ;; However it also lights an LED.
    (light-up ((Area 'a)) (Area 'a))

    (anchor-at      (Integer Integer) Anchor)
    (anchor-dist    (Anchor Anchor) Float)

    ;(anchor-optimizing ((Node -> Integer) Region) Anchor)

    ;; Takes a function to optimize, and an optional refresh rate to re-elect at:
    ;; If no refresh rate is zero, the election only happens once.
    (anchor-maximizing ((Node -> Integer) Integer) Anchor)

    (circle         (Anchor Float)   Region)
    (circle-at      (Integer Integer Float) Region)
    (k-neighborhood (Anchor Integer) Region)
    ;; Shorthand: 
    (khood          (Anchor Integer) Region)
    (khood-at       (Float Float Integer) Region)

    ;; This lifts a node value into the Signal monad:
    (node->anchor   (Node) Anchor)

    (rfilter         (('a -> Bool) (Area 'a)) (Area 'a))
    
    (rintersect       ((Area 'a) (Area 'a)) (Area 'a))
    (runion           ((Area 'a) (Area 'a)) (Area 'a))
    (rrflatten        ((Area (Area 'a)))    (Area 'a))

    ;; This one returns a region of regions:
    (rrcluster        ((Area 'a)) (Area (Area 'a)))
    ;; These don't a lot of sense yet:
    (sparsify       ((Area 'a)) (Area 'a))
    (border         ((Area 'a)) (Area 'a))

;    (planarize      (Area) Area)
;    (treeize        (Area) Area)

    ;; Prolly not the right type:
    ;; Currently this ignores the value carried on the event:
    (until          ((Event 'a) (Signal 'b) (Signal 'b)) (Signal 'b))
    (runtil         ((Event 'a) (Area 'b) (Area 'b)) (Area 'b))
    (areaWhen       ((Event 'a) (Area 'b)) (Area 'b))

    ;; The float is a time in seconds.
    ;(constEvent     ('a Float) (Event 'a))

    ;; What was this one supposed to do and what was it's type?
;    (when           (Event Signal) Signal)
    (rwhen-any        (('a -> Bool) (Area 'a))       (Event #())) ; Could carry the winning value.
    (swhen-any        (('a -> Bool) (Signal 'a))     (Event #()))
    (when-percentage  (Float ('a -> Bool) (Area 'a)) (Event #()))

;     neighbors 
;    time-of
;    (time (Node) Time)
     ))
  
; [2004.03.31]
(define regiment-primitives
  (append regiment-basic-primitives
	  regiment-distributed-primitives
	  regiment-constants))

;======================================================================
;;; Primitive type definitions, TML/Node-local.

;; These are pretty much compiler-internal primitives which can
;; operate on the local node.
(define local-node-primitives 
  '(
    (my-id () NodeID)
    ;(gdist (Token) Integer) ;; Phase this out "dist" is wrong.
    ;(ghopcount (Token) Integer)
    ;(gparent (Token) NodeID)
    ;(gorigin (Token) NodeID)
    ;(gversion (Token) Integer)
    
    ;; An internal primitive for sanity-checking.
    (check-tok (Token) Bool) 

    ;; Gotta have a way to remove tokens also!
    ;; Not to mention expiration dates!!

    (list ('a ...) (List 'a))
    (append List List)

    (rfoldwith (Token ('a 'b -> 'a) 'b (Area 'a)) (Signal 'b))
    ))


;; [2004.06.09]<br>  Many of these are actually language forms.  I gotta
;; get this sorted out eventually.
;; 
;; <br>[2004.10.22]  For now everything that handles tokens is a syntax not a prim.
;; <br>[2005.05] I revoked that.  Basically everything is a prim now.
(define token-machine-primitives
    ; Arithmetic prims:
  '((+ (Integer Integer) Integer) 
    (- (Integer Integer) Integer) 
    (* (Integer Integer) Integer) 
    (/ (Integer Integer) Integer) 
    (max (Number Number) Number)
    (min (Number Number) Number)
    (abs (Number) Number)
    (cos (Float) Float)
    (sin (Float) Float)
    (tan (Float) Float)
    (acos (Float) Float)
    (asin (Float) Float)
    (atan (Float) Float)

    (+. (Float Float) Float) 
    (-. (Float Float) Float) 
    (*. (Float Float) Float) 
    (/. (Float Float) Float) 
    (int->float (Integer) Float)
    (float->int (Float) Integer)

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
    (even? (Integer) Bool)
    (odd? (Integer) Bool)
    (random (Integer) Integer)
    
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
     (light-node (Integer Integer Integer) Void)
;     (sense)
     (my-id  () Integer)
     (my-clock () Integer)

     (linkqual-from (Integer) Integer) ;; Approximate percentage 0-100.
     (linkqual-to   (Integer) Integer) ;; Approximate percentage 0-100.

     (loc () List) ;(loc () Location)
     (locdiff (List List) Float) ;(locdiff (Location Location) Float)

     (dbg (String . Object) Void)

     (call (Token . Object) Void)
     ;; This is a rough attempt at a "high priority" scheduling.
     (call-fast (Token . Object) Void)
     (timed-call (Integer Token . Object) Void)

     (subcall (Token . Object) Object)
     ;; This one happens immediately, possibly by inlining, but in any
     ;; case it does not represent a yield.
     (direct-subcall (Token . Object) Object)

     (bcast (Token . Object) Void)
     ;; This takes a node ID:
     (ucast (Integer Token . Object) Void)
     ;; This is a synchronous command that returns the success or failure of the ucast.
     (ucast-wack (Integer Token . Object) Bool)

     (return (Object) Void)
     ;(greturn (Object) Void) ;; This is a syntax, not a primitive.
     
     (token-scheduled? (Token) Bool)
     (token-deschedule (Token) Void)
     (token-present? (Token) Bool)
     (evict (Token) Void)
     ;; This one ignores subid and evicts all instances sharing the token name.
     (evict-all (Token) Void)

     (token->subid (Token) Integer)

     (void () Object)

     (this () Object)
     ;; TODO: add different kinds of sensors.
     ;; This desugars into (subcall (tok SenseTok 0)):
     (sync-sense Symbol Number)
     (soc-return (Number) Void)
     (soc-return-finished (Number) Void)
     
     ;; LISTS: TEMPORARY, just for debugging/testing:
     (cons (Object List) List) 
     (car (List) Object)
     (cdr (List) List)
     (cadr (List) Object)
     (null? (List) Bool)
     (list Object List)
     (append List List)
    
     (vector Object Array)
     (make-vector (Integer Object) Array)
     (vector-ref (Array Integer) Object)
     (vector-set! (Array Integer Object) Void)
     (vector-length (Array) Integer)

     ;; For debugging only:
     (sim-print-queue Number Void)
     (error (Object String . Object) Void)
     (printf (String . Object) Void)
     (procedure? (Object) Bool)
     (pad-width (Number String) Void)
     ;; For simulator only:

     ;; This just displays text by a nodes' icon.
     (setlabel (String . Object) Void)
     (highlight-edge (Integer) Void)
     ; [2005.04.30] Disabling these for now, will get them back up later.
     (draw-mark (List) Void)
     (rgb (Integer Integer Integer) Object)
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
    (or (assq prim regiment-primitives)
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
  (if (assq x regiment-primitives) #t #f))

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


) ;; end module
