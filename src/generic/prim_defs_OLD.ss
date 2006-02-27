;;;; The type-bindings for all Regiment and TML primitives.
;;;; .author Ryan Newton


;=============================================================

;; [2004.07.28] Introducing 'Area'.  Note that a Region is also an Area.
;; Ok, redoing primitive listings with type information:
;; The types I'm using right now are:
;;   Anchor, Area, Region, Signal, Event, Node, Location, Reading
;;   Function, Number, Integer, Float, Bool, Object, Void
;;   List, Array

;; Since I'm going to go statically typed eventually, Object is just
;; my way of signifying "for all alpha" right now.

;; And the old types from the Snet compiler were:
;;   Bool Char Float64 Int32 List Object
;;   Number Pair Port String Symbol Vector Void


;; Then some types that are used only in the local language are:
;;   Token NodeID

;; These are really the *local* primitives:
(define regiment-basic-primitives 
    ; value primitives
  '((cons (Object Object) Pair)
    (cdr (Pair) Object)
    (car (Pair) Object)
    (append (List List) List)
    (list Object List)
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
    (max (Number Number) Number)
    (min (Number Number) Number)
    (abs (Number) Number)

    (vector Object Array)
    ;(make-vector (Object Integer) Array)
    (vector-ref (Array Integer) Object)
    ;(vector-set! (Array Integer Object) Void)

    (tuple Object Tuple)
    (tupref Integer Integer Object)

    (locdiff (Location Location) Float)

    (not (Bool) Bool)
    (or Bool Bool)
    (and Bool Bool)

    ; predicates
    (=  (Number Number) Bool)
    (<  (Number Number) Bool)
    (>  (Number Number) Bool)
    (<=  (Number Number) Bool)
    (>=  (Number Number) Bool)
;    (eq? (Object Object) Bool)
    (equal? (Object Object) Bool)
    (eq? (Object Object) Bool)  ;; This should just be = when it comes down to it.
    (null? (List) Bool)

    ;; These are dynamically typed primitives: 
    (pair? (Object) Bool)
    (number? (Object) Bool)
    (even? (Integer) Bool)
    (odd? (Integer) Bool)

    ;; Shouldn't this be local??
    ;; I'm not sure...
    (sense         (Symbol Node)  Float)  ;; Takes either (Node) or (Symbol Node)
    (nodeid        (Node) Integer)
    ;(nodeclock     (Node) Integer)        ;; Could just make this a sensor reading...

    ))

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
    
    (check-tok (Token) Bool)
    ;; Gotta have a way to remove tokens also!
    ;; Not to mention expiration dates!!

    (list Object List)
    (append List List)

    (rfoldwith (Token Function Object Region) Signal)
    ))

;; These count as primitives also.
;; All regiment constants are presumed to be "slow prims" for
;; now. (see add-heartbeats)
(define regiment-constants
  '(
    (world          Constant         Region)
    (anchor         Constant         Anchor)
    ))

;; These are the distributed primitives.  The real Regiment combinators.
;; TODO: REMOVE THIS LIST.  Use the one in hm_type_inferencer
(define regiment-distributed-primitives 
  '(
    
    (rmap           (Function Area) Area)

    (rfold          (Function Object Area) Signal)
    (smap           (Function Signal) Signal)
    
    ;; This joins two signals in the network.
    (smap2          (Function Signal Signal) Signal)

    ;; This is the identity function on regions.  
    ;; However it also lights an LED.
    (light-up (Area) Area)

    (anchor-at      (Number Number) Anchor)
    (anchor-dist    (Anchor Anchor) Number)

    (anchor-optimizing (Function Region) Anchor)

    ;; Takes a function to optimize, and an optional refresh rate to re-elect at:
    ;; If no refresh rate is zero, the election only happens once.
    (anchor-maximizing (Function Number) Anchor)

    (circle         (Anchor Dist)   Region)
    (circle-at      (Number Number Dist) Region)
    (k-neighborhood (Anchor Number) Region)
    ;; Shorthand: 
    (khood          (Anchor Number) Region)
    (khood-at       (Number Number Number) Region)

    ;; This lifts a node value into the Signal monad:
    (node->anchor   (Node) Anchor)

    ;; This one returns a region of regions:
    (rrcluster        (Area) Area)
    (sparsify       (Area) Area)
    (border         (Area) Area)
;    (planarize      (Area) Area)
;    (treeize        (Area) Area)

    (rfilter         (Function Area) Area)
    (runion           (Area Area) Area)
    (rintersect       (Area Area) Area)
    (rrflatten        (Area) Area)

    ;; Prolly not the right type:
    (until          (Event Signal Signal) Signal)
    (runtil          (Event Region Region) Signal)
    (areaWhen       (Event Area) Area)
    ;; The float is a time in seconds.
    (constEvent     (Object Float) Event)

    ;; What was this one supposed to do and what was it's type?
;    (when           (Event Signal) Signal)
    (rwhen-any        (Function Area) Event)
    (swhen-any        (Function Signal) Event)
    (when-percentage (Float Function Area) Event)

;     neighbors 
;    time-of
;    (time (Node) Time)
     ))
  
; [2004.03.31] - I don't know what the system's going to be called so
; I'm using the placeholder "blanko" which I will replace later. 
; OK, replacing with "regiment"
(define regiment-primitives
  (append regiment-basic-primitives
	  regiment-distributed-primitives
	  regiment-constants))

;; [2004.06.24] This is for the regiment primitives:
(define get-primitive-entry
  (lambda (prim)
    (or (assq prim regiment-primitives)
	(assq prim token-machine-primitives)
        (error 'get-primitive-entry
               "no entry for this primitive: ~a" prim))))

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

;; [2004.06.09]<br>  Many of these are actually language forms.  I gotta
;; get this sorted out eventually.
;; 
;; TODO: add some kind of type info.
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

;; Keywords allowed in the restricted token machine language.
(define token-machine-keyword?
  (lambda (x)
    (and (memq x '(quote set! if begin letrec let let-stored)) #t)))

