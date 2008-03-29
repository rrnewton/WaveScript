
(module chez_compat mzscheme 
  (require   
;   "../generic/constants.ss"
   "../plt/iu-match.ss"
   "cheztrace.ss"
   "identifier-syntax.ss"
   (lib "date.ss")
   (lib "math.ss")
   (lib "pretty.ss")
   (prefix plt: (lib "list.ss"))
   (prefix plt: (lib "process.ss"))
   (all-except (lib "compat.ss") atom?)
;   "../generic/reg_macros.ss"
   ;          (all-except (lib "list.ss") filter)
   
   ;           "../generic/constants.ss"
   ;           "hashtab.ss"
   ;           "engine.ss"
   (prefix swindle: (lib "misc.ss" "swindle"))
   (only (lib "43.ss" "srfi") vector-copy)
   ;(prefix plt: mzscheme)
   )

  (require-for-syntax "identifier-syntax.ss")

  (provide (all-defined)
	   (all-from "cheztrace.ss")
           vector-copy
           pretty-print flush-output-port
           real-time
           (rename plt:remq remq)
           identifier-syntax
           )

  ;; Here we include the SLIB initialization directly.  This is the only 
  ;; way I can make slib play nice with the PLT module system at all.
;  (load (build-path (collection-path "slibinit") "init.ss"))
;  (include "/usr/local/plt/collects/slibinit/init.ss")  

  ;;; Chez Compatability

  (define call/1cc call/ec)
  
  (define cd current-directory) ;; shorthand

  ;; Leaf nodes in a data structure.  Things that contain no more subthings.
  ;(define (atom? x) (or (symbol? x) (number? x) (null? x) (boolean? x) (char? x) (string? x)))
  ;; No, this is the correct definition of atom?
  (define (atom? x) (not (pair? x)))
  ;; Should ports be included?

;  (define real-time current-milliseconds)

  ;; Chez functions for access to top level environment.
  (define (define-top-level-value sym obj)
    (eval `(define ,sym ',obj)))
  (define (set-top-level-value! sym obj) ;; ditto
    (eval `(set! ,sym ',obj)))
  (define (top-level-value sym) (eval sym)) ;; ditto
  (define (top-level-bound? sym)
    (call/ec (lambda (out)
	       (namespace-variable-value sym #f (lambda () (out #f)))
	       #t)))
  
;  (define record? struct?)
  ;(define flush-output-port flush-output)
  ;; [2005.11.05] HACK: This gets set to the normal console output, and stays that way:
  (define console-output-port (make-parameter (current-output-port)))
  (define console-input-port  (make-parameter (current-input-port)))
  ;(define pp pretty-print)
  (define cpu-time current-process-milliseconds)   ;; Same param, diff name
  (define print-level pretty-print-depth)          ;; Same param, diff name
  (define pretty-line-length pretty-print-columns) ;; Same param, diff name
  
  ;; There's no interactive inspector for PLT:
  (define (inspector-error-handler . args) (apply error args))
  
  ;(define print-length pretty-print-length)
  ;; Can't adjust the length from PLT:
  ;; So this does nothing:
  (define print-length (make-parameter #f))
  ;; There is also no print-gensym in PLT:
  (define print-gensym (make-parameter #f))

  ;; This is defined natively in PLT 370
  #;
  (define (integer-length integer)
    (inexact->exact
     (ceiling (/ (log (if (negative? integer)
			 (- integer)
			 (+ 1 integer)))
		(log 2)))))

  ;; We don't have fixnum/flonum arithmetic in PLT:
  ;; Tried to make a generic "alias", but that didn't work, so here are a bunch of defines.
  (define-syntax fxzero? (identifier-syntax zero?))
  (define-syntax fx+ (identifier-syntax +))
  (define-syntax fx- (identifier-syntax -))
  (define-syntax fx* (identifier-syntax *))
  (define-syntax fx/ (identifier-syntax /))
  (define-syntax fx= (identifier-syntax =))
  (define-syntax fx< (identifier-syntax <))
  (define-syntax fx> (identifier-syntax >))
  (define-syntax fx<= (identifier-syntax <=))
  (define-syntax fx>= (identifier-syntax >=))
  (define-syntax fxmin (identifier-syntax min))
  (define-syntax fxmax (identifier-syntax max))
  (define-syntax fxabs (identifier-syntax abs))

  (define-syntax fl+ (identifier-syntax +))
  (define-syntax fl- (identifier-syntax -))
  (define-syntax fl* (identifier-syntax *))
  (define-syntax fl/ (identifier-syntax /))
  (define-syntax fl= (identifier-syntax =))
  (define-syntax fl< (identifier-syntax <))
  (define-syntax fl> (identifier-syntax >))
  (define-syntax fl<= (identifier-syntax <=))
  (define-syntax fl>= (identifier-syntax >=))
  (define-syntax flabs (identifier-syntax abs))

  (define (fl-make-rectangular x y) (+ x (* y 0+1i)))
  ;(define (cfl-conjugate x) (+ x (* 0+i (* -2 (imag-part x)))))
  (define-syntax cfl-conjugate (identifier-syntax conjugate))

  (define-syntax cfl-imag-part (identifier-syntax imag-part))
  (define-syntax cfl-real-part (identifier-syntax real-part))

  (define-syntax cfl+ (identifier-syntax +))
  (define-syntax cfl- (identifier-syntax -))
  (define-syntax cfl* (identifier-syntax *))
  (define-syntax cfl/ (identifier-syntax /))
  (define-syntax cfl= (identifier-syntax =))
  (define-syntax cfl< (identifier-syntax <))
  (define-syntax cfl> (identifier-syntax >))
  (define-syntax cfl<= (identifier-syntax <=))
  (define-syntax cfl>= (identifier-syntax >=))
  
  (define-syntax fxlogand (identifier-syntax bitwise-and))
  (define-syntax fxlogor (identifier-syntax bitwise-or))
  (define-syntax fxmodulo (identifier-syntax modulo))
  (define-syntax fxquotient (identifier-syntax quotient))
  (define-syntax fxremainder (identifier-syntax remainder))

;   (define-syntax fxzero? (syntax-rules () [(_ e ...) (zero? e ...)]))
;   (define-syntax fx+ (syntax-rules () [(_ e ...) (+ e ...)]))
;   (define-syntax fx- (syntax-rules () [(_ e ...) (- e ...)]))
;   (define-syntax fx* (syntax-rules () [(_ e ...) (* e ...)]))
;   (define-syntax fx/ (syntax-rules () [(_ e ...) (/ e ...)]))
;   (define-syntax fx= (syntax-rules () [(_ e ...) (= e ...)]))
;   (define-syntax fx< (syntax-rules () [(_ e ...) (< e ...)]))
;   (define-syntax fx> (syntax-rules () [(_ e ...) (> e ...)]))
;   (define-syntax fx<= (syntax-rules () [(_ e ...) (<= e ...)]))
;   (define-syntax fx>= (syntax-rules () [(_ e ...) (>= e ...)]))
;   (define-syntax fxmin (syntax-rules () [(_ e ...) (min e ...)]))
;   (define-syntax fxmax (syntax-rules () [(_ e ...) (max e ...)]))
;   (define-syntax fl+ (syntax-rules () [(_ e ...) (+ e ...)]))
;   (define-syntax fl- (syntax-rules () [(_ e ...) (- e ...)]))
;   (define-syntax fl* (syntax-rules () [(_ e ...) (* e ...)]))
;   (define-syntax fl/ (syntax-rules () [(_ e ...) (/ e ...)]))
;   (define-syntax fl= (syntax-rules () [(_ e ...) (= e ...)]))
;   (define-syntax fl< (syntax-rules () [(_ e ...) (< e ...)]))
;   (define-syntax fl> (syntax-rules () [(_ e ...) (> e ...)]))
;   (define-syntax fl<= (syntax-rules () [(_ e ...) (<= e ...)]))
;   (define-syntax fl>= (syntax-rules () [(_ e ...) (>= e ...)]))

  (define (fxsrl n bits) (quotient n (expt 2 bits)))
  (define (fxsll n bits) (* n (expt 2 bits)))

  (define-syntax lognot (identifier-syntax bitwise-not))
  (define-syntax logor  (identifier-syntax bitwise-ior))
  (define-syntax logand (identifier-syntax bitwise-and))
  (define-syntax ash (identifier-syntax arithmetic-shift))

  (define (logbit? ind n) (not (fxzero? (logand 1 (ash n (- ind))))))

  (define (logbit1 ind n) 
    (if (logbit? ind n) n (+ (ash 1 ind) n)))

  (define fxlogbit? logbit?)
  
  (define (cflonum? n) (and (number? n) (inexact? n) (not (eqv? 0 (imag-part n)))))
  
  (define-syntax datum
    (syntax-rules ()
      ((_ t) (syntax-object->datum (syntax t)))))
  
  ;; [2005.11.03] This is lame but apparently PLT doesn't have any such thing.
  ;; Further, this might be dangerous.  Chez promises that fixnum's are eq?
  ;; but I haven't found a place in the PLT documentation where they guarantee that.
  (define most-positive-fixnum
    (let ((x (- (expt 2 30) 1)))  ;; HACK.
      (lambda () x)))
  (define fixnum?
    (let ((x (expt 2 30)))
      (lambda (n) 
	;(printf "PLT fixnum: ~s -> ~s ~s ~s ~s\n" n (integer? n) (< n x) `(< ,n ,x) (eval `(< ,n ,x)))
	;; [2008.03.28] NASTY PLT BUG IN 372 ON UBUNTU:
	;(and (integer? n) (< n x))
	;; [2008.03.28] THIS HAPPENS TO WORK:
	(and (integer? n) (<= n (sub1 x)))
	)))
  
  (define (flonum? x) (and (number? x) (inexact? x) (real? x)))
  (define (flonum->fixnum x) (inexact->exact (floor x)))
  (define (fixnum->flonum x) (exact->inexact x))
    
  (define (list-copy l) (reverse! (reverse l))) ;; Reverse had better be tail-recursive!
   #;(define (remq x ls)
    (cond 
      [(null? ls) ls]
      [(eq? x (car ls)) (remq x (cdr ls))]
      [else (cons (car ls) (remq x (cdr ls)))]))

  
  (define (date-and-time)
    (let ((d (seconds->date (current-seconds))))
      (format "~a, ~a:~a:~a" (date->string d) (date-hour d) (date-minute d) (date-second d))))
  
;; Temporary! < FIXME>:
  (define crit-printf printf) ;; Thread safe, critical section printf.

  ;; [2007.01.23] Changing this to catch all exceptions.
  (define with-error-handlers 
    (let ([orig (uncaught-exception-handler)])
    (lambda (displayproc escape th)
    (let/ec out
      (parameterize ([uncaught-exception-handler orig]
		     [error-display-handler 
                      ;(lambda (ob s) (printf "Error ~s in context ~s\n" s ob))
                      displayproc]
                     [error-escape-handler 
                      (lambda args
		      (let ((result (apply escape args)))
			;; If the escape procedure is not called, we must destroy the continuation:
			(out result)))])
        (th))))))

  ;; Chez's system for warnings -- same as error.
  (define (warning sym . args)
    (fprintf (current-error-port) "Warning ~s: ~a \n" sym (apply format args)))
  (define (with-warning-handler fun th)
    (fluid-let ((warning fun))
      (th)))
  
  (define (syntax-error obj . strings)
    (raise-syntax-error #f (apply string-append strings) obj))

  ;; Make this return a fake error code to act like the Chez version.
  (define system plt:system/exit-code)
  ;; There's no problem with this in PLT:
  (define system/echoed plt:system/exit-code)

  (define (system-to-str str)
    (match (plt:process str)
      [(,in ,out ,id ,err ,fun)
       (let ((p (open-output-string)))
         (let loop ((c (read-char in)))
           (if (and (eof-object? c)
                    (not (eq? 'running (fun 'status))))
               (begin 
                 (close-input-port in)
                 (close-output-port out)
                 (close-input-port err)
                 (get-output-string p))
               (begin (unless (eof-object? c) (display c p))
                      (loop (read-char in))))))]))
  
  ;; PLT doesn't support anything like fasl-writing as far as I can see.
  (define fasl-write write)
  
  ;; This matches the chez parameter, but does nothing.
  (define pretty-maximum-lines (make-parameter #f))

  ;; Primitive in chez:
  (define (make-list n x)
    (let loop ((acc ()) (n n))
      (if (zero? n) acc
          (loop (cons x acc) (sub1 n)))))
  (define list-head
    (lambda (lst n)
      (cond
        [(zero? n) '()]
        [(null? lst) (error 'list-head "list is not long enough: ~s ~s"
                            lst n)]
        [else (cons (car lst) (list-head (cdr lst) (sub1 n)))])))

  ;; [2005.11.03] This will work for our purposes, but should stick in an actual definition here at some point.  
  ;(define (merge! p a b) (swindle:merge! a b p))

  ;; The list.ss version is reversed from the Chez version:
  (define (sort fun ls) (plt:sort ls fun))
  (define (sort! fun ls) (plt:sort! ls fun))
  
;; From Swindle:
;;>> (merge less? a b)
;;>   Takes two lists `a' and `b' such that both (sorted? a less?) and
;;>   (sorted? b less?) are true, and returns a new list in which the
;;>   elements of `a' and `b' have been stably interleaved so that (sorted?
;;>   (merge less? a b) less?) is true.  Note: this does not accept vectors.
'(define (merge less? a b)
  (cond [(null? a) b]
        [(null? b) a]
        [else (let loop ([x (car a)] [a (cdr a)] [y (car b)] [b (cdr b)])
                ;; The loop handles the merging of non-empty lists.  It has
                ;; been written this way to save testing and car/cdring.
                (if (less? y x)
                  (if (null? b)
                    (cons y (cons x a))
                    (cons y (loop x a (car b) (cdr b))))
                  ;; x <= y
                  (if (null? a)
                    (cons x (cons y b))
                    (cons x (loop (car a) (cdr a) y b)))))]))

  (define merge
    (lambda (pred? l1 l2)
      (cond
        ((null? l1) l2)
        ((null? l2) l1)
        ((pred? (car l2) (car l1))
         (cons (car l2) (merge pred? l1 (cdr l2))))
        (else (cons (car l1) (merge pred? (cdr l1) l2))))))

  (define merge! merge)
  
  ;; More Chez compat:

  ;; This is TERRIBLY inefficient.  It has to convert to thy byte-string representation.
  (define block-read
    (let ([bytes (make-bytes 100)])
      (lambda (inp str count)
	(if (> (string-length str) (bytes-length bytes))
	    (set! bytes (make-bytes (string-length str))))
	(let ([rd (read-bytes-avail! bytes inp 0 count)])
;	  (printf "LENGTH: str:~a bytes:~a rd:~a\n" (string-length str) (bytes-length bytes) rd)
	  (cond
	   [(and (number? rd) (> rd (string-length str)))
	    (error 'block-read "buffer not big enough")]
	   [(eof-object? rd) rd]
	   [else ;; Now copy back to the string:
	    (do ([i 0 (+ i 1)])
		((= i rd) rd)
	      ;(printf "  i:~a\n" i)
	      (string-set! str i (integer->char (bytes-ref bytes i)))

	      #;
	      (let ([byt (bytes-utf-8-ref bytes i)])		
		(if byt 
		    (string-set! str i byt)
		    (error 'block-read 
			   "went off the end of the buffer at position: ~a, buflen:~a contents:~a\n" 
			   i (bytes-length bytes) (bytes-ref bytes i)))))]
	   )))))


  (define (block-write outp str count)
    (write-string str outp 0 count))
  (define collect collect-garbage)
              
           

;; HERE: Include base helpers file...

  
) ;; End module.


;(require chez_compat) (apply fxzero? '(1)) (fxzero? 0)
