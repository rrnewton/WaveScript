
(module chez_compat mzscheme 
  (require 
   
   "../generic/constants.ss"
   "../plt/iu-match.ss"
   (lib "date.ss")
   (lib "pretty.ss")
   (lib "list.ss")
   (prefix plt: (lib "process.ss"))
   (all-except (lib "compat.ss") atom?)
;   "../generic/reg_macros.ss"
   ;          (all-except (lib "list.ss") filter)
   
   ;           "../generic/constants.ss"
   ;           "hashtab.ss"
   ;           "engine.ss"
   (prefix swindle: (lib "misc.ss" "swindle"))
   )

  (provide (all-defined)
           pretty-print remq sort flush-output-port
           real-time
           )

  ;; Here we include the SLIB initialization directly.  This is the only 
  ;; way I can make slib play nice with the PLT module system at all.
;  (load (build-path (collection-path "slibinit") "init.ss"))
;  (include "/usr/local/plt/collects/slibinit/init.ss")  

  ;;; Chez Compatability
    
  (define cd current-directory) ;; shorthand

;; Leaf nodes in a data structure.  Things that contain no more subthings.
(define (atom? x) (or (symbol? x) (number? x) (null? x) (boolean? x) (char? x) (string? x)))
;; Should ports be included?

;  (define real-time current-milliseconds)

  ;; Chez functions for access to top level environment.
  (define (define-top-level-value sym obj)
    (eval `(define ,sym ',obj)))
  (define (set-top-level-value! sym obj) ;; ditto
    (eval `(set! ,sym ',obj)))
  (define (top-level-value sym) (eval sym)) ;; ditto
  (define (top-level-bound? sym) ;; ditto
    (error 'top-level-bound?
	   "This is a chez function which can't ~a"
	     "be emulated right now in Plt. -RRN"))
  
;  (define record? struct?)
  ;(define flush-output-port flush-output)
  ;; [2005.11.05] HACK: This gets set to the normal console output, and stays that way:
  (define console-output-port (make-parameter (current-output-port)))
  (define pp pretty-print)
  (define cpu-time current-process-milliseconds)   ;; Same param, diff name
  (define print-level pretty-print-depth)          ;; Same param, diff name
  (define pretty-line-length pretty-print-columns) ;; Same param, diff name
  
  ;(define print-length pretty-print-length)
  ;; Can't adjust the length from PLT:
  ;; So this does nothing:
  (define print-length (make-parameter #f))
  ;; There is also no print-gensym in PLT:
  (define print-gensym (make-parameter #f))

  ;; We don't have fixnum/flonum arithmetic in PLT:
  ;; Tried to make a generic "alias", but that didn't work, so here are a bunch of defines.
  (define-syntax fx+ (syntax-rules () [(_ e ...) (+ e ...)]))
  (define-syntax fx- (syntax-rules () [(_ e ...) (- e ...)]))
  (define-syntax fx* (syntax-rules () [(_ e ...) (* e ...)]))
  (define-syntax fx/ (syntax-rules () [(_ e ...) (/ e ...)]))
  (define-syntax fx= (syntax-rules () [(_ e ...) (= e ...)]))
  (define-syntax fx< (syntax-rules () [(_ e ...) (< e ...)]))
  (define-syntax fx> (syntax-rules () [(_ e ...) (> e ...)]))
  (define-syntax fx<= (syntax-rules () [(_ e ...) (<= e ...)]))
  (define-syntax fx>= (syntax-rules () [(_ e ...) (>= e ...)]))
  (define-syntax fxmin (syntax-rules () [(_ e ...) (min e ...)]))
  (define-syntax fxmax (syntax-rules () [(_ e ...) (max e ...)]))
  (define-syntax fl+ (syntax-rules () [(_ e ...) (+ e ...)]))
  (define-syntax fl- (syntax-rules () [(_ e ...) (- e ...)]))
  (define-syntax fl* (syntax-rules () [(_ e ...) (* e ...)]))
  (define-syntax fl/ (syntax-rules () [(_ e ...) (/ e ...)]))
  (define-syntax fl= (syntax-rules () [(_ e ...) (= e ...)]))
  (define-syntax fl< (syntax-rules () [(_ e ...) (< e ...)]))
  (define-syntax fl> (syntax-rules () [(_ e ...) (> e ...)]))
  (define-syntax fl<= (syntax-rules () [(_ e ...) (<= e ...)]))
  (define-syntax fl>= (syntax-rules () [(_ e ...) (>= e ...)]))
  ;; [2005.11.03] This is lame but apparently PLT doesn't have any such thing.
  ;; Further, this might be dangerous.  Chez promises that fixnum's are eq?
  ;; but I haven't found a place in the PLT documentation where they guarantee that.
  (define most-positive-fixnum
    (let ((x (- (expt 2 30) 1)))  ;; HACK.
      (lambda () x)))
  (define fixnum?
    (let ((x (expt 2 30)))
      (lambda (n) (and (integer? n) (< n x)))))
  (define (flonum? x) (inexact? x))
  (define (flonum->fixnum x) (inexact->exact (floor x)))
  (define (fixnum->flonum x) (exact->inexact x))
  
  
  ;; [2005.11.03] This will work for our purposes, but should stick in an actual definition here at some point.
  ;(define (merge! a b) (merge a b))
  (define (merge! p a b) (swindle:merge! a b p))
  (define (sort! p l)    (swindle:sort! l p))
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

  ;; This isn't working right now.
  (define (with-error-handlers displayproc escape th)
    (let/ec out
      (parameterize ([error-display-handler 
                      ;(lambda (ob s) (printf "Error ~s in context ~s\n" s ob))
                      displayproc]
                     [error-escape-handler 
                      (lambda args
		      (let ((result (apply escape args)))
			;; If the escape procedure is not called, we must destroy the continuation:
			(out result)))])
        (th))))

  ;; Chez's system for warnings -- same as error.
  (define (warning sym . args)
    (fprintf (current-error-port) "Warning ~s: ~a " sym (apply format args)))
  (define (with-warning-handler fun th)
    (fluid-let ((warning fun))
      (th)))

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
  ;; RRN: Non-tail recursive version
  '(define (merge f l1 l2)
     ;(let loop ((acc '()) (l1 l1) (l2 l2))
     (cond
       [(null? l1) l2]
       [(null? l2) l1]
       [(f (car l1) (car l2))
        ;(loop (cons (car l1) acc)
    (cons (car l1)
	  (merge f (cdr l1) l2))]
       ;; This gives us stability:
   [(f (car l1) (car l2))
    (cons (car l2)
	  (merge f l1 (cdr l2)))]
   ;; If they're equal, l1 goes first:
   [else 
    (cons (car l1)
	  (merge f (cdr l1) l2))]))

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

  ;; More Chez compat:
  (define (block-read inp str count)
    (read-bytes-avail! str inp 0 count))
  (define (block-write outp str count)
    (write-bytes str outp 0 count))
  (define collect collect-garbage)
  
  (define (system-to-string cmd)
    (eval 'TODO-IMPLEMENTM))
            
           

;; HERE: Include base helpers file...

  
) ;; End module.


;(require chez_compat)
