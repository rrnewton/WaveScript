
;;;; Chez specific stuff to be loaded for compatibility. 
;;;; This file implements each of the necessary non-R4RS functions mentioned in the
;;;; root documentation of the LAML system. Notice that some of the non-standard Scheme functions used
;;;; in LAML already happens to exist in Chez Scheme.

;;;; .author Ryan Newton


;;; Definition of non-R4RS Scheme functions. 
;;; The functions in this section are general purpose functions which happen
;;; not to be in the Scheme standard (R4RS).

(define (system-to-str str)
  (let* ([proc (process str)]
	 [in (car proc)]
	 [out (cadr proc)])
    (let ((p (open-output-string)))
    (let loop ((c (read-char in)))
      (if (eof-object? c)	  
	  (begin 
	    (close-input-port in)
	    (close-output-port out)
	    (get-output-string p))
	  (begin (display c p)
		 (loop (read-char in))))))))
(define (chomp s)
  (cond
   [(string? s)
    (let ((ind (sub1 (string-length s))))
      (if (eq? #\newline (string-ref s ind))
	  (substring s 0 ind)
	  s))]
   [(null? s) '()]
   [(pair? s) (cons (chomp (car s)) (chomp (cdr s)))]
   [else (error 'chomp "bad input: ~s" s)]))

;; Return the current time in seconds since 1970:
(define current-time
  (let ((absolute (string->number (chomp (system-to-str "date +%s"))))
	(syncpoint (quotient (real-time) 1000)))
    (lambda () 
      (+ (- (quotient (real-time) 1000) syncpoint)
	 absolute))))

;; Sort list using the comparison predicate
(define (sort-list list com) (sort com list))

; ---------------------------------------------------------------------------------------------------
; file-exists?, delete-file, copy-file and directory-exists? all exist in MzScheme

(define directory-exists? file-exists?)

;; This only works on unix systems.
;; TODO: need to rename this file to reflect that lesser generality.
(define (copy-file f1 f2)
  (define (system-to-str str)
    (let ([lst (process str)])
      (let ([in (car lst)] [out (cadr lst)])
	(let ((p (open-output-string)))
	  (let loop ((c (read-char in)))
	    (if (eof-object? c)
		(begin 
		  (close-input-port in)
		  (close-output-port out)
		  (get-output-string p))	      
		(begin (display c p)
		       (loop (read-char in)))))))))
  (define (chomp s)
    (cond
     [(string? s)
      (let ((ind (sub1 (string-length s))))
	(if (eq? #\newline (string-ref s ind))
	    (substring s 0 ind)
	    s))]
     [else (error 'chomp "bad input: ~s" s)]))
  (define (shell-expand-string s)
    (chomp (system-to-str (string-append "exec echo " s))))
  ;; This works with ~/ using file-names:
  (system (format "exec cp ~s ~s"
		  (shell-expand-string f1)
		  (shell-expand-string f2)))
  )

(define (directory-list path)
  (error 'directory-list "not implemented in chez scheme yet.")
  )



; ---------------------------------------------------------------------------------------------------

;; Make a new directory, new-dir, in the directory path (first parameter).
;; The parameter in-directory-path ends in a slash.
(define (make-directory-in-directory in-directory-path new-dir)
  (mkdir (string-append in-directory-path new-dir)))

(define eval-cur-env eval)

; ---------------------------------------------------------------------------------------------------


;; Mail sending support: Send an email to a receiver with title and contents.
;; The optional parameter temp-dir gives a temporary directory used for the mail sending; default is "temp/"
(define (mail receiver title contents . temp-dir)
  (define (write-text-file str fn) 
    (with-output-to-file fn (lambda () (display str))))
 (let ((temp-dir-1 (if (null? temp-dir) "temp/" (car temp-dir))))
   (write-text-file contents (string-append temp-dir-1 "temp-mailfile"))
   (system
    (string-append
     "/usr/bin/mail -s " "'" title "' " receiver (string-append "< " temp-dir-1 "temp-mailfile")))))

; -----------------------------------------------------------------------------

;;; LAML specific, context definition functions. 
;;; The functions in this section return and define the activation context of the LAML processor.


;; Return the contextual command line information passed to LAML upon activation.
;; Returns a list of lenght four or #f if no command line activation exists.
;; The first element must be the symbol laml.
;; Element number two must be the laml source file name (witout extension and initial path).
;; Element number three must be a slash terminated directory, in the source file resides.
;; Element number four must be a list of program parameters.
;; This function must be redefined in scheme-system dependent compatibility file.
(define (laml-canonical-command-line)
  (if (and (vector? argv) (>= (vector-length argv) 2))
      (list 'laml 
	    (vector-ref argv 0) (vector-ref argv 1)
	    (if (>= (vector-length argv) 3) (vector-ref argv 2) '()))
      #f))

;; Fake the contextual startup parameters to a specific source file name and a specific startup directory.
;; Both of the parameters must be strings, or the boolean value #f (in case the informations are unknown).
;; Source-file must be a file name without initial path and without extension.
;; start-dir must be an absolute path to a directory ending in a slash /.
;; This function is useful for programmatic startup of LAML.
;; This function must be redefined in scheme-system dependent compatibility file
;; .form (fake-startup-parameters source-file startup-dir [program-parameter-list])
(define (fake-startup-parameters source-file startup-dir . optional-parameter-list)
  (let ((program-parameters (optional-parameter 1 optional-parameter-list '()))
        (a (make-vector 3 #f)))
    (vector-set! a 0 source-file)
    (vector-set! a 1 startup-dir)
    (vector-set! a 2 program-parameters)
    (set! argv a)))
  

;; RRN: This redefines the error primitive so that 
#;
(define-syntax error
  (syntax-rules ()
    [(_ s) (#%error '|| s)]
;    [(_ e1 e2) (let ((x e1) (y e2))
;		 (if (string? x) 
;		     (#%error '|| x y)
;		     (#%error x y)))]
    [(_ s ...) (#%error s ...)]))

(define error
  (case-lambda 
    [(s) (#%error '|| s)]
    [args (apply #%error args)]
    ))

;(error-print-width 1000)

; Case sensitive reading
(case-sensitive #t)

(optimize-level 1)

; --------------------------------------------------------------------------------
;; [2006.01.20]

;; Trying to rewrite some evaluation-order-dependent functions to have
;; more expected evaluation order. 

;; NOTE: non-tail recursive!
(define map 
  (case-lambda 
    [(f ls)
     (let loop ((ls ls))
       (if (null? ls) '()
	   (let ((v (f (car ls))))
	     (cons v (loop (cdr ls))))))]
    [(f . lsts)
     (let loop ([lsts lsts])
       (cond 
	[(#%andmap null? lsts) '()]
	[(#%ormap null? lsts) 
	 (error 'map "ran out of items in some list args but not others: ~a" lsts)]
	[else (let ((v (apply f (#%map car lsts))))
		(cons v (loop (#%map cdr lsts))))]))]))
