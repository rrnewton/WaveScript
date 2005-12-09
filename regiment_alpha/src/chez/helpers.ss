
(module helpers   
	;; Remember to update the plt equivalent when you update this:
	(;; Syntax:
	  for grep mvlet let-match (match-lambda match-lambda-helper)
	  ++ ^ ;; Exponentiation
	  define-id-syntax
	  ;;reg:define-struct ;; Could be define-structure or define-record. ;; Moved to constants.ss
	  apply-ordered
	  
	  ;; For plt compat:
	  foldl
	  let/ec call/ec

	  make-n-list 

	  with-error-handlers with-warning-handler
	  current-error-port 

	  system/echoed system-to-str with-evaled-params 
	  chomp shell-expand-string seconds-since-1970

	  ;; Values:	    
	  id ignore gnuplot histogram display-progress-meter count-nodes
	  string-split periodic-display all-equal?
	  
	  set->hashtab hashtab->list


	  default-unit-tester tester-eq?
	  ;default-unit-tester-retries
	  substring?
	  gobj? 

	  gaussian
	  
	  list-repeat! make-repeats
	  mapi for-eachi diff
	  set? subset? set-equal? list->set set-cons union intersection difference
	  remq-all assq-remove-all list-remove-first list-remove-last! list-remove-after 
	  filter list-index snoc rac rdc last 
	  list-find-position list-remove-before
	  randomize-list randomize-vector insert-between iota disp crit-printf
	  extract-file-extension remove-file-extension file->string string->file file->slist slist->file pad-width round-to
	  graph-map graph-get-connected-component graph-neighbors graph-label-dists ;cyclic? 
	  graph:simple->vertical graph:vertical->simple
	  deep-assq deep-assq-all deep-member? deep-all-matches deep-filter
	  list-get-random unfold-list average clump
	  partition partition-equal split-before
	  myequal?
	  stream? live-stream? stream-empty? stream-car stream-cdr stream-map stream-take 
	  counter-stream stream-append ;random-stream 
	  display-constrained
	  symbol-append 

	  testhelpers testshelpers test-this these-tests

					;reg:all-unit-tests 
	  
					;   (all-except (lib "rutils_generic.ss")
					;               list->set union intersection difference set?
					;               list-head filter list-index snoc rac rdc 
					;               insert-between iota disp)
					;   (all-from (lib "rutils_generic.ss") )
					;   (all-from-except (lib "rutils_generic.ss") 
					;                    list->set union intersection difference set?) 


	  )

;(import (except topsort-module test-this these-tests))
(import scheme)


;; [2005.11.26] Moved reg:define-struct to chez/constants.ss

;; This doesn't seem to work in PLT.  Besides, let-values is a perfect
;; substitute.  That's the kind of thing I'd like my
;; scheme-meta-language/package-manager to do for me!!

;;; multiple-value let
(define-syntax mvlet
  (lambda (x)
    (define domvlet
      (lambda (bindings ids tmps body)
        (if (null? bindings)
            `((,#'lambda ,ids ,@body) ,@tmps)
            (syntax-case (car bindings) ()
              [(*ids expr)
               (with-syntax ([*tmps (generate-temporaries #'*ids)])
                 (with-syntax ([body (domvlet (cdr bindings)
                                              (append #'*ids ids)
                                              (append #'*tmps tmps)
                                              body)])
                   #'(call-with-values
                       (lambda () expr)
                       (lambda *tmps body))))]))))
    (syntax-case x ()
      [(_ (((id ...) expr) ...) form ...)
       (andmap (lambda (ls) (andmap identifier? ls))
               #'((id ...) ...))
       (domvlet #'(((id ...) expr) ...) '() '() #'(form ...))])))


(define-syntax let/ec
  (syntax-rules ()
    [(_ v exp ...)
     (call/1cc (lambda (v) exp ...))]))
(alias call/ec call/1cc)

;(define-syntax define-values
;  (syntax-rules ()
;    [(_ (v ...) exp)
;     (begin 
;       (call-with-values
		

 ;; [2004.06.13] Matches the function defined in plt, provides
 ;; functionality used by the generic code.
 ;; The escape handler had *better* escape.
 (define (with-error-handlers display escape th)
   (parameterize ([#%error-handler (lambda args 
				   (apply display args)
				   (escape))])
		 (th)))
(define (with-warning-handler fun th)
  (parameterize ([#%warning-handler fun])
    (th)))


;; This is too lenient, but there's no other option.
(define promise? procedure?)


;; This is a hack, it's not safe because it can report false
;; positives.  This is used to tell when something is a graphics
;; screen object as constructed with SWL's (make <foo> ...):
(define gobj?
  (lambda (x)
    (and (vector? x)
	 (> (vector-length x) 2)
	 (procedure? (vector-ref x 0))
	 (vector? (vector-ref x 1))
	 (> (vector-length (vector-ref x 1)) 1)
	 (eq? 'class (vector-ref (vector-ref x 1) 0)))))


#;(define stderr
  (let ((buffer-size 1))
    (let ((p (make-output-port 2 (make-string buffer-size))))
      (set-port-output-size! p (- buffer-size 1))
      p)))
(define current-error-port (make-parameter stderr))


;; Moved include!
; ======================================================================
;; We play nasty tricks with symbolic links here. 
;; It doesn't matter if we load this file from "src" or "src/chez"
;; because we've linked the "generic" subdir from both locations.
(include "generic/helpers.ss")
; ======================================================================


(define (crit-printf . args)
  (critical-section (apply printf args)))


(define make-n-list
  (lambda (n func)
    (letrec ((loop (lambda (n acc)
                     (if (zero? n)
                         acc
                         (loop (sub1 n) (cons (func n) acc))))))
      (loop n '()))))


;; From PLT's list.ss
  (define foldl
     (letrec ((fold-one
               (lambda (f init l)
                 (letrec ((helper
                           (lambda (init l)
                             (cond
			       [(null? l) init]
			       [else (helper (f (car l) init) (cdr l))]))))
                   (helper init l))))
              (fold-n
               (lambda (f init  l)
                 (cond
		   [(ormap null? l)
		    (if (andmap null? l) 
			init
			(error 'foldl "received non-equal length input lists"))]
		   [else (fold-n
			  f
			  (apply f (mapadd car l init))
			  (map cdr l))]))))
       (case-lambda
        [(f init l) (fold-one f init l)]
        [(f init l . ls) (fold-n f init (cons l ls))])))



;; [2005.11.14] This is like chez's system command, except it routes
;; the output through the standard scheme output port.  This way it
;; can be captured/redirected by the normal means.
(define (system/echoed str)
  (let-match ([(,in ,out ,id) (process str)])
    (let loop ((line (read-line in)))
      (unless (or (not line) (eof-object? line))
	(display line)(newline)
	(loop (read-line in))))
    ;; Alas, I don't know how to get the error-code through "process",
    ;; so we must assume there was no error:
    0))
;; Example: compare (with-output-to-string (lambda () (system "ls")))
;;               to (with-output-to-string (lambda () (system/echoed "ls")))

;; [2005.11.17] This one is similar 
(define (system-to-str str)
  (let-match ([(,in ,out ,id) (process str)])
    (let ((p (open-output-string)))
    (let loop ((c (read-char in)))
      (if (eof-object? c)	  
	  (begin 
	    (close-input-port in)
	    (close-output-port out)
	    (get-output-string p))
	  (begin (display c p)
		 (loop (read-char in))))))))

;; [2005.11.17] This is reminescent of the perl command "chomp" 
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

(define (shell-expand-string s)
  (chomp (system-to-str (string-append "exec echo " s))))

;; Return the current time in seconds since 1970:
(define seconds-since-1970
  (lambda ()
    (let ((absolute (string->number (chomp (system-to-str "date +%s"))))
	  (syncpoint (quotient (real-time) 1000)))
      (lambda () 
	(+ (- (quotient (real-time) 1000) syncpoint)
	   absolute)))))

)

