
(chez:module helpers   
	;; Remember to update the plt equivalent when you update this:
	(;; Syntax:
	  
	  ;; For plt compat:
	  foldl
	  let/ec call/ec
	  define-values

	  make-n-list 

	  with-error-handlers with-warning-handler
	  current-error-port 

	  system/echoed system-to-str with-evaled-params add-parameter-hook
	  chomp shell-expand-string seconds-since-1970

	  ;; Values:	    
	  id ignore gnuplot histogram grep-oblist comma-number
	  display-progress-meter progress-dots runN count-nodes
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
	  setq? subsetq? set-eq?
	  remq-all assq-remove-all list-remove-first list-remove-last! list-remove-after 
	  filter list-index snoc rac rdc last 
	  list-find-position list-remove-before
	  vector-for-each vector-map vector-map!


	  insert-between iota compose compose/values disp crit-printf
	  extract-file-extension remove-file-extension
	  file->string string->file file->slist port->slist slist->file file->linelists
	  read-line

	  pad-width round-to uppercase lowercase symbol-uppercase symbol-lowercase
	  graph-map graph-get-connected-component graph-neighbors graph-label-dists ;cyclic? 
	  graph:simple->vertical graph:vertical->simple
	  deep-assq deep-assq-all deep-member? deep-all-matches deep-filter
	   unfold-list average median stddev clump
	  partition partition-equal split-before
	  myequal?

	  stream? live-stream? stream-empty? stream-cons stream-car stream-cdr
	  stream-map stream-filter stream-take stream-take-all 
	  iota-stream stream-append browse-stream ;random-stream 
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
;(import (except scheme atom?))
(import scheme)

;; [2005.11.26] Moved reg:define-struct to chez/constants.ss

;; This doesn't seem to work in PLT.  Besides, let-values is a perfect
;; substitute.  That's the kind of thing I'd like my
;; scheme-meta-language/package-manager to do for me!!


(define-syntax let/ec
  (syntax-rules ()
    [(_ v exp ...)
     (call/1cc (lambda (v) exp ...))]))
;(alias call/ec call/1cc)
(define call/ec call/1cc)

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
(include "../generic/helpers.ss")
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

;; Lifted from schemewiki:
#;(define-syntax define-values 
   (syntax-rules () 
     ((define-values () exp) 
      (call-with-values (lambda () exp) (lambda () 'unspecified))) 
     ((define-values (var . vars) exp) 
      (begin  
        (define var (call-with-values (lambda () exp) list)) 
        (define-values vars (apply values (cdr var))) 
        (define var (car var)))) 
     ((define-values var exp) 
      (define var (call-with-values (lambda () exp) list)))))
;; My version for chez:
(define-syntax define-values 
  (lambda (x)
    (define iota 
      (case-lambda [(n) (iota 0 n)]
		   [(i n) (if (= n 0) '() (cons i (iota (+ i 1) (- n 1))))]))
    (syntax-case x ()  
     [(define-values (vars ...) exp)
      (with-syntax ([(nums ...) (datum->syntax-object  
				 #'define-values 
				 (iota (length (datum (vars ...)))))])
	#'(begin  
	    (define newtempvar (call-with-values (lambda () exp) vector))
	    (define vars (vector-ref newtempvar nums))
	    ...))])))


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


;; [2006.02.20] Copying here from my other utility files.
;; This is a very useful little utility that allows you to search
;; through all the currently bound top-level symbols.
(define grep-oblist
  ;; Only for unixy-machines
  (lambda (str)
    (let* ([name "___temp___.txt"]
;           [out (open-output-file name 'replace)])
           [out (open-output-file name )])
      (for-each (lambda (o) (display o out) (newline out))
                (oblist))
      (system (format "cat \"~a\" | grep \"~a\"" name str))
      (close-port out)
      (delete-file name)
      (void))))

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



;; Simply reads a line of text.  Embarassing that this isn't in r5rs.
;; Man, they need a standardized set of libraries.
;; .parameter port (optional) Port to read from.
#;
(define read-line
  (case-lambda 
    [() (read-line (current-output-port))]
    [(p) (let loop ((c (read-char p)) (acc '()))
	   (if (or (eof-object? c) (char=? #\newline c))
	       (list->string (reverse! acc))
	       (loop (read-char p) (cons c acc))))]))
;This read-line will handle line delimination either by #\newline
;or by a consecutive #\return #\newline
;JEEZ, this has some problems right now [01.06.08], Chez for windows
;seems to be totally screwy regarding char-ready?.
(define read-line ;returns false if the port is empty
  (lambda args
    (let ([port (if (null? args)
                    (current-input-port)
                    (car args))])
      (letrec ([loop
                 (lambda (c)
                   (cond
                     [(or (eof-object? c)
                          (eq? c #\newline)) '()]
                     [(or (eq? c #\linefeed)
                          (eq? c #\return))
                      (when (and (char-ready? port)
                                 (eq? (peek-char port) #\newline))
                            (read-char) '())]
                     [else (cons c (loop (read-char port)))]))])
        (let ([c (read-char port)])
          (if (eof-object? c) #f
              
              ((lambda (x) x)
               (list->string (loop c)))
              ))))))
;; TODO: write a more efficient version with block-read!!! [2006.02.22]

;; Used for expanding strings with environment variables in them.
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

