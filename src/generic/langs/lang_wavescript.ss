;; [2006.07.22]

;; This language definition implements WaveScript, with its Sigsegs and all.
;; Uses the stream-processing prims from helpers.ss

;; TODO: Make the core language bindings into a module which is
;; imported when eval happens.  This way even petite with a limited
;; heap will still get performance.

;; This has become *terribly* non-portable.

(module lang_wavescript mzscheme
  (require "../../plt/common.ss")
  (provide default-marmotfile )
  (chezimports )
  
  ;; Provide for PLT only, in Chez it goes to top-level.
  (IFCHEZ (begin) (provide wavescript-language))
  
;;======================================================================

;; Testing file IO on marmot audio traces:

;; This just checks some hard coded locations for the marmot file.
(define (default-marmotfile)
  (let ([file (cond
	       [(file-exists? "/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw")
		"/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw"]
	       [(file-exists? "~/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw")
		"~/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw"]
	       [(file-exists? "/tmp/100.raw")
		"/tmp/100.raw"]
	       [else (error 'default-marmotfile "couldn't find marmot data")])])
    (printf "Reading marmot datafile: ~s\n" file)
    file))

;;; Some TESTS of the reader:	
;;; Commenting out because the PLT-reader can't handle the hash syntax:
;------------------------------------------------------------
#|
;; Takes 35 seconds using stupid approach (read-char).
(define (read-all)
  (wavescript-language)
  (time 
  (let loop ((s (audio 0 1024 0)))
    ;(if (= 0 (remainder n 100)) (display #\.))
;    (display #\.)  
;    (printf "~a\n" (stream-car s))
;    (ASSERT (= 1000 (vector-length (stream-car s))))
    (if (stream-empty? s) 'DONE
	(loop (stream-cdr s) ;(add1 n)
	      )))))
(define (read-n n)
  (wavescript-language)
  (time 
  (let loop ((s (audio 0 1024 0)) (n n))
    ;(if (= 0 (remainder n 100)) (display #\.))
;    (display #\.)  
;    (printf "~a\n" (stream-car s))
;    (ASSERT (= 1000 (vector-length (stream-car s))))
    (if (fxzero? n) 'DONE
	(loop (stream-cdr s) (fx- n 1)
	      )))))
;; Takes 3.3 seconds.
(define (baseline-read-all)
  (let ((p (default-marmotfile)))
     (time 
      (let loop ()
	(let ((c (#3%read-char p)))
	  (if (#3%eof-object? c)
	      'alldone
	      (loop)))))
     (close-input-port p)))
;; Takes 350-430 ms (depending on optimize-level) to load 315mb on faith.
;; (That must be using some disk caching, eh?)
;; Note: changing block size:
;;   10 - 17 sec
;;   100 - 2 sec
;;   1024 - 350 ms
;;   2048 - 260 ms
;;   16384 - 210-270 ms
;;   32768 - 180 ms
;;   65536 - 180-250 ms
(define (baseline-read-all-block)
  (define chunk 8000)
   (let ((p (open-input-file (default-marmotfile)))
	 (s (make-string chunk #\_)))
     (time 
      (let loop ()
	(let ((n (#3%block-read p s chunk)))
	  ;(printf "~a " n)
	  (if (#3%eof-object? n)
	      'alldone ;(printf "done: ~a\n" s)
	      (loop)))))
     (close-input-port p)))|#
;------------------------------------------------------------

(define-syntax define-inlined
  (syntax-rules ()
    [(_ (f x ...) e ...)
     (define-syntax f 
       (syntax-rules ()
	 [(_ x ...) e ...]))]))

;; ======================================================================

;; This uses a convoluted evaluation order.  But it allows us to eval
;; the wavescope defs ONCE at load time, and have them visible to all
;; the unit tests
#;
(define these-tests
  (let ([inlanguage (lambda (e)
                      (IFCHEZ (eval `(let () (import wavescript_sim_library) ,e))
                              (eval `(module temp-module mzscheme 
                                       (require "../sim/wavescript_sim_library.ss")  
                                       ,e))))])
    (inlanguage '(let ([listprim list])
	   ;,(wavescript-language 'return)
	   ;(import wavescript_sim_library)
	   ;(import (except mod_scheme break length + - * / ^ inspect letrec import))
	   ;(import mod_constants)
	   ;(import mod_helpers)
	   ;;(define nulltimebase 'nulltimebase)
	   `(
	    ["Joinsegs" 
	      (,(lambda ()
		  (reg:struct->list
		   (joinsegs (make-sigseg 10 19 (list->vector (iota 10)) nulltimebase)
			     (make-sigseg 15 24 (list->vector (map (lambda (x) (+ x 5)) (iota 10))) nulltimebase)))))
	      ("sigseg" 10 24 #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14) ,nulltimebase)]

	    ["Subseg"
	      (,(lambda ()
		  (reg:struct->list
		   (subseg (make-sigseg 10 19 (list->vector (iota 10)) nulltimebase) 11 5))))
	      ("sigseg" 11 15 #(1 2 3 4 5) ,nulltimebase)]

	    ["seg-get"
	      (,(lambda ()	
		  (seg-get (make-sigseg 10 19 (list->vector (iota 10)) nulltimebase) 12)))
	      2]

	    ["width/start/end"
	      (,(lambda ()	
		  (let ([seg (make-sigseg 11 20 (list->vector (iota 10)) nulltimebase)])
		    (list (width seg) (start seg) (end seg)))))
	      (10 11 20)]

	    ["audioFile"
	      (,(lambda ()	
		  (let* ([stream (audioFile (string-append (REGIMENTD) "/demos/wavescope/countup.raw")
					    1024 0)]
			 [first (stream-car stream)]
			 [second (stream-car (stream-cdr stream))])
		    (list (width first) (start first) (end first)
			  (width second) (start second) (end second)))))
	      (1024 0 1023
	       1024 1024 2047)]

	    ["for loop"
	      (,(lambda ()
		  (let ([sum 0])
		    ;; This outer loop goes 10 times.
		    (for (i 1 20)
			(set! sum (+ sum (* 1000 i)))
		      ;; This inner loop goes 10 times for each outer iteration (100 total)
		      (for (i 21 40)
			  (set! sum (+ sum 1))
			(if (= i 30) (break))
			)
		      (if (= i 10) (break)))
		    sum)
		  ))
	      55100]

	    
	    )))))

#;
(define these-tests
  (eval `(let ()
	   ,(wavescript-language 'return)
	   ;(define nulltimebase 'nulltimebase)
	   
	   (list 
	    `["Joinsegs" 
	      (,(lambda ()
		  (reg:struct->list
		   (joinsegs (make-sigseg 10 19 (list->vector (iota 10)) nulltimebase)
			     (make-sigseg 15 24 (list->vector (map (lambda (x) (+ x 5)) (iota 10))) nulltimebase)))))
	      ("sigseg" 10 24 #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14) ,nulltimebase)]
	    `["Subseg"
	      (,(lambda ()
		  (reg:struct->list
		   (subseg (make-sigseg 10 19 (list->vector (iota 10)) nulltimebase) 11 5))))
	      ("sigseg" 11 15 #(1 2 3 4 5) ,nulltimebase)]

	    `["seg-get"
	      (,(lambda ()	
		  (seg-get (make-sigseg 10 19 (list->vector (iota 10)) nulltimebase) 12)))
	      2]

	    `["width/start/end"
	      (,(lambda ()	
		  (let ([seg (make-sigseg 11 20 (list->vector (iota 10)) nulltimebase)])
		    (list (width seg) (start seg) (end seg)))))
	      (10 11 20)]

	    `["audioFile"
	      (,(lambda ()	
		  (let* ([stream (audioFile (string-append (REGIMENTD) "/demos/wavescope/countup.raw")
					    1024 0)]
			 [first (stream-car stream)]
			 [second (stream-car (stream-cdr stream))])
		    (list (width first) (start first) (end first)
			  (width second) (start second) (end second)))))
	      (1024 0 1023
	       1024 1024 2047)]

	    `["for loop"
	      (let ([sum 0])
		;; This outer loop goes 10 times.
		(for (i 1 20)
		    (set! sum (+ sum (* 1000 i)))
		  ;; This inner loop goes 10 times for each outer iteration (100 total)
		  (for (i 21 40)
		      (set! sum (+ sum 1))
		    (if (= i 30) (break))
		    )
		  (if (= i 10) (break)))
		sum)
	      55100]
	    
	    ))))

  (IFCHEZ 
 ;; For CHEZ this becomes a top-level binding:
 (define-language
   'wavescript-language
   `(begin
     ;; We only import these basic bindings to keep us honest.
     (import-only wavescript_sim_library)
     ;; Then we import some "sub-modules" exported by the language-module.
     ;; This is everything but the overriden bindings from default scheme language:
     (import (except mod_scheme break length + - * / ^ inspect letrec import let))
     (import mod_constants)
     (import mod_helpers)
 
     ;(eval-when (compile eval load) (printf "TRYING..\n"))
;     (printf "TRYING..\n")
;     (inspect list)

     ;; A safety mechanism:
#;
     ,@(IFDEBUG
       '((let ([rawconstructor make-sigseg])	   
	   (set! make-sigseg (lambda (start end vec timebase)
			       (ASSERT (integer? start))
			       (ASSERT (integer? end))
			       (ASSERT (vector? vec))
			       (ASSERT (eq? timebase nulltimebase))
			       (rawconstructor start end vec timebase)))
	   ))
       '())
     
     ))

;; PLT Version:
(define (wavescript-language expr)
  (eval `(begin 
	   (current-directory (REGIMENTD))
	   (current-directory "src/")
#;
	   (module temp mzscheme
	     (require "generic/sim/wavescript_sim_library.ss")
	     ,expr
	     )
           ;; Fighing with PLT's module system.  I don't know how to over-write mzscheme 
           ;; bindings (like letrec) except at top-level.  Here we mangle the top-level then try to un-mangle it.           
           (require "generic/sim/wavescript_sim_library.ss")
           (define THISWSVAL ,expr)
           (require mzscheme)
           THISWSVAL
           ))
  )
)

) ; End module.

;(require lang_wavescript)

; ======================================================================
;; SCRATCH

;; UNFINISHED VERSION: Doing this with different sized grab-chunk and window-size is very annoying:
#|
       (define (read-window)
	 (let ([win (make-vector len)])	       
	   (let loop ()
	      (cond
	      [remainder 
	       (ASSERT (fxzero? ind1))
	       (if (fx>= (fx+ (string-length remainder) count1) winsize)
		   (begin
		     ;; TODO FINISH:
		     'notdone
		     )
		   (error 'read-window "dammit, block-read just didn't read enough"))
	       ]

	      ;; We're chugging through buffer1 and there's still enough left:
	      [(fx>= (fx- count1 ind1) winsize)
	       (for (i 0 (fx- len 1))
		   (vector-set! win i (read-sample buffer1 (fx+ ind1 (fx* i 8)))))
	       (set! ind1 (fx+ ind1 winsize))
	       win]

	      ;; There's some left, but not enough.
	      [(fx< ind1 count1)
	       ;; Push this leftover to the remainder:
	       (printf "Leftover! ~a\n" (- count1 ind1))
	       (set! remainder (substring buffer1 ind1 count1))
	       ;; Load up some new input:
	       (set! count1 (block-read infile buffer1 chunksize))
	       (set! ind1 0)
 	       (if (eof-object? count1)
		   #f (loop))]

	      ;; We precisely used up everything, reload and loop.
	      [(fx= ind1 count1)
	       (set! count1 (block-read infile buffer1 chunksize))
	       (set! ind1 0)
	       (if (eof-object? count1)
		   #f (loop))
	       ]
	      ))))
|#

