#!r6rs

;; [2006.07.22]

;; This language definition implements WaveScript, with its Sigsegs and all.
;; Uses the stream-processing API defined elsewhere.

;; TODO: Make the core language bindings into a module which is
;; imported when eval happens.  This way even petite with a limited
;; heap will still get performance.

;; This has become *terribly* non-portable.

(library (ws langs lang_wavescript)
  (export ;wavescript-language
           unit-representation
	   ws-show
	   run-wavescript-sim
	   wavescript-language
	   )
  (import (except (rnrs (6)) error) 
	  (rnrs eval)
	  (ws common)
	  (ws compiler_components c_generator))
  
;;======================================================================

;; Testing file IO on marmot audio traces:

;;; Some TESTS of the reader:	
;;; Commenting out because the PLT-reader can't handle the hash syntax:
;------------------------------------------------------------
;;; TAKING OUT these tests after rev 2635, you can go back if you want
;;; to look at them.

;------------------------------------------------------------

(define-syntax define-inlined
  (syntax-rules ()
    [(_ (f x ...) e ...)
     (define-syntax f 
       (syntax-rules ()
	 [(_ x ...) e ...]))]))

;; ======================================================================

;;; This is a generic value printer.
;;; It's used in multiple places.

;; Eventually need to pass a type to this ...
(define (ws-show x) 
  ;; HACK: strings at the top are treated differently as nested strings:
  (if (string? x) x
      (let loop ([x x])
	(cond
	 [(vector? x) (format "#~a" (loop (vector->list x)))]
	 [(tuple? x) (string-append (apply string-append "(" (insert-between ", " (map loop (tuple-fields x)))) ")")]
	 [(list? x) (text->string (list "[" (insert-between ", " (map loop x)) "]"))]
	 [(eqv? x unit-representation) "()"]
	 [(uniontype? x) (format "~a(~a)" (deunique-name (uniontype-tag x)) 
				 (loop (uniontype-val x)))]
	 [(string? x) (string-append "\"" x "\"")]
	 [(double? x) (number->string (double-val x))]
	 [else (format "~a" x)]
	 ))
      ))

;  (define unit-representation #())
(define unit-representation 'UNIT)

;; ======================================================================

;;; This is the actual language binding that evaluates wavescript programs.

;; This functionality should probably be included in 'wavescript-language' itself.
;; But due to limitations in the language-mechanism that I've been usng, that doesn't work presently.
(define run-wavescript-sim 
  (lambda (p)
    ;; New Streams:

    ;; [2007.02.06] Now we wrap it with a little extra to run
    ;; the query.  This is needed as a result of switching over
    ;; to imperative streams.
    
    ;; [2007.07.05] TODO: This means that the "wavescript-language" isn't really complete.
    ;; It SHOULD be self contained, even if that means discarding the existing "language-mechanism.ss"
    (wavescript-language
     ;; We strip binding types because we not all the scheme syntax has been overloaded to expect them (e.g. lambda).
     (match (strip-binding-types p)
       [(,lang '(program ,body ,_ ...))
        ;; If strip-binding-types worked there shouldn't be any VQueue symbols!
        (DEBUGASSERT (not (deep-assq 'VQueue (list body _))))
        `(begin (reset-wssim-state!)
                (run-stream-query ,body))
        ]))
))


(IFCHEZ 
 ;; For CHEZ this becomes a top-level binding:
 (define-language
   'wavescript-language
   `(begin
      ;; First we have to import the language bindings.
      ;; [2007.01.29] ACK, this is another thing that's mysteriously broken when loading from .so:
      ,(if (and (top-level-bound? 'regiment-origin)
		(or (equal? regiment-origin "compiled .so")
		    (equal? regiment-origin "compiled .boot"))
	       )
	   ;; For some reason we have to handle this differently when working from compiled .so.
	   '(begin
	     (import ws_sim_wavescript_sim_library_push)
	     ;;(import (except scheme break length + - * / ^ inspect letrec import let))
	     ;;(import constants) 
   	     ;;(import helpers)
	     )
	   (begin 
	     (unless (and (top-level-bound? 'regiment-origin)
			  (or (equal? regiment-origin "source")
			      (equal? regiment-origin "saved heap")))
	       (warning 'wavescript-language
			"Regiment loaded from unknown origin.  Might have problems will delicate module issues."))
	     '(begin 
		;; We only import these basic bindings to keep us honest.
		;(import-only ws_sim_wavescript_sim_library_push)
		;; [2008.08.12] Resurecting this, but import-only won't work
		(import ws_sim_wavescript_sim_library_push)
		
		;; Then we import some "sub-modules" exported by the language-module.
		;; This is everything but the overriden bindings from default scheme language:
		;(import (except scheme break length + - * / ^ inspect import let)) ; letrec 
		;(import mod_constants)
		;(import mod_helpers)
		)))
 
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

     ;; Finally, we set up a deallocation mechanism for foreign storage:
     (define old-crh (collect-request-handler))
     ;; Lame, but gotta put it at top-level for the wavescript_sim_library module to get it.
     (foreign-guardian (make-guardian))
     (collect-request-handler 
      (lambda () (collect)
	      ;; TODO: maybe we should just set the foreign-guardian back to #f when we're done with a stream.
	      ;; (But what if other streams are open!!)
	      ;; Sigh, really this guardian should be stored with the stream object.
	      ;;
	      ;; We can do that.... we can bind it *locally* (here)
	      ;; and then while we *are* in this dynamic scope, we can
	      ;; capture & save it as we construct the stream graph...
	      (let f ([x ((foreign-guardian))] [n 0])
		(when x 
;		  (inspect `(FREEING ,x))
		  (pretty-print `(FREEING ,x ,n))
		  (C-free (unbox x)) (f ((foreign-guardian)) (fx+ n 1))))
	      ))
     ) ;; End header:
   ;; Now the footer:
   `(begin 
      ;; Free any remaining foreign objects:
;      (collect)
;      (collect-request-handler old-crh)
      ;; [2007.05.07] DONT KNOW HOW I SHOULD DO THIS!
      ;; THE PROGRAM RETURNS A *STREAM*.
      ;; THE EVALUATION OF THE STREAM IS NOT WITHIN THIS DYNAMIC SCOPE...
      ))

;; R6RS Version:
;; This is also insanely slow.
(define (wavescript-language expr)
  ;(printf "  Evaling in wavescript-language: \n")(pretty-print expr)
  ;; if (simulator-write-sims-to-disk) 
  (let ([result (eval expr (environment '(except (rnrs (6)) error + - * / or and)  
					'(ws sim wavescript_sim_library_push)))])
   ; (printf "    Returning result: ~s\n" result)
    result))

) ; End IFCHEZ

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

