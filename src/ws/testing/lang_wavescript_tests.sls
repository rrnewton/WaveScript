#!r6rs

(library (ws testing lang_wavescript_tests) 
  (export test-ws test-lang_wavescript
	  lang_wavescript_prim-exceptions
	  )
  (import  (except (rnrs (6)) error)
	   (ws common)
           (prefix (ws globals) ws:)
           (prefix (ws sim wavescript_sim_library_push) ws:))
#;
  (chezimports (add-prefix wavescript_sim_library_push ws:)
	       ;; Hack, migrated some bindings here:
	       (add-prefix constants ws:))

;; Things not covered:
(define lang_wavescript_prim-exceptions
  ;; Make some exceptions for things that are in Regiment but not WaveScript.
  ;; Also exceptions for geneeric prims and other prims that have been desugared.
  (append '(eq? 
	        locdiff nodeid sense world anchor IS_SIM
		even? odd? dataFile readFile

		ptrToArray

		tuple tupref
                 __foreign foreign 
		foreign_source __foreign_source
		or and 
		ensBoxAudio ensBoxAudioF ensBoxAudioAll

		;; These were resolved into the w/namespace versions:
		head tail map append fold 
		ws:or ws:and

		;; These are defined as syntaxes:
		Mutable:ref ref deref static statref 
		
		;; This is no longer supported:
		break		
		_emit_to

		;; Not supported in scheme backend or metaprogram eval:
		marshal unmarshal __type_unsafe_write __type_unsafe_read
		)
	  generic-arith-primitives
	  meta-only-primitives))
 
  (define-testing  test-lang_wavescript
    (default-unit-tester "Wavescript emulation language bindings" 
       (let ()      
      `(
;; [2008.04.30] Don't have reg:struct->list yet in our R6RS port:
#|
        ["Joinsegs" 
         (',(lambda ()
             (reg:struct->list
              (ws:joinsegs (ws:make-sigseg 10 19 (list->vector (iota 10)) ws:nulltimebase)
                        (ws:make-sigseg 15 24 (list->vector (map (lambda (x) (+ x 5)) (iota 10))) ws:nulltimebase)))))
         ("sigseg" 10 24 #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14) ,ws:nulltimebase)]
        
        ["Subseg"
         (',(lambda ()
             (reg:struct->list
              (ws:subseg (ws:make-sigseg 10 19 (list->vector (iota 10)) ws:nulltimebase) 11 5))))
         ("sigseg" 11 15 #(1 2 3 4 5) ,ws:nulltimebase)]
|#

        
        ["seg_get"
         (',(lambda ()	
             (ws:seg_get (ws:make-sigseg 10 19 (list->vector (iota 10)) ws:nulltimebase) 2)))
         2]
        
        ["width/start/end"
         (',(lambda ()	
             (let ([seg (ws:make-sigseg 11 20 (list->vector (iota 10)) ws:nulltimebase)])
               (list (ws:width seg) (ws:start seg) (ws:end seg)))))
         (10 11 20)]


;; [2008.04.30] Don't have break any more:        
#;
        ["for loop"
         (',(lambda ()
             (let ([sum 0])
               ;; This outer loop goes 10 times.
               (ws:for (i 1 20)
                    (set! sum (+ sum (* 1000 i)))
                    ;; This inner loop goes 10 times for each outer iteration (100 total)
                    (ws:for (i 21 40)
                         (set! sum (+ sum 1))
                         (if (= i 30) (ws:break))
                         )
                    (if (= i 10) (ws:break)))
               sum)
             ))
         55100]

	["num reps 1" (',(lambda () (ws:__cast_num #f 'Int16 32.0))) 32]
	["num reps 2" (',(lambda () (ws:__cast_num #f 'Int16 32769.0))) -32767]

	;[]

	,@(map
	      (lambda (prim)
		(list (format "Testing that wavescript implements all primitives: ~a" prim)
		      `(wavescript-language (quote ,prim))
		      'unspecified))
	    (difference
	     (map car (append  wavescript-basic-primitives 
			       ;wavescript-distributed-primitives
			       wavescript-primitives
			       meta-only-primitives
			       higher-order-primitives
			       wavescript-constants))
	     lang_wavescript_prim-exceptions
	     ))

	))))

  (define test-ws test-lang_wavescript)
  
) ; End module

;(require lang_wavescript_tests) (test-ws)
; ;(eval (caddr (test-ws 'get 4)))

; (require (prefix ws: "../sim/wavescript_sim_library.ss")         "../util/helpers.ss")

; (define foo (open-input-string "theuuusoaetuhoeasthaosenthueoaunnnnnnnnnnnnnnnnnnnnn"))
; (define buf (make-string 200 #\-))
; (define (r) (block-read foo buf 150))
; (display (r))

; (display (ws:sigseg-vec (stream-car (ws:audioFile  "../..//demos/wavescope/countup.raw" 1024 0))))
