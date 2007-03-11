
(module lang_wavescript_tests mzscheme
  (require "../constants.ss"
           "../util/helpers.ss"
           (prefix ws: "../sim/wavescript_sim_library.ss"))
  (provide test-ws test-lang_wavescript)
  (chezimports (add-prefix wavescript_sim_library_push ws:))
 
  (define-testing these-tests
    (let ()      
      `(
        ["Joinsegs" 
         (,(lambda ()
             (reg:struct->list
              (ws:joinsegs (ws:make-sigseg 10 19 (list->vector (iota 10)) ws:nulltimebase)
                        (ws:make-sigseg 15 24 (list->vector (map (lambda (x) (+ x 5)) (iota 10))) ws:nulltimebase)))))
         ("sigseg" 10 24 #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14) ,ws:nulltimebase)]
        
        ["Subseg"
         (,(lambda ()
             (reg:struct->list
              (ws:subseg (ws:make-sigseg 10 19 (list->vector (iota 10)) ws:nulltimebase) 11 5))))
         ("sigseg" 11 15 #(1 2 3 4 5) ,ws:nulltimebase)]
        
        ["seg-get"
         (,(lambda ()	
             (ws:seg-get (ws:make-sigseg 10 19 (list->vector (iota 10)) ws:nulltimebase) 2)))
         2]
        
        ["width/start/end"
         (,(lambda ()	
             (let ([seg (ws:make-sigseg 11 20 (list->vector (iota 10)) ws:nulltimebase)])
               (list (ws:width seg) (ws:start seg) (ws:end seg)))))
         (10 11 20)]

	;; This primitive is discontinued:
#;        
        ["audioFile"
         (,(lambda ()	
             (let* ([stream (ws:audioFile 
                             (IFCHEZ (string-append (REGIMENTD) "/demos/wavescope/countup.raw")
                                     "../..//demos/wavescope/countup.raw")
                             1024 0 44000)]
                    [first (stream-car stream)]
                    [second (stream-car (stream-cdr stream))])
               (list (ws:width first) (ws:start first) (ws:end first)
                     (ws:width second) (ws:start second) (ws:end second)))))
         (1024 0 1023
	       1024 1024 2047)]
        
        ["for loop"
         (,(lambda ()
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

	,@(map
	      (lambda (prim)
		(list (format "Testing that wavescript implements all primitives: ~a" prim)
		      `(wavescript-language (quote ,prim))
		      'unspecified))
	    (difference
	     (map car (append regiment-basic-primitives
			      wavescript-primitives))
	     ;; Make some exceptions for things that are in Regiment but not WaveScript.
	     ;; Also exceptions for geneeric prims and other prims that have been desugared.
	     (append '(eq? locdiff nodeid sense even? odd? tuple tupref dataFile
			   or and show-and-string-append buildArray
			   ENSBoxAudio
			   )
		     generic-arith-primitives
		     meta-only-primitives)
	     ))

	)))
  
  (define test-this (default-unit-tester "Wavescript emulation language bindings" these-tests))
  (define test-ws test-this)
  (define test-lang_wavescript test-this)
  
) ; End module

;(require lang_wavescript_tests) (test-ws)
; ;(eval (caddr (test-ws 'get 4)))

; (require (prefix ws: "../sim/wavescript_sim_library.ss")         "../util/helpers.ss")

; (define foo (open-input-string "theuuusoaetuhoeasthaosenthueoaunnnnnnnnnnnnnnnnnnnnn"))
; (define buf (make-string 200 #\-))
; (define (r) (block-read foo buf 150))
; (display (r))

; (display (ws:sigseg-vec (stream-car (ws:audioFile  "../..//demos/wavescope/countup.raw" 1024 0))))
