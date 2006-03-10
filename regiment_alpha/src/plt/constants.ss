
#cs ;; Case Sensitivity
(module constants mzscheme
	(require (lib "include.ss"))

	(provide 
         ;; Syntax:
         IFDEBUG
         DEBUGMODE UBERDEBUGMODE DEBUGPRINT DEBUGPRINT2 DEBUGASSERT ASSERT
         IF_GRAPHICS
         REGIMENT_DEBUG
         chezimports ;; To make the common module facility work.
         
         REGIMENTD
         SCHEDULE_DELAY         
         RADIO_DELAY ;PROCESSING_TIME ;; Not used yet
         
	 define-regiment-parameter regiment-parameters
	 regiment-verbose 
	 simulation-logger 
	 simulation-logger-count
	 simulation-logger-level
	 simulation-logger-human-readable
	 simulation-logger-fasl-batched
	 simulation-logger-gzip-output
	 reg:all-unit-tests
	 reg:comment-code
	 reg:define-struct
         reg:struct?
	 reg:struct->list
         
         pass-names
         
         default-slow-pulse default-fast-pulse
         
         deglobalize-markup-returns
         
         unknown-place noplace
         
	 TMNULL
         KINIT_FLAG KCALL_FLAG NULLK
	 DEFAULT_SUBTOK DEFAULT_SUBTOK_VAR
         MAX_SUBTOK        

	 desugar-gradients-mode 
	 etx-retry-delay 
	 etx-max-retries

         window-width window-height
         processor-screen-radius
         world-xbound world-ybound radius numsimnodes SPECIAL_RETURN_TOKEN 
         BASE_ID NULL_ID	 
       	 return-window-size
         
         sim-num-nodes
         simalpha-output-port
	 simalpha-outer-radius
	 simalpha-inner-radius
	 simalpha-placement-type
	 simalpha-max-gridlike-perturbation
	 simalpha-world-xbound
	 simalpha-world-ybound
	 simalpha-channel-model
	 simalpha-failure-model
	 simalpha-current-simworld 
	 sim-timeout
	 simalpha-realtime-mode
	 simalpha-consec-ids
	 simalpha-dbg-on
	 simalpha-zeropad-args
	 simalpha-stream-result 
	 simalpha-sense-function
	 simalpha-sense-function-constructor
	 simalpha-graphics-on
         simalpha-write-sims-to-disk
	 simalpha-label-msgcounts 
	 simalpha-label-sensorvals 
	 simalpha-pause-hook
	 
         make-rgb
         rgb-red
         rgb-green
         rgb-blue
         
	 default-unit-tester-retries
                  
         )

  ;; [2005.11.04] This is lame, but the only way I know of to check for MrED is
  ;; to try to require it -- if we get an error, it's not there.
#;
   (define-syntax IF_GRAPHICS
    (lambda (x)
      (let ((flag (let/ec esc
               (parameterize ([error-display-handler (lambda (ob s) (void))]
                              [error-escape-handler (lambda args (esc #f))])
                 (eval '(require (lib "mred.ss" "mred")))
                 (esc #t)))))
        (syntax-case  x ()
          [(_ E1 E2) (if flag #'E1 #'E2)]
          [(_ E1) (if flag #'E1)]))))
  

   
   (define-syntax reg:define-struct
     (syntax-rules ()
       [(_ (sname field ...))
	(define-struct sname (field ...) 
	  ;;(current-inspector))])) 
	  ;;(make-inspector))]))
	  #f)]))
  (define reg:struct? struct?)
  (define (reg:struct->list s)
    (let-values ([(strty _) (struct-info s)])
      (let-values ([(name initf autof access mutat immutlst super skipped?) (struct-type-info strty)])
	(let ([stop (+ initf autof)])
	  (cons (string-copy name)
		(let loop ([i 0])
		  (if (= i stop)
		      '()
		      (cons (access s i) (loop (add1 i))))))
	  ))))

  (define-syntax chezimports
    (syntax-rules ()
      [(_ e ...) (begin)]))
  
   (include (build-path "generic" "constants.ss"))

   
   ;; [2005.11.04] I was having trouble with that.  When compiling from
   ;; command line I'd get an error.  Let's just set it manually:
   (define-syntax IF_GRAPHICS
     (syntax-rules ()
       [(_ t f) f]
       [(_ t)   (void)]))

)

;(require constants)
