


(library (ws compat compat)
  (export syntax->list getenv
	  merge merge! sort! append! reverse! call/ec inspect native-inspect define-values
	  fluid-let parameterize reg:define-struct reg:struct?
	  void make-parameter
	  printf error format format-syntax-nicely
	  define-top-level-value set-top-level-value! top-level-bound? top-level-value 
	  reg:top-level-eval simple-eval
	  warning warning-handler real-time cpu-time
	  print-level print-graph print-length pretty-maximum-lines pretty-line-length print-vector-length

	  process get-string-available pretty-print

	  box unbox set-box! box? 
	  promise? 
	  
	  system repl make-list with-output-to-string
	  which-scheme IFCHEZ	  
	  
	  (rename 
	          (ik:gensym gensym) 
		  (ik:syntax-error syntax-error) 

		  ;(ik:fx= fx=)  (ik:fx< fx<) (ik:fx> fx>) (ik:fx<= fx<=) (ik:fx>= fx>=) (ik:fxadd1 fxadd1)
		  ;(ik:add1 add1) (ik:sub1 sub1)
		  (ik:random random) 
		  (ik:fprintf fprintf) 
		  (ik:time time) 
		  (ik:current-directory current-directory)
		  (ik:include include)

		  (ik:with-output-to-port with-output-to-port)

		  (ik:delay delay) (ik:force force)
		  (ik:trace-define trace-define)
		  (ik:trace-lambda trace-lambda)

		  )
	  ;current-directory
	  )
  (import (except (rnrs (6)) error)
	  (rnrs eval (6))
	  (prefix (ikarus) ik:)
	  (rnrs programs)
	  )

  (define which-scheme 'ikarus)

  (define (format-syntax-nicely syn) syn)

  (define-syntax make-parameter (identifier-syntax ik:make-parameter))
  (define-syntax parameterize (identifier-syntax ik:parameterize))
  (define-syntax printf (identifier-syntax ik:printf))
  (define-syntax format (identifier-syntax ik:format))
  (define-syntax void   (identifier-syntax ik:void))
  (define-syntax pretty-print (identifier-syntax ik:pretty-print))
  (define-syntax make-list    (identifier-syntax ik:make-list))
  (define-syntax with-output-to-string (identifier-syntax ik:with-output-to-string))

  ;; For the timing being make this nondestructive:
  (define-syntax reverse! (identifier-syntax reverse))
  (define-syntax append! (identifier-syntax append))
  (define-syntax sort! (identifier-syntax list-sort))

  (define call/ec call/cc)

  ;; Include is defined relative to REGIMENTD
  #;
  (define-syntax include 
    (syntax-case ()
      (eval '(REGIMENTD) (environment '(ws globals)))
      ))

  (define (native-inspect x)
    (error 'ikarus "doesn't have a built-in object inspector"))

  (ik:include "ws/compat/common.ss")
  (ik:include "ws/compat/inspector.ss")
  (ik:include "ws/compat/top-level-values.ss")
  (ik:include "ws/compat/multiple-values.ss")

  (define (inspect x)    
    (parameterize ((print-graph #t))
      (generic-inspect x)))
 
  (reg:define-struct (boxrec contents))
  (define-syntax box      (identifier-syntax make-boxrec))
  (define-syntax set-box! (identifier-syntax set-boxrec-contents!))
  (define-syntax unbox    (identifier-syntax boxrec-contents))
  (define-syntax box?     (identifier-syntax boxrec?))

  ;; This is required to return #t for promises, but false positives are ok.
  ;; This is too lenient, but there's no other option.
  (define promise? procedure?)

  (define print-level          (make-parameter 0))
  (define print-length         (make-parameter 0))
  (define pretty-maximum-lines (make-parameter 0))
  (define pretty-line-length   (make-parameter 80))
;  (define print-graph          (make-parameter #f))
  (define print-vector-length  (make-parameter #f))

;   (define print-level          ik:print-level)
;   (define print-length         ik:print-length)
;   (define pretty-maximum-lines ik:pretty-maximum-lines)
;   (define pretty-line-length   ik:pretty-line-length)
   (define print-graph          ik:print-graph)
;   (define print-vector-length  ik:print-vector-length)


  (define (real-time) 0)
  (define (cpu-time) 0)

  (define warning-handler 
    (make-parameter
     (lambda (who str . args)
       (printf "Warning in ~a: ~a\n" who (apply format str args)))))
  (define (warning who str . args)
    (apply (warning-handler) who str args))

  (define (system cmd)
    (let ([ret (ik:system cmd)])
      (if (zero? ret) #t #f)))

  (define (system-to-str str)
    (let* ([pr (process str)]
	   [in (car pr)]
	   [out (cadr pr)])
       (call-with-values open-string-output-port
	(lambda (p extract)
	  (let loop ((c (read-char in)))
	    (if (eof-object? c)	  
		(begin 
		  (close-input-port in)
		  (close-output-port out)
		  (extract))
		(begin (display c p)
		       (loop (read-char in)))))))))

  (define (repl)
    (define env (environment '(except (rnrs (6)) error) '(main_r6rs) '(main) '(prefix (ikarus) ik:) '(ws shortcuts)))
    ;; Ack, here if we pass an explicit evaluator then we can't "define".
    (ik:new-cafe 
     (lambda (x)
       (reg:top-level-eval x env)
       ;(eval x (environment '(except (rnrs (6)) error) '(main_r6rs) '(main) '(prefix (ikarus) ik:)))
       )))

  ;; Ikarus has this now:
#;
  (define current-directory
    (case-lambda 
      [() 
       (let ([s (system-to-str "pwd") ]) 
	 (substring s 0 (- (string-length s) 1)))]
      [(x) (error 'current-directory "cannot set working dir -- not implemented yet")]))
  
  (define (process str)
    ;; Split the string apart by spaces.  This is a bit hackish:
    (define cmd-args 
      (let loop ([chars (string->list str)][acc '()])
	(cond
	 [(null? chars)
	  (if (null? acc) '() (list (list->string (reverse acc))))]
	 [(eq? (car chars) #\space)
	  (cons (list->string (reverse acc)) (loop (cdr chars) '()))]
	 [else (loop (cdr chars) (cons (car chars) acc))])))
    ;(printf "Calling ikarus process command with arguments: ~s\n" cmd-args)
    (let-values ([(pid stdin stdout stderr) (apply ik:process cmd-args)])
      ;; Ape Chez's process for now (even though ikarus's is better!)
      (list (transcoded-port stdout (native-transcoder)) 
	    (transcoded-port stdin (native-transcoder)) pid)))

  (define (getenv s) 
    (let ([res (ik:getenv s)])
      (if (string=? res "") #f res)))

  (define (get-string-available inp)
    (error 'get-string-available "non-blocking IO not implemented..."))
#;  
  (define (get-string-available inp)
    (list->string
     (reverse!
      (let loop ([acc '()])
	(if (char-ready? inp)
	    (let ([c (read-char inp)])
	      (if (eof-object? c) acc
		  (loop (cons c acc))))
	    acc)))))

)
