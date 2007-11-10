;
;
;
(module measure mzscheme

  ;
  (require (lib "pretty.ss")
           "compile.ss"
           "running.ss")

  ;
  (provide measure-wavescript-program
           compile-and-measure-wavescript-program)


;
; no defaults for: ws-filename
;
(define default-plan
  '(
    (disabled-passes)
    (backend . c++)
    (optimizations)
    (source-code)
    (exe)
    (scheduler . corefit-scheduler-df)
    (num-tuples . -1)
    (run-before) ; FIXME: not impl. yet
    (run-after)  ; FIXME: not impl. yet
    (compile-logfile)
    (run-logfile)
    ))


;
;
;
(define (measure-wavescript-program plan)
  
  (define full-plan (append plan default-plan))
  (define execution-output (run-wavescope-program full-plan))

  (define implicit-parameters
    `(,@(get-svn-info)))

  (append execution-output implicit-parameters plan))


;
;
;
(define (compile-and-measure-wavescript-program plan)

  (define full-plan (append plan default-plan))
  (define compilation-output (compile-wavescript-program full-plan))
  (define execution-output (run-wavescope-program (append compilation-output full-plan)))

  (define implicit-parameters
    `(,@(get-svn-info)))

  (append execution-output implicit-parameters plan))


;
; end of module declaration
;
)