#!/usr/bin/mzscheme -C


;
;
;
(require (lib "pretty.ss")
         (lib "string.ss")
         (lib "cmdline.ss")
         "measure.ss")


;
;
;
(define (main args)
  
  ;;
  (define plan '())

  (let ((immediate-plan #f)
        (plan-file #f)
        (ws-filename #f))
    
    (command-line (car args) (cdr args)
      (once-each
       [("-p" "--plan")      p
                             "Specify the plan directly"
                             "(appended to the front, if a plan file"
                             "is also specified)"
                             (set! plan p)]
       [("-f" "--plan-file") pf
                             "Specify a file containing the plan"
                             (set! plan-file pf)])

      (args args           "WaveScript source filename"
                           "(if given, appended to the front of the plan)"
                           (case (length args)
                             ((0) '())
                             ((1) (set! ws-filename (car args)))
                             (else (error 'main "too many extra command-line arguments given")))))

    (if plan-file
        (set! plan (append (read (open-input-file plan-file)) plan)))

    (if immediate-plan
        (set! plan (append immediate-plan plan)))

    (if ws-filename
        (set! plan (cons `(ws-filename ,ws-filename) plan)))
    
    ; FIXME: figure out how to print usage from the command-line structure above
    (if (andmap not `(,immediate-plan ,plan-file ,ws-filename))
        (error 'main "please provide some command-line arguments")))

  (pretty-print
   (measure-wavescript-program plan)))



      

    
