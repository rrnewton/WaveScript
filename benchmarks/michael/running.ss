;
; all of this may break if the stats. printed by WaveScope are not in the expected format
;
; FIXME: was it a mistake to do this using plt scheme?
(module running mzscheme

  ;
  (require (lib "process.ss")
           (lib "pregexp.ss")
           (lib "port.ss")
           ;"../generic/util/helpers.ss"
           )

  ;
  (provide run-wavescope-program

           get-svn-info
           )

  ; FIXME: these are the duplicates from helpers.ss
  (provide n-times foldr)


;
; FIXME: only works on stdout-bound processes
;
(define (run-program cmdline greppers output-port)
  (let* ((proc-vals (process cmdline))
         (proc-stdout (list-ref proc-vals 0))
         (proc-stdin  (list-ref proc-vals 1))
         (proc-pid    (list-ref proc-vals 2))
         (proc-stderr (list-ref proc-vals 3))
         (proc-ctrl   (list-ref proc-vals 4))
         (stdouterr   (input-port-append #f proc-stdout proc-stderr)))

    ;;
    (define summary
      (let loop ((gathered-info ()))
        (let ((line (read-line stdouterr))) ; FIXME: does not guarantee that lines from stdout/stderr don't overrun each other

          ;;
          (fprintf output-port "~a~n" line)

          ;; run each of the line greppers until there's a match
          (if (eof-object? line)
              gathered-info
              (let ((new-info (ormap (lambda (gf) (gf line)) greppers)))
                (if new-info
                    (loop (append new-info gathered-info))
                    (loop gathered-info)))))))

    ;;
    (proc-ctrl 'wait)
    (close-input-port proc-stdout)
    (close-output-port proc-stdin)
    (close-input-port proc-stderr)

    ;;
    summary))


;
;
;
(define (run-wavescope-program-c++ plan)

  ;; pull out params.
  (define filename (cdr (assoc 'exe plan)))
  (define num-tups (cdr (or (assoc 'num-tuples plan) '(_ . #f))))
  (define logfilename (cdr (assoc 'run-logfile plan)))

  ;; greppers:
  ;; a list of functions, each taking an input line,
  ;; and returning a list of new statistics, or #f
  (define greppers
    (list

     [lambda (line)
       (let ((times (pregexp-match "real=([0-9]*) user=([0-9]*) sys=([0-9]*) sum=[0-9]*" line)))
         (if times
             (let-values ([(real-time user-time sys-time)
                           (apply values (map string->number (cdr times)))])
               `((real-time . ,real-time) (user-time . ,user-time) (sys-time . ,sys-time)))
             #f))]
     ))

  ;; build command-line
  (define command-line
    (string-append filename
                   " -j 1 " ; FIXME: should be in the plan
                   (if num-tups (string-append " -n " (number->string num-tups)) "")))

  ;;
  (if (not (null? logfilename))
      (let ((logfile (open-output-file logfilename 'append)))
        (run-program command-line greppers logfile)
        (close-output-port logfile))
      (run-program command-line greppers (current-output-port))))


;
;
;
(define (run-wavescope-program-mlton plan)

  ;; pull out params.
  (define filename (cdr (assoc 'exe plan)))
  (define num-tups (cdr (or (assoc 'num-tuples plan) '(_ . #f))))
  (define logfilename (cdr (assoc 'run-logfile plan)))

  ;; greppers
  (define greppers
    (list

     [lambda (line)
       (let ((times (pregexp-match "^usertime, systime, realtime: (.*), (.*), (.*)" line)))
         (if times
             (let-values ([(user-time sys-time real-time)
                           (apply values (map string->number (cdr times)))])
               `((real-time . ,real-time) (user-time . ,user-time) (sys-time . ,sys-time)))
             #f))]
     ))

  ;; build command-line
  (define command-line
    (string-append "/usr/bin/time -f \"usertime, systime, realtime: %U, %S, %e\n\""
                   " " filename
                   (if num-tups (string-append " -n " (number->string num-tups)) "")))

  ;;
  (if (not (null? logfilename))
      (let ((logfile (open-output-file logfilename 'append)))
        (run-program command-line greppers logfile)
        (close-output-port logfile))
      (run-program command-line greppers (current-output-port))))
      

;
;
;
(define (run-wavescope-program plan)
  (case (cdr (assoc 'backend plan))
    ((c++)   (run-wavescope-program-c++ plan))
    ((mlton) (run-wavescope-program-mlton plan))
    (else  (error 'run-wavescope-program "unknown backend: ~a" (cdr (assoc 'backend plan))))))


;
; FIXME: should not be in this file
;
(define (get-svn-info)
  (run-program
   "cd $WAVESCRIPTD; svn info"
   (list
    [lambda (line)
      (let ((url-match (pregexp-match "^URL: (.*)" line)))
        (if url-match
            `((svn-url . ,(cadr url-match)))
            #f))]
    
    [lambda (line)
      (let ((rev-match (pregexp-match "^Last Changed Rev: ([0-9]*)" line)))
        (if rev-match
            `((svn-revision . ,(string->number (cadr rev-match))))
            #f))])
   (current-output-port)))
   


;
;
; FIXME: until i figure out how to include helpers.ss normally
;
;

;; Also from PLT's list.ss
(define foldr
  (letrec ((fold-one
	    (lambda (f init l)
	      (letrec ((helper
			(lambda (init l)
			  (cond
			   [(null? l) init]
			   [else (f (car l) (helper init (cdr l)))]))))
		(helper init l))))
	   (fold-n
	    (lambda (f init l)
	      (cond
	       [(ormap null? l)
		(if (andmap null? l)
		    init
		    (error 'foldr "received non-equal length input lists"))]
	       [else (apply f
			    (mapadd car l
				    (fold-n f init (map cdr l))))]))))
    (case-lambda
      [(f init l) (fold-one f init l)]
      [(f init l . ls) (fold-n f init (cons l ls))])))

;;
(define (mapadd f l last)
  (let loop ([l l])
    (if (null? l)
        (list last)
        (cons (f (car l)) (loop (cdr l))))))

; --mic
; result is (rn ... r2 r1 r0), where r0 is first call, r1 second, etc.
; n must be >= 0
;
(define (n-times p n . args)
  (let loop ((i 0)
             (results ()))
    (if (< i n)
        (loop (+ i 1) (cons (apply p args) results))
        results)))




;
; end of module declaration
;
)
