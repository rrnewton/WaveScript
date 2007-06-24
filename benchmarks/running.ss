;
; all of this may break if the stats. printed by WaveScope are not in the expected format
;
; FIXME: was it a mistake to do this using plt scheme?
(module running mzscheme

  ;
  (require (lib "process.ss")
           (lib "pregexp.ss")
           ;"../generic/util/helpers.ss"
           )

  ;
  (provide compile-program
           measure-wavescope-program)

  ; FIXME: these are the duplicates from helpers.ss
  (provide n-times foldr)
   


;
;
;
(define (run-process path . args)
  (let ((proc-vals (apply process* (cons path args))))
    (let loop ((output-line (read-line (car proc-vals))))
      (if (not (eq? output-line eof))
          (begin
            (printf "~a~n" output-line)
            (loop (read (car proc-vals))))
          (begin
            (close-input-port (car proc-vals))
            (close-output-port (cadr proc-vals))
            (close-input-port (cadddr proc-vals)))))))

          
      
;
; compile a wavescript program
;   extension is a string to add into the names of the c++ source and binary
;   returns c++ binary's filename on success, #f on failure
;
(define (compile-program filename extension disabled-passes)
  (let ((c++-src-filename (string-append filename "." extension ".cpp"))
        (c++-bin-filename (string-append filename "." extension ".c++.exe"))
        (disabled-pass-flags
         (foldr (lambda (x y) (string-append " --disable-pass " (symbol->string x) y)) "" disabled-passes)))

    (if (and (system (string-append "wsc " filename disabled-pass-flags))
             (system (string-append "mv query.cpp " c++-src-filename))
             (system (string-append "mv query.exe " c++-bin-filename)))
        c++-bin-filename
        #f)))


;
;
;
(define (measure-wavescope-program filename)
  (let ((proc-vals (process filename)))
    (let ((proc-stdout     (list-ref proc-vals 0))
          (proc-stdin      (list-ref proc-vals 1))
          (proc-pid        (list-ref proc-vals 2))
          (proc-stderr     (list-ref proc-vals 3))
          (proc-controller (list-ref proc-vals 4)))

      ;(display (proc-controller 'status)) (newline)

      ; loop until start of scheduler statistics output
      (let loop ()
        (if (not (pregexp-match "^Scheduler Statistics Summary" (read-line proc-stdout)))
            (loop)))

      ; gather scheduler statistics
      (let* ((deferred-queues (car (pregexp-match "[0-9]"           (read-line proc-stdout))))
             (processing-time (car (pregexp-match "[0-9]*\\.[0-9]*" (read-line proc-stdout))))
             (user-time       (car (pregexp-match "[0-9]*\\.[0-9]*" (read-line proc-stdout))))
             (system-time     (car (pregexp-match "[0-9]*\\.[0-9]*" (read-line proc-stdout)))))

        (proc-controller 'wait)
        (close-input-port proc-stdout)
        (close-output-port proc-stdin)
        (close-input-port proc-stderr)

        (map string->number (list processing-time user-time system-time))))))



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