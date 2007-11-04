;
;
;
(module compile mzscheme
  
  ;
  (require (lib "process.ss")
           (lib "file.ss")
           )

  ;
  (provide compile-wavescript-program)
  

;
;
;
(define (compile-wavescript-program-c++ plan)

  ;; pull out params.
  (define input-filename      (cdr (assoc 'ws-filename plan)))
  (define disabled-passes     (cdr  (assoc 'disabled-passes plan)))
  (define output-src-filename (cdr (assoc 'source-code plan)))
  (define output-exe-filename (cdr (assoc 'exe plan)))
  (define scheduler           (cdr (assoc 'scheduler plan)))
  (define logfile             (cdr (assoc 'compile-logfile plan)))

  (define disabled-pass-flags
    (foldr (lambda (pass flags-str) (string-append " --disable-pass " (symbol->string pass) flags-str)) "" disabled-passes))

  ;; FIXME: this is a really silly way of pushing an s-exp. of parameters into the wavescript compiler
  (define tmp-plan-file-path (make-temporary-file))
  (let ((tmp-file (open-output-file tmp-plan-file-path 'truncate)))
    (print plan tmp-file)
    (close-output-port tmp-file))


  ;; FIXME: hack
  (if (null? output-src-filename) (set! output-src-filename "./query.cpp"))
  (if (null? output-exe-filename) (set! output-exe-filename "./query.exe"))

  ;;
  (let ((compiled?
         (and
          (system (string-append "wsc"
                                 " " input-filename
                                 " --scheduler " (symbol->string scheduler)
                                 " --param-file " (path->string tmp-plan-file-path)
                                 " " disabled-pass-flags
                                 (if (not (null? logfile)) (string-append " &>" logfile) "")))
          (or (string=? "./query.cpp" output-src-filename) ; FIXME: tiny bug, if output-src-filename is surrounded by whitespace, etc.
              (system (string-append "mv ./query.cpp " output-src-filename)))
          (or (string=? "./query.exe" output-exe-filename) ; FIXME: tiny bug again
              (system (string-append "mv ./query.exe " output-exe-filename))))))

    ;; cleanup
    (delete-file tmp-plan-file-path)

    (if compiled?

        ;; FIXME: this is the barest possible output;
        ;;        need to gather up the compile-time profiling stats. as well
        `((exe . ,output-exe-filename))

        ;; failure -- not compiled
        #f)))


;
;
;
(define (compile-wavescript-program-mlton plan)

  ;; pull out params.
  (define input-filename      (cdr (assoc 'ws-filename plan)))
  (define output-src-filename (cdr (assoc 'source-code plan)))
  (define output-exe-filename (cdr (assoc 'exe plan)))
  (define logfile             (cdr (assoc 'compile-logfile plan)))

  ;; FIXME: this is a really silly way of pushing an s-exp. of parameters into the wavescript compiler
  (define tmp-plan-file-path (make-temporary-file))
  (let ((tmp-file (open-output-file tmp-plan-file-path 'truncate)))
    (print plan tmp-file)
    (close-output-port tmp-file))


  ;; FIXME: hack
  (if (null? output-src-filename) (set! output-src-filename "./query.sml"))
  (if (null? output-exe-filename) (set! output-exe-filename "./query.mlton.exe"))
  
  ;;
  (let ((compiled?
         (and
          (system (string-append "wsmlton"
                                 " " input-filename
                                 (if (not (null? logfile)) (string-append " &>" logfile) "")))
          (or (string=? "./query.sml" output-src-filename) ; FIXME: tiny bug, if output-src-filename is surrounded by whitespace, etc.
              (system (string-append "mv ./query.sml " output-src-filename)))
          (or (string=? "./query.mlton.exe" output-exe-filename) ; FIXME: tiny bug again
              (system (string-append "mv ./query.mlton.exe " output-exe-filename))))))

    ;; cleanup
    (delete-file tmp-plan-file-path)

    (if compiled?

        ;; FIXME: barest possible output
        `((exe . ,output-exe-filename))

        ;; failure -- not compiled
        #f)))


;
;
;
(define (compile-wavescript-program plan)
  (case (cdr (assoc 'backend plan))
    ((c++)   (compile-wavescript-program-c++ plan))
    ((mlton) (compile-wavescript-program-mlton plan))
    (else  (error 'compile-wavescript-program "unknown backend: ~a" (cdr (assoc 'backend plan))))))


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


;
; end of module declaration
;
)      

  