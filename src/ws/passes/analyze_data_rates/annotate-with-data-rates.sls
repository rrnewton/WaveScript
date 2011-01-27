#!r6rs

;;;; .title Pass: Annotate with Data Rates
;;;; .author Michael Craig

;;;; This pass runs the simulator on the program, recording the total amounts
;;;; of data sent between boxes, and annotating the iterates with those
;;;; amounts.

;;;; Proper function of this code relies on the following conditions:
;;;; * occurrences of iterate in the given program are of the form:
;;;;     (iterate (let ,state-bindings (lambda ,args ,types ,body)) ,stream)
;;;;
;;;; * the variable name edge-counts-table is not used elsewhere in the given program

(library (ws passes analyze_data_rates annotate-with-data-rates)  
  (export annotate-with-data-rates)
  (import (except (rnrs (6)) error) (ws common) (ws util helpers)
	  (ws langs lang_wavescript))

;;
(define (stream-type? ty)   (and (pair? ty) (eq? (car ty) 'Stream)))
(define (type-of-stream ty) (match ty [(Stream ,elt) elt]))

;; Use whatever hashtable rep
(define HashTable:get (wavescript-language 'HashTable:get))

;;
(define-pass annotate-with-data-rates
    [Program (lambda (prog Expr)
	       (parameterize ([source-frequencies '()])

               (define annotated (annotate-iterates-with-types prog))	       
               (define stripped (strip-binding-types annotated))

               ; taken near-literally from eval-and-peruse-stream
               (define stream-and-table
                 (wavescript-language
                  (match stripped
                    [(,lang '(program ,body ,meta* ... ,type))
		     ;; rrn: Note, this program is designed to run only once.
		     ;; The hash table is allocated here.
                     `(let ((edge-counts-table ',(make-default-hash-table 1000))  ;; (HashTable:make 1000)
                            (sum-type-declarations (cdr ',(assq 'union-types meta*))))
			;(display  "RUNNING PROFILE PROGRAM...\n")
			,(make-uniontype-defs (assq 'union-types meta*))
                        (reset-wssim-state!)
                        (cons
                         (run-stream-query ,body)
                         edge-counts-table))])))

               (define stream      (car stream-and-table))
               (define rates-table (cdr stream-and-table))	       	       

               ; perform the actual simulation;
               ; this will fill in rates-table
	       (when (>= (regiment-verbosity) 1)
		 (printf "Directing profiling run output to .profile_run.out\n"))
	       (let ([out (force-open-output-file ".profile_run.out")])
		 (parameterize (;[current-output-port out]
				;[current-error-port out]
				[ws-print-output-port out]
				)
		   (run-stream-to-completion stream)))

	       ;(printf "the rates:~n")
               ;(hash-table-map rates-table (lambda (k v) (printf "~a: ~a~n" k v)))

               ; convert back to regular program, with data-rate annotations
	       (match (annotate-iterates-with-rates annotated rates-table)
		 [(,lang '(program ,bod ,meta* ...))
		  `(,lang '(program ,bod (profiled-input-frequencies ,@(source-frequencies)) ,meta* ...))]
		 ;(inspect (deep-assq-all 'data-rates result))
		 ;(inspect result)
		 )))])

(define source-frequencies (wavescript-language 'bench-source-frequencies))

;(define source-frequencies '())
;(trace-define (register-profiling-source-frequency! freq)
;  (set! source-frequencies (cons freq source-frequencies)))

;; it makes me unhappy that i have to add a new expression to strip-binding-types,
;; but i don't see a way around it.
(define-pass annotate-iterates-with-types

    ;; to be filled in from input-parameters
    (define num-tuples #f)

    [Expr (lambda (x fallthru)
            (match x

	      ;; rrn: Turn IFPROFILE switch ON:
	      ;[(IFPROFILE ,[a] ,[b]) a]

              [(iterate ,annot
                        (let ,state
                          (lambda ,args (,input-type (VQueue ,output-type)) ,[body]))
                        ,stream)

               ; FIXME: having to pass in edge-counts-table here annoys me
               `(iterate-bench ,annot
                               ',output-type ',(unique-name 'itb)
                               edge-counts-table
                               sum-type-declarations
                               (let ,state
                                 (lambda ,args (,input-type ,output-type) ,body))
                               ,stream)]
              
              [(let ((,stream-name
                      ,output-stream-type
                      (_merge ,annot ,s1 ,s2)))
                 ,[body])
               (ASSERT (stream-type? output-stream-type))

               `(let ((,stream-name
                       ,output-stream-type
                       (_merge-bench ,annot
                                     ',(type-of-stream output-stream-type)
                                     ',(unique-name 'mgb)
                                     edge-counts-table
                                     sum-type-declarations
                                     ,s1 ,s2)))
                  ,body)]

              ; FIXME: get rid of the assert-type here (from annotate-outside-prims, probably)
              [(let ((,stream-name
                      ,output-stream-type
                      (assert-type ,output-stream-type-assert
                                   (unionN ,annot ,in-strs ...))))
                 ,[body])
               (ASSERT (equal? output-stream-type output-stream-type-assert))
               (ASSERT (stream-type? output-stream-type))

               `(let ((,stream-name
                       ,output-stream-type
                       (assert-type ,output-stream-type-assert
                                    (unionN-bench ,annot
                                                  ',(type-of-stream output-stream-type)
                                                  ',(unique-name 'unb)
                                                  edge-counts-table
                                                  sum-type-declarations
                                                  ,@in-strs))))
                  ,body)]
              
              [(let ((,stream-name
                      ,output-stream-type
                      (__readFile ,annot ,file ,srcstrm ,mode ,repeat ,skipbytes ,offset ,winsize ,types)))
                 ,[body])
               (ASSERT (stream-type? output-stream-type))

               `(let ((,stream-name
                       ,output-stream-type
                       (__readFile ,annot ,file ,srcstrm ,mode ,repeat ,skipbytes ,offset ,winsize ,types
                                   ',(type-of-stream output-stream-type)
                                   ',(unique-name 'rfb)
                                   edge-counts-table
                                   sum-type-declarations)))
                  ,body)]

              [(let ((,stream-name
                      ,output-stream-type
                      (timer ,annot ,freq)))
                 ,[body])
               (ASSERT (stream-type? output-stream-type))
               
               `(let ((,stream-name
                       ,output-stream-type
                       (timer-bench ,annot ',(type-of-stream output-stream-type) ',(unique-name 'tmr)
                                    edge-counts-table sum-type-declarations
                                    ,freq ,num-tuples)))
                  ,body)]
              

              [,oth (fallthru oth)]))]

    [Program (lambda (p Expr)
               (match p
                 [(,lang '(program ,bod ,meta* ... (input-parameters ,params) ,type))
                  (set! num-tuples (cdr (or (assoc 'num-tuples params) '(_ . -1))))
		  `(,lang '(program ,(Expr bod) ,@meta* (input-parameters ,params) ,type))]))])


;;
;; FIXME: rename rates to something closer to edge-counts-table
(define (annotate-iterates-with-rates prog rates)

  ;; FIXME: this should be pulled out to a common file soon,
  ;;        and otherwise made a little smarter
  (define (add-annotation annots new-annot)
    `(,(car annots) ,new-annot . ,(cdr annots)))

  (define-pass annotate-iterates-with-rates
      [Expr (lambda (x fallthru)
              (match x
                [(iterate-bench ,annot
                                ',output-type ',box-name
                                edge-counts-table
                                sum-type-declarations
                                (let ,state
                                  (lambda ,args (,input-type ,output-type-redundant) ,[body]))
                                ,stream)

                 `(iterate ,(add-annotation annot `(data-rates ,box-name ,(hashtab-get rates box-name)))
                           (let ,state
                             (lambda ,args (,input-type (VQueue ,output-type)) ,body))
                           ,stream)]


                [(let ((,stream-name
                        ,output-stream-type
                        (_merge-bench ,annot
                                      ',output-type
                                      ',box-name
                                      edge-counts-table
                                      sum-type-declarations
                                      ,s1 ,s2)))
                   ,[body])
                 
                 `(let ((,stream-name
                         ,output-stream-type
                         (_merge ,(add-annotation annot `(data-rates ,box-name ,(hashtab-get rates box-name)))
                                 ,s1 ,s2)))
                    ,body)]


                [(let ((,stream-name
                        ,output-stream-type
                        (assert-type ,output-stream-type-assert
                                     (unionN-bench ,annot
                                                   ',output-type
                                                   ',box-name
                                                   edge-counts-table
                                                   sum-type-declarations
                                                   ,in-strs ...))))
                   ,[body])

                 `(let ((,stream-name
                         ,output-stream-type
                         (assert-type ,output-stream-type-assert
                                      (unionN ,(add-annotation annot
                                                               `(data-rates ,box-name ,(hashtab-get rates box-name)))
                                              ,@in-strs))))
                    ,body)]


                [(__readFile ,annot ,file ,srcstrm ,mode ,repeat ,skipbytes ,offset ,winsize ,types
                             ',output-type ',box-name edge-counts-table sum-type-declarations)
                 `(__readFile ,(add-annotation annot `(data-rates ,box-name ,(hashtab-get rates box-name)))
                              ,file ,srcstrm ,mode ,repeat ,skipbytes ,offset ,winsize ,types)]


                [(timer-bench ,annot ',output-type ',box-name edge-counts-table sum-type-declarations ,freq ,num-tuples)
                 `(timer ,(add-annotation annot `(data-rates ,box-name ,(hashtab-get rates box-name))) ,freq)]


                [,oth (fallthru oth)]))])

  (annotate-iterates-with-rates prog))


;; FIXME: this is also copied wholesale from main.ss
(define (make-uniontype-defs x)
  (match x
    [#f '(void)]
    [(union-types ((,name* . ,_) [,fld** ,ty**] ...) ...)
     (cons 'begin
	   (map (lambda (fld ty) 
             `(define ,fld (lambda (val) (make-uniontype ',fld val))))
	     (apply append fld**)
	     (apply append ty**)))]))


;; rrn: "completion" is now defined by the parameter ws-profile-limit
;;
;; Note this can't currently preempt the running graph, it can only
;; stop after each output element is produced.  Could use engines for
;; this if they weren't already used in the stream implementation.
(define (run-stream-to-completion stream)
  (define timelimit #f)
  (define virtlimit #f)
  (define elemlimit #f)
  (define get-vtime (wavescript-language 'get-current-vtime))
  
  (match (ws-profile-limit)
    [none (void)]
    [(time ,t)     (ASSERT integer? t) (set! timelimit t)]
    ;; Here we convert milliseconds to microseconds:
    [(virttime ,t) (ASSERT integer? t) (set! virtlimit (* 1000 t))]
    [(elements ,n) (ASSERT integer? n) (set! elemlimit n)])
  (let ([end-time (and timelimit (+ timelimit (cpu-time)))])
    (let loop ([stream stream] [elems 0])
      (if (or (stream-empty? stream)
	      (and virtlimit (> (get-vtime) virtlimit))
	      (and end-time  (> (cpu-time) end-time))
	      (and elemlimit (>= elems elemlimit))      
	      )
	  (printf " Finished profiling program.\n")
	  (loop (stream-cdr stream) (fx+ 1 elems))))))


) ;; End module
