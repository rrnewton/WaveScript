

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

(module annotate-with-data-rates mzscheme
  (require "../../../plt/common.ss")
  (provide annotate-with-data-rates
           )

  (chezimports)


;;
(define-pass annotate-with-data-rates

    [Program (lambda (prog Expr)
               (define annotated (annotate-iterates-with-types prog))
               (define stripped (strip-types annotated))

               ; taken near-literally from eval-and-peruse-stream
               (define stream-and-table
                 (wavescript-language
                  (match stripped
                    [(,lang '(program ,body ,meta* ... ,type))
                     `(let ((edge-counts-table (make-hash-table))
                            (sum-type-declarations (cdr ',(assq 'union-types meta*))))
                        ,(make-uniontype-defs (assq 'union-types meta*))
                        (reset-wssim-state!)
                        (cons
                         (run-stream-query ,body)
                         edge-counts-table))])))

               (define stream      (car stream-and-table))
               (define rates-table (cdr stream-and-table))

               ; perform the actual simulation;
               ; this will fill in rates-table
               (run-stream-to-completion stream)
               
               ;(printf "the rates:~n")
               ;(hash-table-map rates-table (lambda (k v) (printf "~a: ~a~n" k v)))

               ; convert back to regular program, with data-rate annotations
               (annotate-iterates-with-rates annotated rates-table))])


;; it makes me unhappy that i have to add a new expression to strip-types,
;; but i don't see a way around it.
(define-pass annotate-iterates-with-types

    [Expr (lambda (x fallthru)
            (match x
              [(iterate ,annot
                        (let ,state
                          (lambda ,args (,input-type ,output-type) ,body))
                        ,stream)

               ; FIXME: having to pass in edge-counts-table here annoys me
               `(iterate-bench ,annot
                               ',input-type ',(unique-name 'itb)
                               edge-counts-table
                               sum-type-declarations
                               (let ,state
                                 (lambda ,args (,input-type ,output-type) ,body))
                               ,stream)]

              [,oth (fallthru oth)]))])


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
                                ',input-type ',box-name
                                edge-counts-table
                                sum-type-declarations
                                (let ,state
                                  (lambda ,args (,input-type-redundant ,output-type) ,body))
                                ,stream)
                 ; FIXME: assert that input-type is input-type-redundant?

                 `(iterate ,(add-annotation annot `(data-rate ,box-name ,(get-hash-table rates box-name 0)))
                           (let ,state
                             (lambda ,args (,input-type ,output-type) ,body))
                           ,stream)
                 
                 #;
                 `(data-rate
                   (,box-name ,(get-hash-table rates box-name 0))
                   (iterate ,annot
                            (let ,state
                              (lambda ,args (,input-type ,output-type) ,body))
                            ,stream))

                 ]

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


;;
(define (run-stream-to-completion stream)
  (let loop ()
    (if (stream-empty? stream)
        #t
        (begin
          (set! stream (stream-cdr stream))
          (loop)))))


) ;; End module

