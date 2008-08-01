#!r6rs

;;;; This is the simpler variant of merge-iterates that precedes
;;;; Michael's "full merging" version.

(library (ws passes optimizations simple-merge-iterates)
  
  (export simple-merge-iterates
           simple-merge-policy:always
	   test-simple-merge-iterates

           new-simple-merge-iterates
           new-simple-merge-policy:always)

  (import (rnrs) (ws common)
	  (ws compiler_components annotations))


; REV 2807: before the full-merging was checked in:
;;
;; mic: this assumes that the program has been smoosh-together'd
;;
(define-pass simple-merge-iterates 
    ;; -> [X] -> [Y] ->
    ;; -> [X(Y)] -> 
    
  (define (subst-emits body fun vq)
    (core-generic-traverse
     (lambda (expr fallthrough)  ;; driver
       (match expr
	 [(emit ,vqueue ,[x]) `(app ,fun ,x ,vq)]
	 [(iterate . ,_)
	  (error 'merge-iterates:subst-emits "shouldn't have nested iterates! ~a" expr)]
	 [,other (fallthrough other)]))
     (lambda (ls k) (apply k ls)) ;; fuser
     body))

  ;; do full inlining of Y's body into X's emits
  (define (inline-subst-emits body fun fun-var fun-var-type)
    (core-generic-traverse
     (lambda (expr fallthrough)  ;; driver
       (match expr
         [(emit ,vqueue ,[x])
          `(let ((,fun-var ,fun-var-type ,x)) ,fun)]
         [(iterate . ,_)
          (error 'merge-iterates:inline-subst-emits "shouldn't have nested iterates! ~a" expr)]
         [,other (fallthrough other)]))
     (lambda (ls k) (apply k ls)) ;; fuser
     body))
             

  ;; merge the two lists of annotations
  ;; FIXME: we will probably need something more intelligent soon!
  #;
  (define (merge-annotations annot-outer annot-inner)
    (append annot-outer annot-inner))

  ;; stats for a merged box, ->[left]->[right]->  =>  ->[left->right]->
  (define (merge-data-rates left-stats right-stats)
    (make-bench-stats (bench-stats-bytes  right-stats)
                      (bench-stats-tuples right-stats)
                      (+ (bench-stats-cpu-time left-stats) (bench-stats-cpu-time right-stats))))

  (define (do-expr expr fallthrough)
    (match expr
      ;; [2006.11.24] rrn, NOTE: There seem to be some bugs in the pattern matcher 
      ;; related to (,foo ... ,last) patterns.  Avoiding them for now.

      ;; Modifying to not create free-variables in the introduced lambda abstraction.
      
      ;;
      ;; FIXME: this will keep the "merge-with-downstream" annotation from the upper box!!!
      ;;
      [(iterate (annotations . ,annoty) (let ,statey (lambda (,y ,VQY) (,ty (VQueue ,outy)) ,body))
		(iterate (annotations . ,annotx) (let ,statex
				   (lambda (,x ,VQX) (,tx (VQueue ,outx))
                                           ;; By convention the return-value is the vqueue:
					   ;(begin ,exprs ... ,return-val)
                                           ;; rrn: loosening this up, don't require that the body's a begin:
                                           ,bodx
                                           ))
			 ,inputstream))
       ;; don't merge indiscriminately; let an earlier pass tell us when to merge
       (guard (assq 'merge-with-downstream annoty))

       (log-opt "OPTIMIZATION: merge-iterates: merging nested iterates\n")
       
       ;; rrn: it was good to enforce this convention, but not doing it anymore:
					;(ASSERT (eq? return-val VQX)) 
       (do-expr
        `(iterate (annotations . ,(merge-annotations annoty
                                                     annotx
                                                     `([merge-with-downstream . right-only]
                                                       [data-rates manual ,merge-data-rates])))
                  (let ,(append statey statex) (lambda (,x ,VQY) (,tx (VQueue ,outy))
                                                       ,(inline-subst-emits
                                                         `(let ((,VQX (VQueue ,outy) ,VQY)) ,bodx)
                                                         `(begin ,body (tuple))
                                                         y ty)))
                  ,inputstream)
        fallthrough)]


      [(iterate ,annoty (lambda ,_ ...) (iterate ,annotx (lambda ,__ ...) ,___))
       (error 'merge-iterates "implementation problem, should have matched this but didn't: \n~s" 
	      `(iterate ,annoty (lambda ,_ ...) (iterate ,annotx (lambda ,__ ...) ,___)))]
      [,other (fallthrough other)]))


    [Expr do-expr])


;; perform the simple merge whenever possible;
;; this should be run after explicit-stream-wiring,
;; and after convert-to-multi-in-multi-out
;; (expects incoming/outgoing lists to have port-numbers attached to names)
(define new-simple-merge-policy:always
  (let ()

    ;;
    ;;
    (lambda (prog)
      (match prog
        [(,input-language
          '(graph ,consts
                  ,inits
                  ,sources
                  (operators (,op* (name ,n*)
                                   (output-type ,oty*)
                                   (code ,opcode*)
                                   (incoming ,oin** ...)
                                   (outgoing ,oout** ...))
                             ...)
                  ,sink
                  ,meta* ...))

         (let ([n->oin*  (map cons n* oin**)]
               [n->oout* (map cons n* oout**)])

           (define _ (pretty-print n->oin*))
           (define __ (pretty-print n->oout*))
           (define (simple-pipe-down? n op)
             (and (eq? op 'iterate)
                  (= 1 (length (cdr (or (assoc n n->oin*) `(,n)))))
                  (let ([downs (cdr (or (assoc n n->oout*) `(,n)))])
                    (and (= 1 (length downs))
                         (= 1 (length (cdr (or (assoc (caar downs) n->oin*) `(,n)))))))))

           (define (annotate-to-merge-down opcode)
             (match opcode
               [(iterate (annotations . ,annot) ,rest* ...)
                `(iterate (annotations . ,(merge-annotations '((merge-with-downstream)) annot))
                          ,@rest*)]))

           (let ([newopcode* (map (lambda (n op opcode)
                                    (if (simple-pipe-down? n op)
                                        (annotate-to-merge-down opcode)
                                        opcode))
                               n* op* opcode*)])

             `(,input-language
               '(graph ,consts
                       ,inits
                       ,sources
                       (operators (,op* (name ,n*)
                                        (output-type ,oty*)
                                        (code ,newopcode*)
                                        (incoming ,@oin**)
                                        (outgoing ,@oout**))
                                  ...)
                       ,sink
                       ,@meta*))))]))))


;;
;; this should be run after explicit-stream-wiring,
;; and after convert-to-multi-in-multi-out
;; (expects incoming/outgoing lists to have port-numbers attached to names)
(define new-simple-merge-iterates
  ;; -> [X] -> [Y] ->
  ;; -> [X(Y)] ->

  (let ()

    ;;
    (define (inline-subst-emits body fun fun-var fun-var-type old-idx-arg new-idx-arg)
      (core-generic-traverse
       (lambda (expr fallthru)
         (match expr
           [(_emit_to ,to ,props ,vq ,[x])
            `(let ((,fun-var ,fun-var-type ,x)) ,fun)]
           [,idx
            (guard (eq? idx old-idx-arg))
            new-idx-arg]
           [,oth (fallthru oth)]))
       (lambda (ls k) (apply k ls))
       body))

    ;; stats for a merged box, ->[left]->[right]-> => ->[left->right]->
    (define (merge-data-rates left-stats right-stats)
      (make-bench-stats (bench-stats-bytes right-stats)
                        (bench-stats-tuples right-stats)
                        (+ (bench-stats-cpu-time left-stats) (bench-stats-cpu-time right-stats))))

    ;;
    ;;
    (lambda (prog)

      ; FIXME: is this defined somewhere?
      (define (true-q pred ls)
        (cond [(null? ls) #f]
              [(pred (car ls)) ls]
              [else (true-q pred (cdr ls))]))

      (match prog
        [(,input-language
          '(graph ,consts
                  ,inits
                  ,sources
                  (operators ,oper* ...)
                  ,sink
                  ,meta* ...))

         (let ([get-oper-values
                (lambda (oper)
                  (match oper
                    [(,op (name ,n)
                          (output-type ,oty)
                          (code ,opcode)
                          (incoming ,oin* ...)
                          (outgoing ,oout* ...))
                     `(,op ,n ,oty ,opcode ,oin* ,oout*)]))])

         ; FIXME: there must be a better way to do this...

         (let ([get-op     (lambda (oper) (list-ref (get-oper-values oper) 0))]
               [get-n      (lambda (oper) (list-ref (get-oper-values oper) 1))]
               [get-oty    (lambda (oper) (list-ref (get-oper-values oper) 2))]
               [get-opcode (lambda (oper) (list-ref (get-oper-values oper) 3))]
               [get-oin*   (lambda (oper) (list-ref (get-oper-values oper) 4))]
               [get-oout*  (lambda (oper) (list-ref (get-oper-values oper) 5))])

           (define n->oper (make-eq-hashtable))

           (define (merge-down? oper)
             (match (get-opcode oper)
               [(iterate (annotations . ,annot) ,rest* ...)
                (annotq 'merge-with-downstream annot)]
               [,oth #f]))

           ; FIXME: subst the real code
           ; FIXME: do a name change *AND* fix up names in neighbors
           ; only handles one-arg/one-arg, along with the index args
           (define (merge-pipelined-boxes up-oper down-oper)

             ; match on both iterates at once
             (match `(,(get-opcode up-oper) ,(get-opcode down-oper))
               [((iterate
                  (annotations . ,up-annot)
                  (let ,up-state
                    (lambda (,up-idx-arg ,up-arg ,up-vq) (Int ,up-ty ,up-vqty)
                            (case ,up-idx-arg-dup
                              ((0) ,up-body))))
                  ,in-str)
                 
                 (iterate
                  (annotations . ,down-annot)
                  (let ,down-state
                    (lambda (,down-idx-arg ,down-arg ,down-vq) (Int ,down-ty ,down-vqty)
                            (case ,down-idx-arg-dup
                              ((0) ,down-body))))
                  ,mid-str))

                (let ([new-opcode
                       `(iterate
                         (annotations . ,(merge-annotations up-annot
                                                            down-annot
                                                            `([merge-with-downstream . right-only]
                                                              [data-rates manual ,merge-data-rates])))
                         (let ,(append up-state down-state)
                           (lambda (,down-idx-arg ,down-arg ,up-vq) (Int ,down-ty ,up-vqty)
                                   (case ,down-idx-arg
                                     ((0) ,(inline-subst-emits
                                            `(let ((,down-vq ,up-vqty ,up-vq)) ,down-body)
                                            `(begin ,up-body (tuple))
                                            up-arg up-ty up-idx-arg down-idx-arg)))))
                         ,in-str)])

                  `(iterate (name ,(get-n up-oper))
                            (output-type ,(get-oty down-oper))
                            (code ,new-opcode)
                            (incoming ,@(get-oin* up-oper))
                            (outgoing ,@(get-oout* down-oper))))]))

           ;; assuming upst-box can be merged with its downstream neighbor,
           ;; get that single downstream box
           (define (get-downstream-box upst-box)
             (hashtable-ref n->oper (caar (get-oout* upst-box)) #f))

           ;; fill in the hashtable
           (for-each (lambda (n oper) (hashtable-set! n->oper n oper))
             (map get-n oper*) oper*)
           
           ;; loop until done
           ;; not the fastest - but it hardly matters for now
           (let loop ()
             (let-values ([(n* oper*) (hashtable-entries n->oper)])
               (let ([upstr-boxes-to-merge
                      (filter merge-down? (vector->list oper*))])
                 
                 (void)
                 (if (null? upstr-boxes-to-merge)
                     (void)
                     (let* ([upstr-box (car upstr-boxes-to-merge)]
                            [downstr-box (get-downstream-box upstr-box)]
                            [new-box (merge-pipelined-boxes upstr-box downstr-box)])
                       
                       (hashtable-delete! n->oper (get-n downstr-box))
                       (hashtable-delete! n->oper (get-n upstr-box))
                       (hashtable-set! n->oper (get-n new-box) new-box)

                       ; update the incoming lists for any boxes that previous took input
                       ; from downstr-box
                       (for-each
                           (lambda (n)
                             (hashtable-update! n->oper n
                                                (lambda (oper)
                                                  (match oper
                                                    [(,op (name ,n)
                                                          (output-type ,oty)
                                                          (code ,opcode)
                                                          (incoming ,oin* ...)
                                                          (outgoing ,oout* ...))
                                                     (let ([new-incomings
                                                            (map (lambda (np)
                                                                   (if (eq? (car np) (get-n downstr-box))
                                                                       `(,(get-n new-box) . ,(cdr np))
                                                                       np))
                                                              oin*)])
                                                       
                                                       `(,op (name ,n)
                                                             (output-type ,oty)
                                                             (code ,opcode)
                                                             (incoming ,@new-incomings)
                                                             (outgoing ,@oout*)))]))
                                                #f))
                         (vector->list (hashtable-keys n->oper)))
                       
                       (loop)
                       ; FIXME: also need to update incoming/outgoing of neighbors
                       )))))

           (let-values ([(n* oper*) (hashtable-entries n->oper)])
             oper*)

           ))]))))
           

                                                   


             
                 
    

;; perform the simple merge whenever possible
(define-pass simple-merge-policy:always

    ;; merge the two lists of annotations
    ;; FIXME: we will probably need something more intelligent soon!
    #;
    (define (merge-annotations annot-outer annot-inner)
      (append annot-outer annot-inner))
  
    (define (do-expr expr fallthrough)
      (match expr
        [(iterate (annotations . ,annot-up) ,f-up
                  (iterate (annotations . ,annot-down) ,f-down ,in-str))

         `(iterate (annotations . ,(merge-annotations '((merge-with-downstream))
                                                      annot-up))
                   ,f-up
                   ,(do-expr
                     `(iterate (annotations . ,annot-down) ,f-down ,in-str)
                     fallthrough))]

        
        [,other (fallthrough other)]))


  [Expr do-expr])


;; EVEN OLDER ONE:
#;
(define simple-merge-iterates
  (build-compiler-pass ;; This wraps the main function with extra debugging
   'merge-iterates
   `(input)
   `(output) 
   (let ()
     (define (subst-emits body fun)
       (core-generic-traverse
	(lambda (expr fallthrough)
	  (match expr
	    [(emit ,vqueue ,x) `(app ,fun ,x)]
	    [(iterate . ,_)
	     (error 'merge-iterates:subst-emits "shouldn't have nested iterates! ~a" expr)]
	    [,other (fallthrough other)]))
	(lambda (ls k) (apply k ls))
	body))

     (define process-expr
       (lambda (expr)
	 (core-generic-traverse
	  (lambda (expr fallthrough) ;; driver
	    (match expr
	      [(iterate (lambda (,y) (,ty) 
				(letrec ([___VIRTQUEUE___ (VQueue ,outy) (virtqueue)])
				  ,body))
			(iterate (lambda (,x) (,tx) 
					 (letrec ([___VIRTQUEUE___ (VQueue ,outx) (virtqueue)])
					   ,bodx))
				 ,inputstream))
	       (let ([f (unique-name 'f)])
		 (process-expr
		  `(iterate (lambda (,x) (,tx)
				    (letrec ([___VIRTQUEUE___ (VQueue ,outy) (virtqueue)])
				      (letrec ([,f (,ty -> ,outy)
						   (lambda (,y) (,ty) ,body)])
					,(subst-emits bodx f))))
			    ,inputstream)))]
	      [,other (fallthrough other)]))
	  (lambda (ls k) (apply k ls)) ;; fuser
	  expr)))

     ;; Main pass body:
     (lambda (expr)
       (match expr
	 [(,input-language (quote (program ,body ,type)))
	  (let ([body (process-expr body)])
	    `(merge-iterates-language '(program ,body ,type)))])))))






;; ============================================================================================== ;;
;; TESTING:

(define-testing test-simple-merge-iterates
  (default-unit-tester "Merge-Iterates: collapse adjacent iterates"      
  `(
    ["Peform basic iterate merging."
     (length (deep-assq-all 'iterate
	       (simple-merge-iterates 
		'(foolang 
		  '(program (iterate (annotations (merge-with-downstream)) 
				     (let () 
				       (lambda (x vq1) (Int (VQueue Int))
					       (begin (emit vq1 (_+_ x 1)) 
						    (emit vq1 (_+_ x 100))
						    vq1)))
				     (iterate (annotations)
					      (let ()
						(lambda (y vq2) (Int (VQueue Int))
							(begin (emit vq2 (*_ y 2))
							       (emit vq2 (*_ y 3))
							       vq2)))
				      SOMESTREAM))
		     T)))))
     ;; Simply verify that it reduces two iterates to one.
     1]
    )))


) ;; End module
