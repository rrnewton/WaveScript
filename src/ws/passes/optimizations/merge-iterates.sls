#!r6rs

;;;; Pass: Merge Iterates
;;;; .author RRN & MIC
;;;; 

;;;; rrn: NOTE, this introduces (app ...).  Originally, user applications
;;;; of functions were gone by this juncture.

;;;; mic: should be done only after the standardize-iterate pass

(library (ws passes optimizations merge-iterates)
  (export merge-iterates)
  (import (rnrs) (rnrs mutable-pairs)(ws common))

   ;; -> [X] -> [Y] ->
   ;; -> [X(Y)] -> 

  ; FIXME: this *has* to be defined somewhere else
  (define first-index
    (case-lambda
      [(e ls) (first-index e ls 0)]
      [(e ls i)
       (cond ((null? ls) #f)
             ((eq? e (car ls)) i)
             (else (first-index e (cdr ls) (+ i 1))))]))


  ; FIXME: this also *has* to be defined somewhere else
  (define (first-true pred ls)
    (cond ((null? ls) #f)
          ((pred (car ls)) (car ls))
          (else (first-true pred (cdr ls)))))


  ;; assumes a (begin ... vq) is embedded within some lets
  (define (remove-vq-return body)
    (core-generic-traverse
     (lambda (expr fallthrough)  ;; driver
       (match expr
         [(let ,bindings ,[inner-body]) `(let ,bindings ,inner-body)]
         [(begin ,exprs ...) `(begin ,(rdc exprs) ...)]
         [,other (fallthrough other)]))
     (lambda (ls k) (apply k ls)) ;; fuser
     body))
    
  
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

  (define (subst-var v-before v-after body)
    (core-generic-traverse
     (lambda (expr fallthrough)  ;; driver
       (match expr
         [,var (guard (and (eq? var v-before)))
               v-after]
         [,other (fallthrough other)]))
     (lambda (ls k) (apply k ls)) ;; fuser
     body))

  ; FIXME: don't repeat the remove-vq-return
  (define (subst-emits2 outer-body var inner-body)
    (core-generic-traverse
     (lambda (expr fallthrough)  ;; driver
       (match expr
         [(emit ,vqueue ,[x]) (subst-var var x (remove-vq-return inner-body))]
         [,other (fallthrough other)]))
     (lambda (ls k) (apply k ls)) ;; fuser
     outer-body))

  (define (subst-emit-vqs body new-queue-name)
    (core-generic-traverse
     (lambda (expr fallthrough)  ;; driver
       (match expr
         [(emit ,vqueue ,[x]) `(emit ,new-queue-name ,x)]
         [,other (fallthrough other)]))
     (lambda (ls k) (apply k ls))  ;; fuser
     body))

  ; FIXMEFIXME: if the expression in an emit is *not* a variable reference,
  ;             then we will be running ops. twice -- could be incorrect behavior with state!
  (define (subst-and-multiply-emits body queues)
    (let ((new-body
    (core-generic-traverse
     (lambda (expr fallthrough)  ;; driver
       (match expr
         [(emit ,vqueue ,[x]) (cons 'begin (map (lambda (q) `(emit ,q ,x)) queues))]
         [,other (fallthrough other)]))
     (lambda (ls k) (apply k ls))  ;; fuser
     body)

    ))
      new-body
      )
    )

  ;; only substitute emits on the given stream str
  ;; FIXME: may be some bugs with state... if they're embedded in the emits...
  ; FIXME: don't repeat the remove-vq-return
  (define (subst-some-emits outer-body var inner-body str)
    (core-generic-traverse
     (lambda (expr fallthrough)  ;; driver
       (match expr
         [(emit ,emit-str ,[x]) (guard (eq? emit-str str)) (subst-var var x (remove-vq-return inner-body))]
         [,other (fallthrough other)]))
     (lambda (ls k) (apply k ls)) ;; fuser
     outer-body))

  ;; this really just inlines the behavior of unionN
  (define (subst-some-emits-with-tuples body mid-strs out-str out-elt-type)
    (core-generic-traverse
     (lambda (expr fallthrough)  ;; driver
       (match expr
         [(emit ,emit-str ,[x])
          (let ((i (first-index emit-str mid-strs)))
            (if i
                (let ((tmp-name (unique-name 'tmp)))
                  `(let ([,tmp-name ,out-elt-type (tuple ',i ,x)])
                     (emit ,out-str ,tmp-name)))
                `(emit ,emit-str ,x)))]
         [,other (fallthrough other)]))
     (lambda (ls k) (apply k ls)) ;; fuser
     body))
          


  (define (do-expr expr fallthrough)
    (match expr
      ;; [2006.11.24] rrn, NOTE: There seem to be some bugs in the pattern matcher 
      ;; related to (,foo ... ,last) patterns.  Avoiding them for now.

      ;; Modifying to not create free-variables in the introduced lambda abstraction.
      [(iterate (lambda (,y ,VQY) (,ty (VQueue ,outy)) ,body)
		(iterate (lambda (,x ,VQX) (,tx (VQueue ,outx))
				 ;; By convention the return-value is the vqueue:				 
				 ;(begin ,exprs ... ,return-val)
				 ;; rrn: loosening this up, don't require that the body's a begin:
				 ,bodx
				 )
			 ,inputstream))
       ;; rrn: it was good to enforce this convention, but not doing it anymore:
       ;(ASSERT (eq? return-val VQX)) 
       (let ([f (unique-name 'f)])
	 (do-expr
	  `(iterate (lambda (,x ,VQX) (,tx (VQueue ,outy))
			    (letrec ([,f (,ty (VQueue ,outy) -> #())
					 (lambda (,y ,VQY) (,ty (VQueue ,outy))
						 (begin ,body (tuple)))])
			      ,(subst-emits ;`(begin ,@exprs ,VQX)
				            `(begin ,@bodx ,VQX)
					    f VQX)))
		    ,inputstream)
	  fallthrough))]

      [(let ([,stream1-out ,type1 (iterate (let ,state1 (lambda
                                                            (,x ,vqx)
                                                            (,tx ,tvqx)
                                                            ,body1)) ,stream0)])
         (let ([,stream2-out ,type2 (iterate
                                     (let ,state2 (lambda
                                                      (,y ,vqy)
                                                      (,ty ,tvqy) (begin ,body2-exprs ...))) ,stream1-in)])
           ,full-body) )

       (guard (eq? stream1-out stream1-in))

       (let ()
         (printf "it's a match!\n")
         ;; rdc below chops off the virtqueue return
         (do-expr
          `(let ([,stream2-out ,type2 (iterate (let ,(append state1 state2)
                                                 (lambda
                                                     (,x ,vqx)
                                                     (,tx ,tvqx)
                                                     ,(subst-var
                                                       vqy vqx
                                                       (subst-emits2 body1 y (rdc `(begin ,body2-exprs ...))))))
                                               ,stream0)])
             ,full-body)
          fallthrough)
         )]

      [(iterate (lambda ,_ ...) (iterate (lambda ,__ ...) ,___))
       (error 'merge-iterates "implementation problem, should have matched this but didn't: \n~s" 
	      `(iterate (lambda ,_ ...) (iterate (lambda ,__ ...) ,___)))]
      [,other (fallthrough other)]))

;(define-pass merge-iterates [Expr do-expr])


;;
;; FIXME: does not currently work if the graph has one box outputting multiple times into another box
;;
;;
;;
(define-pass merge-iterates
    [Program (lambda (prog Expr)

               ;; define the graph instance and its accessors
               ;;
               ; FIXME: helper, move elsewhere
               (define (all-eq? ls)
                 (or (null? ls) (andmap (lambda (x) (eq? (car ls) x)) (cdr ls))))


               (define boxes (make-default-hash-table))
               (define streams (make-default-hash-table))

               (define (make-box name defn) (list name defn '() '()))
               (define box-name car)
               (define box-defn cadr)
               (define box-in-strs caddr)
               (define box-out-strs cadddr)
               (define (set-box-defn! box defn) (set-car! (cdr box) defn))
               (define (set-box-in-strs! box in-strs) (set-car! (cddr box) in-strs))
               (define (set-box-out-strs! box out-strs) (set-car! (cdddr box) out-strs))

               (define (make-stream name in-box out-box-names elt-type)
                 (list name in-box (map (lambda (bn) (hashtab-get boxes bn)) out-box-names) elt-type))
               (define stream-name car)
               (define stream-in-box cadr)
               (define stream-out-boxes caddr)
               (define stream-elt-type cadddr)
               (define (set-stream-in-box! str box) (set-car! (cdr str) box))

               (define (add-new-box! name box-defn) (hashtab-set! boxes name (make-box name box-defn)))
               (define (add-new-stream! name in-box-name elt-type)
                 (let* ((in-box (hashtab-get boxes in-box-name))
                        (new-stream (make-stream name in-box '() elt-type)))
                   (hashtab-set! streams name new-stream)
                   (set-car! (cdddr in-box) (cons new-stream (cadddr in-box)))))
                   
               (define (add-stream-output-edge! name out-box-name)
                 (let* ((str (hashtab-get streams name))
                        (new-out-box (hashtab-get boxes out-box-name)))
                   (set-car! (cddr str) (cons new-out-box (caddr str)))
                   (set-car! (cddr new-out-box) (cons str (caddr new-out-box)))))
               
               (define (get-out-stream-box-defn out-str-name)
                 (let ((str (hashtab-get streams out-str-name)))
                   (if str (box-defn (stream-in-box str)) #f)))


               
               ;;
               ;; FIXME: the lambda passed here does multiple redundant lookups...
               ;; FIXME: is BASE an appropriate name to hard-code here?
               ;;
               (build-box-graph
                 prog
                 (lambda (box-name out-str-name box-defn . in-str-names)
                   (add-new-box! box-name box-defn)
                   (add-new-stream! out-str-name box-name (cadar box-defn))
                   (for-each (lambda (sn) (add-stream-output-edge! sn box-name)) in-str-names))
                 (lambda (s)
                   (add-new-box! 'BASE s)
                   (add-stream-output-edge! s 'BASE)))



               ;;
               ;;
               ;; update the stream graph
               ;; right now, just linear merging
               ;;
               ;;
               (let loop ()
                 (let ((changed #f))
                   
;                   (printf "loop\n")
                   (hashtab-for-each
                    boxes
                    (lambda (bn b) 
                      (if (and (= 1 (length (box-out-strs b)))
                               (= 1 (length (stream-out-boxes (car (box-out-strs b)))))
                               (not (eq? (box-name (car (stream-out-boxes (car (box-out-strs b))))) 'BASE)) ; FIXME: hack!
                               (= 1 (length (box-in-strs (car (stream-out-boxes (car (box-out-strs b)))))))
                               (= 1 (length (box-in-strs b)))
                               (= 1 (length (stream-out-boxes (car (box-in-strs b))))))

                          ; ok, can fuse b with its one forward neighbor
                          (begin

                            ; FIXME: for now, assuming they're both iterates
                            (let* ((upstream-box (box-defn b))
                                   (downstream-box (box-defn (car (stream-out-boxes (car (box-out-strs b))))))
                                   (fused-box
                                    ; FIXME: would like a cleaner way to match on both at the same time
                                    (match (list upstream-box downstream-box)
                                      [((,up-type (iterate
                                                   (let ,up-state (lambda (,x ,vqx) (,tx ,tvqx) ,up-body))
                                                   ,in-str))
                                        (,down-type (iterate
                                                     (let ,down-state (lambda
                                                                          (,y ,vqy) (,ty ,tvqy) ,down-body))
                                                     ,mid-str)))

                                       `(,down-type (iterate
                                                     (let ,(append up-state down-state)
                                                       (lambda (,x ,vqy) (,tx ,tvqy)
                                                               ,(subst-var
                                                                 vqx vqy
                                                                 (subst-emits2 up-body y
                                                                               down-body))))
                                                     ,in-str))]

                                      ; FIXME: produces a failure if, e.g., downstr. box is a unionN
                                      ;        with just one input
                                      [,oth (begin
                                              (error "not the right form for the boxes to be fused!"
                                                   (symbol->string bn)))])))

                              
                              ; update our tables with the fused-box
                              (hashtab-remove! boxes (box-name (car (stream-out-boxes (car (box-out-strs b))))))
                              (hashtab-remove! streams (stream-name (car (box-out-strs b))))
                              (set-box-defn! b fused-box)

                              (set-box-out-strs! b (box-out-strs (car (stream-out-boxes (car (box-out-strs b))))))
                              (set-stream-in-box! (car (box-out-strs b)) b)

                              (set! changed #t))))))
                 
                   (if changed (loop))))


               ;;
               ;; convert the boxes to the temporary multi-out structure
               ;; (no boxes yet have multiple outputs)
               ;;
;               (printf "*** converting to multi-out...\n")
               (hashtab-for-each
                boxes
                (lambda (bn b)
                  (set-box-defn! b
                    (match (box-defn b)
                      [#;
                       (,out-type (iterate (let ,state (lambda (,x ,vq) (,tx ,tvq) (begin ,body ...))) ,in-str))
                       
                       (,out-type (iterate (let ,state (lambda (,x ,vq) (,tx ,tvq) ,body)) ,in-str))
                       
                       #;
                       (let ([exprs (rdc body)]) ; rdc removes ___VIRTQUEUE___ return at end
                         `(iterate (let ,state (lambda
                                                 (,x) (,tx)
                                                 (begin ; FIXME: not sure why this begin is necessary...
                                                    ,(subst-emit-vqs
                                                      `(begin ,@exprs)
                                                      (stream-name (car (box-out-strs b)))))))
                                   ,in-str))

                       #;
                       (let ([exprs (rdc body)]) ; rdc removes ___VIRTQUEUE___ return at end
                         `(iterate (let ,state (lambda
                                                 (,x) (,tx)
                                                 (begin ; FIXME: not sure why this begin is necessary...
                                                   ,(subst-emit-vqs
                                                     `(begin ,@exprs)
                                                     (stream-name (car (box-out-strs b)))))))))

                       `(iterate (let ,state (lambda
                                               (,x) (,tx)
                                               ,(subst-emit-vqs body (stream-name (car (box-out-strs b)))))))
                       ]

                      [,oth oth]))
                  
                  ))

               ;;
               ;; now unique-ify streams
               ;;
               ;; FIXME: do it
               (let ((all-new-streams '())
                     (all-old-streams '()))
                 (hashtab-for-each
                  streams
                  (lambda (sn s)
                    (let ((out-boxes (stream-out-boxes s))
                          (in-box (stream-in-box s))
                          (new-streams '()))
                      (if (> (length out-boxes) 1)
                          ;; do the splitting for this stream
                          (begin
                            (set! new-streams
                                  (map (lambda (b) (make-stream (unique-name 's) in-box
                                                                (list (box-name b)) (stream-elt-type s)))
                                       out-boxes))


                            #;
                            (for-each (lambda (s b)
                                        (let ((new-box-defn
                                               (match (box-defn b)
                                                 [(iterate (let ,state
                                                             (lambda (,x) (,tx)
                                                                     (begin ,body ...)))
                                                           ,in-str)

                                                  `(iterate (let ,state
                                                              (lambda (,x) (,tx)
                                                                      (begin ; FIXME: this begin also
                                                                        ,(subst-and-multiply-emits
                                                                          `(begin ,body ...)
                                                                          (map stream-name new-streams)))))
                                                            ,in-str)]


                                                 [,oth oth])))
                                          
                                          (set-box-defn! b new-box-defn)
                                          (set-box-in-strs! b (list s)) ; FIXME: incorrect!!!
                                          ))
                              new-streams out-boxes)


                            (let ((new-box-defn
                                   (match (box-defn in-box)
                                     [
                                      #;
                                      (iterate (let ,state
                                                 (lambda (,x) (,tx) (begin ,body ...))))
;                                               ,in-str)

                                      (iterate (let ,state
                                                 (lambda (,x) (,tx) ,body)))

                                      #;
                                      `(iterate (let ,state
                                                  (lambda (,x) (,tx)
                                                          (begin ; FIXME: this begin also
                                                            ,(subst-and-multiply-emits
                                                              `(begin ,body ...)
                                                              (map stream-name new-streams)))))
;                                                ,in-str
                                                )

                                      `(iterate (let ,state
                                                  (lambda (,x) (,tx) ,(subst-and-multiply-emits
                                                                        body
                                                                        (map stream-name new-streams)))))
                                      ]
                                     
                                     ;; only iterate boxes will have multiple outputs
                                     [,oth oth])))
                              
                              (set-box-defn! in-box new-box-defn)

                              ; FIXME: this is *incorrect* if in-box feeds the same stream into an out-box
                              ;        multiple times!!!
                              (for-each (lambda (s b) (set-box-in-strs! b (list s))) new-streams out-boxes)
                            
                              (set-box-out-strs! in-box new-streams)
                          
                              (set! all-new-streams (append new-streams all-new-streams))
                              (set! all-old-streams (cons s all-old-streams))
                          
                          ))))))

                 ; remove the old streams here
                 (for-each (lambda (s) (hashtab-remove! streams (stream-name s))) all-old-streams)

                 ; add the new streams here
                 (for-each (lambda (s) (hashtab-set! streams (stream-name s) s)) all-new-streams)

                 )


               ;;
               ;; now, every stream feeds into only one box
               ;;


               ;;
               ;; do the harder loop
               ;;
               (let loop ()
                 (let ((changed #f))
                   
;                   (printf "loop (hard)\n")
                   (hashtab-for-each
                    boxes
                    (lambda (bn b)
                      ; check that all of the input streams for b come from the same box
                      (if (and (not (eq? bn 'BASE)) ; FIXME: hack!
                               (all-eq? (map stream-in-box (box-in-strs b)))
                               (not (null? (box-in-strs b)))
                               ;; don't try to merge if the upstream box is the source, or has multiple
                               ;; inputs itself
                               (= 1 (length (box-in-strs (stream-in-box (car (box-in-strs b)))))))

                          (let* ((down-box b)
                                 (up-box (stream-in-box (car (box-in-strs down-box))))
                                 ;(mid-stream (car (box-in-strs down-box)))
                                 (mid-streams (box-in-strs down-box))
                                 (fused-box-defn

                                  (begin

                                  (match (box-defn b)

                                    ;; this handles multi-input, unionN downstream boxes
                                    ;; FIXME: fix up the types of the output streams!
                                    ;;        (i think we can remove them from the intermediate form,
                                    ;;         and change them on the streams only...)
                                    [(,out-str-type (unionN ,mid-strs ...))
                                     
                                     (match (box-defn up-box)
                                       [
                                        #;
                                        (iterate (let ,up-state (lambda (,x) (,tx) (begin ,up-body ...)))
;                                                 ,in-str
                                                 )

                                        (iterate (let ,up-state (lambda (,x) (,tx) ,up-body)))
                                        
                                        #;
                                        `(iterate (let ,up-state
                                                    (lambda
                                                        (,x) (,tx)
                                                        ,(subst-some-emits-with-tuples
                                                          `(begin ,up-body ...)
                                                          `(,mid-strs ...)
                                                          (stream-name (car (box-out-strs down-box))))))
;                                                  ,in-str
                                                  )
                                        
                                        `(iterate (let ,up-state
                                                    (lambda
                                                        (,x) (,tx)
                                                        ,(subst-some-emits-with-tuples
                                                           up-body
                                                           mid-strs
                                                           (stream-name (car (box-out-strs down-box)))
                                                           (stream-elt-type (car (box-out-strs down-box)))))))
                                        ]
                                       ; FIXME: need to make sure all those mid-strs are removed!


                                       [,oth (error "upstream box has unknown form"
                                                    (symbol->string (box-name up-box)))])
                                     ]

                            
                                    ;; this handles one-input downstream boxes
                                    [
                                     #;
                                     (iterate (let ,down-state (lambda (,y) (,ty) (begin ,down-body ...)))
;                                              ,mid-str
                                              )
                                     
                                     (iterate (let ,down-state (lambda (,y) (,ty) ,down-body)))
                                    
                                     (match (box-defn up-box)
                                       [
                                        #;
                                        (iterate (let ,up-state (lambda (,x) (,tx) (begin ,up-body ...)))
;                                                 ,in-str
                                                 )

                                        (iterate (let ,up-state (lambda (,x) (,tx) ,up-body)))
                                        
                                        #;
                                        `(iterate (let ,(append up-state down-state)
                                                    (lambda
                                                        (,x) (,tx)
                                                        ,(subst-some-emits `(begin ,up-body ...) 
                                                                          y `(begin ,down-body ...)
                                                                          (stream-name (car mid-streams)))))
;                                                  ,in-str
                                                  )

                                        `(iterate (let ,(append up-state down-state)
                                                    (lambda
                                                        (,x) (,tx)
                                                        ,(subst-some-emits up-body y down-body
                                                                           (stream-name (car mid-streams))))))
                                        ]

                                      
                                       [,oth 
                                        (begin
                                          (error "upstream box has unknown form" (list->string
                                                                                      (box-defn up-box))))])]
                                      

                                    [,oth (error "downstream box has unknown form" (symbol->string bn))]) ) ))

                            ;; update our tables with the fused-box
;                            (printf "*** removing...")
                            (hashtab-remove! boxes (box-name down-box)) ; FIXME: is this dangerous?
                            (for-each (lambda (ms) (hashtab-remove! streams (stream-name ms))) mid-streams)
;                            (hashtab-remove! streams (stream-name mid-stream))
                            (set-box-defn! up-box fused-box-defn)
                            
                            ; FIXME: should be able to use remove!
                            (for-each (lambda (ms) (set-box-out-strs! up-box (remove ms (box-out-strs up-box))))
                                      mid-streams)
                            (set-box-out-strs! up-box (append (box-out-strs up-box) (box-out-strs down-box)))
;                            (for-each (lambda (ms)
;                                        (set-box-out-strs! up-box (append (remove ms (box-out-strs up-box))
;                                                                          (box-out-strs down-box))))
;                                      mid-streams)
;                            (set-box-out-strs! up-box
;                                               (append (remove mid-stream (box-out-strs up-box))
;                                                       (box-out-strs down-box)))
                            (for-each (lambda (s) (set-stream-in-box! s up-box)) (box-out-strs down-box))

                            (set! changed #t)
;                            (printf "done.\n")

                            ))))

                   (if changed (loop))))

;               (printf "DONE with hard loop\n")
               

               ;;
               ;; convert the boxes back to regular single-out structure
               ;; (no boxes should have multiple outputs now)
               ;; FIXME: would like to give an error if there is a multi-out box still
               ;;
               (hashtab-for-each
                boxes
                (lambda (bn b)
                  (set-box-defn! b
                    (match (box-defn b)
                      
                      #;
                      [((,out-type) (iterate (let ,state (lambda (,x (,vq)) (,tx (,tvq)) ,body ...)) ,in-str))
                       `(,out-type (iterate (let ,state (lambda (,x ,vq) (,tx ,tvq) ,body ...)) ,in-str))]

                      [
                       #;
                       (iterate (let ,state (lambda (,x) (,tx) (begin ,body ...)))
;                                ,in-str
                                )

                       (iterate (let ,state (lambda (,x) (,tx) ,body)))


                       (let ((out-type (stream-elt-type (car (box-out-strs b))))
                             (new-vq (unique-name '___VIRTQUEUE___)))
                         (ASSERT (= 1 (length (box-in-strs b))))
                         `((Stream ,out-type)
                           (iterate (let ,state
                                      (lambda (,x ,new-vq)
                                              (,tx (VQueue ,out-type))
                                              #;
                                              ,(append `(begin ,(subst-emit-vqs `(begin ,body ...) new-vq))
                                                       `(,new-vq))
                                              ,(append `(begin ,(subst-emit-vqs
                                                                  (remove-vq-return body)
                                                                  new-vq))
                                                       `(,new-vq))
                                              ))
;                                    ,in-str
                                    ,(stream-name (car (box-in-strs b)))
                                    )))]

            
                      [,oth oth]))))


               ;; rebuild the program: take out boxes that are not in the updated graph,
               ;; and for those that are still in the graph, replace their bodies as appropriate
               (rewrite-program-from-graph prog get-out-stream-box-defn)


             )])


;;
;; FIXME: may be able to write this with [] recursion
;;
(define-pass lift-nested-boxes
    (define Expr (lambda (x fallthru)
            (match x

              [(let ([,out-str ,out-str-type (iterate (let ,state (lambda (,x ,vq) (,tx ,tvq) ,body-exprs ...)) ,in-str)]) ,rest-of-prog)
               (guard (not (symbol? in-str)))
               (Expr
                (let ((new-str-name (unique-name 's)))
                  `(let ([,new-str-name (Stream ,tx) ,in-str])
                     (let ([,out-str ,out-str-type (iterate (let ,state (lambda (,x ,vq) (,tx ,tvq) ,body-exprs ...)) ,new-str-name)]) ,rest-of-prog)))
                fallthru)]


              [(let ([,out-str ,out-str-type (unionN ,in-strs ...)]) ,rest-of-prog)

               (let ((first-inlined (first-true (lambda (s) (not (symbol? s))) in-strs)))
                 (ASSERT (and (vector? (cadr out-str-type))
                              (= 2 (vector-length (cadr out-str-type)))))
                 (if first-inlined
                     (let ((new-str-name (unique-name 's)))
                       (Expr `(let ([,new-str-name (Stream ,(vector-ref (cadr out-str-type) 1)) ,first-inlined])
                                (let ([,out-str ,out-str-type (unionN ,(subst new-str-name first-inlined in-strs))])
                                  ,rest-of-prog))
                             fallthru))))]
                                                      

              [,oth (fallthru oth)])))
  
  [Expr Expr])


;; FIXME: be sure to handle readFile, unionN/unionList, _merge, and others?
;; 
(define (build-box-graph prog add-box! set-base!)
  (define-pass build-box-graph
      (define Expr (lambda (x fallthru)
              (match x
                [(let ([,out-str ,out-str-type (iterate (let ,state ,func) ,in-str)])
                   ,rest-of-prog)
                 
                 (begin
                   
                   (add-box! (unique-name 'it) out-str
                             `(,out-str-type (iterate (let ,state ,func) ,in-str)) in-str)

                   (Expr rest-of-prog fallthru))]


                [(let ([,out-str ,out-str-type (assert-type
                                                ,out-str-type-assert
                                                (readFile ,file ,args))])
                   ,rest-of-prog)

                 (begin

                   (add-box! (unique-name 'it) out-str
                             `(,out-str-type (assert-type ,out-str-type-assert (readFile ,file ,args))))

                   (Expr rest-of-prog fallthru))]


                [(let ([,out-str ,out-str-type (unionN ,in-strs ...)])
                   ,rest-of-prog)

                 (begin
                   (apply add-box!
                     `(,(unique-name 'it) ,out-str (,out-str-type (unionN ,in-strs ...)) ,in-strs ...))
                   (Expr rest-of-prog fallthru))]


                ;; FIXME: is BASE an appropriate name?
                ;; FIXME: this will be called multiple times...
                [,s (guard (symbol? s))
                    
                    (begin
                      (set-base! s)
                      )]


                [,oth (fallthru oth)])))
    
    [Expr Expr])

  (build-box-graph prog))


;;
;; get-box-definition should return structure like (type box), e.g.
;;   ((Stream Int) (iterate ...))
;;
;; this would not work with fission, because we traverse the original
;; program and look for stream names there, with the assumption that
;; the "fused" program just-reused those names
;;
(define (rewrite-program-from-graph prog get-box)
  (define-pass rewrite-program-from-graph
      (define Expr (lambda (x fallthru)
                     (match x
                       
                       [(let ([,s (Stream ,type) ,box])
                          ,body)
                        
                        (let ((box (get-box s)))
                          (if box
                              `(let (,[cons s box]) ,(Expr body fallthru))
                              (Expr body fallthru)))]

                       [,oth (fallthru oth)])))

    [Expr Expr])

  (rewrite-program-from-graph prog))

) ;; End module
