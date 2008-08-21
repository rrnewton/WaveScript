#!r6rs

;;;; Pass: Merge Iterates 2
;;;; .author MIC
;;;;

;;;; this pass expects to follow explicit-stream-wiring

(library (ws passes optimizations merge-iterates2)
  (export ;merge-iterates2
          convert-to-multi-in-multi-out ; FIXME: temporarily exported
          make-output-streams-unique    ; FIXME: temporarily exported
          ;merge-two-boxes               ; FIXME: temporarily exported
          rewrite-merges-as-iterates    ; FIXME: temporarily exported
          )
  (import (except (rnrs (6)) error) (rnrs mutable-pairs)
          (ws common)
          (ws util helpers))


;;
;; expects output from explicit-stream-wiring
;;
;; inserts index arg., and wraps body in case statement (with one case, for index = 0);
;; also rewrites outgoing and ingoing lists with index numbers
;;
;; NOTE: this depends subtlely, but critically, on being run when the only multi-input
;;       boxes are merges (two input streams) -- this is so that input index numbers can
;;       be specified correctly. (this is information not actually available from
;;       the explicit-stream-wiring pass -- but also not really meaningful at that point).
;;
;; NOTE: also, duplicates are not checked here - this should (probably) be switched on
;;       in explicit-stream-wiring pass (although it could be done here)
;;
(define convert-to-multi-in-multi-out
  (let ()

    ;;
    ;;
    (define Expr
      (core-generic-traverse
       (lambda (x fallthru)
         (match x
           [(iterate (annotations . ,annot)
                     (let ,bind* (lambda ,arg* ,argty* ,body* ...))
                     ,in-str)
            
            (let ([index-arg (unique-name 'index)])
              `(iterate (annotations ,@annot)
                        (let ,bind* (lambda (,index-arg ,@arg*)
                                            (Int ,@argty*)
                                            (case ,index-arg
                                              [(0) ,@body*])))
                        ,in-str))]

           [,oth (fallthru oth)]
           ))))

    ;;
    ;;
    (lambda (prog)
      (match prog
        [(,input-language
          '(graph ,consts
                  ,inits
                  (sources ((name ,src-n*)
                            (output-type ,src-oty*)
                            (code ,src-code*)
                            (outgoing ,src-out** ...))
                           ...)
                  (operators (,op* (name ,n*)
                                   (output-type ,oty*)
                                   (code ,opcode*)
                                   (incoming ,oin** ...)
                                   (outgoing ,oout** ...))
                             ...)
                  (sink ,sink-in ,sinkty)
                  ,meta* ...))

         (let* ([newopcode* (map (lambda (opcode) (Expr opcode))
                              opcode*)]
                [orderings (map (lambda (n oin*)
                                  (cons n (mapi (lambda (i oin) `(,oin . ,i)) oin*)))
                             `(BASE ,@n*) `((,sink-in) ,@oin**))]
                [_ (printf "Oooh but orderings is ~a\n" orderings)]
                
                ; this is to insert duplicates where appropriate
                ; FIXMEFIXME: this is basically a (nasty) temporary hack - it's probably better
                ;             to institute the change in explicit-stream-wiring
                #;
                [oout**-duped (map (lambda (n oout*)
                                     (apply append
                                            (map (lambda (oout)
                                                   (n-times (lambda () `(,oout))
                                                            (length (filter
                                                                        (lambda (p) (eq? (car p) n))
                                                                      (cdr (assoc oout orderings))))))
                                              oout*)))
                                n* oout**)]

                [newoout** (map (lambda (n oout*)
                                  (map (lambda (oout)
                                         (let ([in-portnum (cdr (assoc n (cdr (assoc oout orderings))))])
                                           ; FIXME: this set-cdr! is a slight bug fix
                                           (set-cdr! (assoc n (cdr (assoc oout orderings))) (+ 1 in-portnum))
                                           `(,oout . ,in-portnum)))
                                    oout*))
                             n*
                             oout** ;oout**-duped
                             )]

                [newoin** (map (lambda (oin*)
                                 ; only one output so far, so the output-port number is always 0
                                 (map (lambda (oin) `(,oin . 0)) oin*))
                            oin**)]

                ; FIXMEFIXME: same nasty hack as above
                #;
                [src-out**-duped (map (lambda (n src-out*)
                                        (apply append
                                               (map (lambda (src-out)
                                                      (n-times (lambda () `(,src-out))
                                                               (length (filter
                                                                           (lambda (p) (eq? (car p) n))
                                                                         (cdr (assoc src-out orderings))))))
                                                 src-out*)))
                                   src-n* src-out**)]                                

                [newsrc-out** (map (lambda (n src-out*)
                                     (map (lambda (src-out)
                                            (let ([in-portnum (cdr (assoc n (cdr (assoc src-out orderings))))])
                                              ; FIXME: this set-cdr! is the same bug fix as above
                                              (set-cdr! (assoc n (cdr (assoc src-out orderings)))
                                                        (+ 1 in-portnum))
                                              `(,src-out . ,in-portnum)))
                                       src-out*))
                                src-n*
                                src-out** ;src-out**-duped
                                )])

           `(,input-language
             '(graph ,consts
                     ,inits
                     (sources ((name ,src-n*)
                               (output-type ,src-oty*)
                               (code ,src-code*)
                               (outgoing ,@newsrc-out**))
                              ...)
                     (operators (,op* (name ,n*)
                                      (output-type ,oty*)
                                      (code ,newopcode*)
                                      (incoming ,@newoin**)
                                      (outgoing ,@newoout**))
                                ...)
                     (sink ,sink-in ,sinkty)
                     ,@meta*)))]))))


;;
;; rewrites _merge operators as multi-input iterates
;;
(define rewrite-merges-as-iterates
  (let ()
  
    ;;
    ;;
    (define (Expr type outstrs)
      (core-generic-traverse
       (lambda (x fallthru)
         (match x
           [(_merge (annotations . ,annot) ,in1 ,in2)

            (let ([index-arg (unique-name 'index)]
                  [arg1 (unique-name 'x)]
                  [arg2 (unique-name 'x)]
                  [vq-arg (unique-name "___VIRTQUEUE___")])
              `(iterate (annotations ,@annot)
                        (let ()
                          (lambda (,index-arg ,arg1 ,arg2 ,vq-arg)
                            (Int ,type ,type (VQueue ,type))
                            (case ,index-arg
                              [(0) (_emit_to 0 '() (assert-type (VQueue ,type) ,vq-arg) ,arg1)]
                              [(1) (_emit_to 0 '() (assert-type (VQueue ,type) ,vq-arg) ,arg2)])))
                        ,in1))] ; FIXME: this is meaningless, now

           ;; FIXME: this is only here because i haven't changed core-generic-traverse yet
           ;;        to handle case statements
           [(iterate ,rest* ...) `(iterate ,@rest*)]

           [,oth (fallthru oth)]
           ))))

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

         (let ([newopcode* (map (lambda (opcode oty outstrs) ((Expr oty outstrs) opcode))
                             opcode*
                             (map cadr oty*) ; FIXME: this is hacky way of getting the base type of a Stream
                             oout**
                             )]
               [newop* (map (lambda (op) (if (eq? op '_merge) 'iterate op)) op*)])
           `(,input-language
             '(graph ,consts
                     ,inits
                     ,sources
                     (operators (,newop* (name ,n*)
                                         (output-type ,oty*)
                                         (code ,newopcode*)
                                         (incoming ,@oin**)
                                         (outgoing ,@oout**))
                                ...)
                     ,sink
                     ,@meta*)))]))))
  


;;
;; split any output stream that feeds into N input ports (N >= 2)
;; into N unique output streams. in particular, for input ports
;; that read from such streams, attach the correct unique port number
;; to their (incoming ...) list.
;;
;; NOTE: this necessarily performs an ordering operation that
;;       affects the generated code!
;;
(define make-output-streams-unique
  (let ()

    ;;
    ;; FIXMEFIXME: needs to consider sources and sink and non-iterates too!!!
    ;; FIXME: need to lift out the body of the emit, if it's not a symbol or a deref
    ;; FIXME: need to deal with virtqueues too

    (define (Expr num-oout)
      (core-generic-traverse
       (lambda (x fallthru)
         (match x
           [(_emit_to ,to ,props ,vq ,x)
            `(begin-reorderable
              ,@(map (lambda (i) `(_emit_to ,i ,props ,vq ,x)) (iota num-oout)))]

           [,oth (fallthru oth)]))))


    ;;
    ;;
    (lambda (prog)
      (match prog
        [(,input-language
          '(graph ,consts
                  ,inits
                  (sources ((name ,src-n*)
                            (output-type ,src-oty*)
                            (code ,src-code*)
                            (outgoing ,src-out** ...))
                           ...)
                  (operators (,op* (name ,n*)
                                   (output-type ,oty*)
                                   (code ,opcode*)
                                   (incoming ,oin** ...)
                                   (outgoing ,oout** ...))
                             ...)
                  ,sink
                  ,meta* ...))

         
         (let*

          ;; first update the (incoming ...) lists with correct output-port numbers
          ([stream-output-counts ; this assoc. list gets mutated
            (map (lambda (n) `(,n . 0)) (append src-n* n*))]
           [newoin** (map (lambda (oin*)
                            (map (lambda (oin)
                                   (let* ([oin-ref (assoc oin stream-output-counts)]
                                          [newoin (cons (car oin-ref) (cdr oin-ref))])
                                     (set-cdr! oin-ref (+ 1 (cdr oin-ref)))
                                     newoin))
                              (map car oin*)))
                       oin**)]

           ;; now replace the emits inside the code
           [newopcode* (map (lambda (opcode num-oout) ((Expr num-oout) opcode))
                         opcode* (map length oout**))])

             `(,input-language
               '(graph ,consts
                       ,inits
                       (sources ((name ,src-n*)
                                 (output-type ,src-oty*)
                                 (code ,src-code*)
                                 (outgoing ,@src-out**))
                                ...)
                       (operators (,op* (name ,n*)
                                        (output-type ,oty*)
                                        (code ,newopcode*)
                                        (incoming ,newoin** ...)
                                        (outgoing ,oout** ...))
                                  ...)
                       ,sink
                       ,meta* ...)))

         ]))


    ))


;;
;; each box should be as created by explicit-stream-wiring
;;
#;
(define merge-two-boxes
  (let ()

    (define (get-relevant-info box)
      (match box
        [(,op (name ,name)
              (output-type ,oty)
              (code ,opcode)
              (incoming ,oin* ...)
              (outgoing ,oout* ...))

         (values op name oty opcode oin* oout*)]))

    (lambda (box1 box2)
      (let-values ([(box1-name box1-oty box1-opcode box1-oin* box1-oout*)
                    (get-relevant-info box1)]
                   [(box2-name box2-oty box2-opcode box2-oin* box2-oout*)
                    (get-relevant-info box2)])

        ;(define merged-streams)

      ))
    
))


) ;; End module
