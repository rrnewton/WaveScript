
(module alpha_lib mzscheme
  (require 
   "iu-match.ss"
   (lib "include.ss")
   (lib "pretty.ss")
   (lib "list.ss")
   (all-except (lib "compat.ss") define-structure flush-output-port) 
   "constants.ss"  
   (all-except "helpers.ss" test-this these-tests filter)
   (all-except "basic_graphics.ss" test-this these-tests)
   (all-except "graphics_stub.ss" test-this these-tests) 
   (all-except "pass21_cleanup-token-machine.ss" test-this these-tests)
   ;; Would like to remove this dependency eventually:
   (all-except "simulator_alpha.ss" ) 
   )
  (provide (all-defined)
           (all-from "simulator_alpha.ss"))
  
  (include (build-path "generic" "alpha_lib.ss"))
  (include (build-path "generic" "alpha_lib_scheduler.ss"))
  (include (build-path "generic" "alpha_lib_scheduler_simple.ss"))
 )


;    (require alpha_lib)

;    (alpha-repl)
    
;    (run-alpha-sim 'simple)
    
;    (begin (require alpha_lib) (time (run-alpha-sim 'simple 10.0)))


;    (begin (require alpha_lib) (t) (run-alpha-sim))


;    (begin (require "alpha_lib.ss") (time (run-alpha-sim 15.0)))


(define (node-code this)
  (define-structure (tokstore
                     storedgparent_21
                     storedgorigin_20
                     storedghopcount_19
                     storedgversion_18
                     storedgparent_27
                     storedgorigin_26
                     storedghopcount_25
                     storedgversion_24
                     ver_23
                     storedliftoption_22))
  (let ([local-sense
         (lambda ()
           ((current-sense-function)
            (node-pos (simobject-node this))))])
    (let* ([result_15 '3])
      (letrec ([global-tree
                (lambda (current-vtime
                         subtok-index
                         g_parent
                         g_origin
                         g_hopcount
                         g_version)
                  (let* (#0=(the-store (simobject-token-store this))
                         [this-tokname
                          (cons 'global-tree . #1=(subtok-index))]
                         .
                         #2=((old-outgoing
                               (simobject-outgoing-msg-buf this))
                             (old-local (simobject-local-msg-buf this))))
                    #3="Is there already an allocated token object?:"
                    (let #4=((tokobj (hashtab-get the-store this-tokname)))
                      (if #5=(not tokobj)
                          (begin #6="If not, then we allocate that token object..."
                                 #7=" setting the invoke counter to zero."
                                 (set! tokobj
                                   (vector
                                     0
                                     (#8='#f)
                                     (#9='#f)
                                     (#10='#f)
                                     (#11='#f)))
                                 .
                                 #12=((hashtab-set!
                                        the-store
                                        this-tokname
                                        tokobj))))
                      #13=(set-simobject-outgoing-msg-buf! this '())
                      #14=(set-simobject-local-msg-buf! this '())
                      (if (if (not (vector-ref tokobj 3))
                              '#t
                              (if (= '0 g_hopcount)
                                  (if (> g_version (vector-ref tokobj 4))
                                      (if (= g_version
                                             (vector-ref tokobj 4))
                                          (< g_hopcount
                                             (vector-ref tokobj 3))
                                          '#f)
                                      '#f)
                                  '#f))
                          (begin (set-simobject-outgoing-msg-buf!
                                   this
                                   (cons (make-simevt
                                           #f
                                           17
                                           (bare-msg-object
                                             '(tok global-tree subtok_ind)
                                             (list #15=(node-id
                                                         (simobject-node
                                                           this))
                                                   g_origin
                                                   (+ '1 g_hopcount)
                                                   g_version)
                                             .
                                             #16=(current-vtime)))
                                         .
                                         #17=((simobject-local-msg-buf
                                                this))))
                                 (if (not (= g_hopcount '0))
                                     (begin (vector-set! tokobj 1 g_parent)
                                            (vector-set! tokobj 2 g_origin)
                                            (vector-set!
                                              tokobj
                                              3
                                              g_hopcount)
                                            (vector-set!
                                              tokobj
                                              4
                                              g_version)
                                            (void))
                                     (void))
                                 (void))
                          (void))
                      .
                      #18=((set-simobject-outgoing-msg-buf!
                             this
                             (append
                               (reverse (simobject-outgoing-msg-buf this))
                               old-outgoing))
                           (set-simobject-local-msg-buf!
                             this
                             (append
                               (reverse (simobject-local-msg-buf this))
                               old-local))
                           (void)))))]
               [spread-global
                (lambda (current-vtime
                         subtok-index
                         g_parent
                         g_origin
                         g_hopcount
                         g_version)
                  (let* (#0#
                         [this-tokname (cons 'spread-global . #1#)]
                         .
                         #2#)
                    #3#
                    (let #4#
                      (if #5#
                          (begin #6#
                                 #7#
                                 (set! tokobj
                                   (vector
                                     0
                                     (#8#)
                                     (#9#)
                                     (#10#)
                                     (#11#)
                                     ((void))
                                     ('#f)))
                                 .
                                 #12#))
                      #13#
                      #14#
                      (if (if (not (vector-ref tokobj 3))
                              '#t
                              (if (= '0 g_hopcount)
                                  (if (> g_version (vector-ref tokobj 4))
                                      (if (= g_version
                                             (vector-ref tokobj 4))
                                          (< g_hopcount
                                             (vector-ref tokobj 3))
                                          '#f)
                                      '#f)
                                  '#f))
                          (begin (if (not (vector-ref tokobj 6))
                                     (begin (vector-set! tokobj 6 '#t)
                                            (vector-set! tokobj 5 '0)
                                            (void)))
                                 (vector-set!
                                   tokobj
                                   5
                                   (+ '1 (vector-ref tokobj 5)))
                                 (set-simobject-outgoing-msg-buf!
                                   this
                                   (cons (make-simevt
                                           #f
                                           17
                                           (bare-msg-object
                                             '(tok global-tree '0)
                                             (list #15#
                                                   '1
                                                   (vector-ref tokobj 5))
                                             .
                                             #16#))
                                         .
                                         #17#))
                                 (set-simobject-timed-token-buf!
                                   this
                                   (cons (make-simevt
                                           (+ 1000 current-vtime)
                                           25
                                           (bare-msg-object
                                             '(tok spread-global 0)
                                             (list '#f '#f '0 '#f)
                                             current-vtime))
                                         (simobject-timed-token-buf this)))
                                 (if (not (= g_hopcount '0))
                                     (begin (vector-set! tokobj 1 g_parent)
                                            (vector-set! tokobj 2 g_origin)
                                            (vector-set!
                                              tokobj
                                              3
                                              g_hopcount)
                                            (vector-set!
                                              tokobj
                                              4
                                              g_version)
                                            (void))
                                     (void))
                                 (void))
                          (void))
                      .
                      #18#)))]
               [node-start
                (lambda (current-vtime subtok-index)
                  (let* (#0# [this-tokname (cons 'node-start . #1#)] . #2#)
                    #3#
                    (let #4#
                      (if #5#
                          (begin #6# #7# (set! tokobj (vector 0)) . #12#))
                      #13#
                      #14#
                      (void)
                      .
                      #18#)))]
               [soc-start
                (lambda (current-vtime subtok-index)
                  (let* (#0# [this-tokname (cons 'soc-start . #1#)] . #2#)
                    #3#
                    (let #4#
                      (if #5#
                          (begin #6# #7# (set! tokobj (vector 0)) . #12#))
                      #13#
                      #14#
                      (begin (soc-return result_15)
                             (soc-finished)
                             (void)
                             'multiple-bindings-for-token
                             (void))
                      .
                      #18#)))])
        (let ([dyndispatch_table (make-default-hash-table)])
          (begin (hashtab-set!
                   dyndispatch_table
                   'global-tree
                   global-tree)
                 (hashtab-set!
                   dyndispatch_table
                   'spread-global
                   spread-global)
                 (hashtab-set! dyndispatch_table 'node-start node-start)
                 (hashtab-set! dyndispatch_table 'soc-start soc-start))
          (values
            (lambda (msgob current-vtime)
              (mvlet
                (((name subtok)
                  (let ([tok (msg-object-token msgob)])
                    (if (pair? tok)
                        (values (car tok) (cdr tok))
                        (values tok 0)))))
                (let ([handler (hashtab-get dyndispatch_table name)])
                  (apply
                    handler
                    current-vtime
                    subtok
                    (msg-object-args msgob)))))
            '((global-tree 17)
              (spread-global 25)
              (node-start 1)
              (soc-start 4))))))))

(require alpha_lib)
(define (t) (start-alpha-sim node-code 'simple 10.0))
