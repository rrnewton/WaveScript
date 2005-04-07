
(module pass25_rename-stored mzscheme
  (require (lib "include.ss"))
  (require "constants.ss")
  (require "iu-match.ss")
  (require "helpers.ss")

  (require (all-except (lib "list.ss") filter)) 

  (include (build-path "generic" "pass25_rename-stored.ss"))
  
  (provide (all-defined))
  )

;(require pass25_rename-stored)

#;(define (t)
(rename-stored 
'(desugar-let-stored-lang
  '(program
     (bindings (result_1 '3))
     (nodepgm
       (tokens
         (soc-start
           subtok_ind
           ()
           (stored)
           (begin (void)
                  (soc-return result_1)
                  (soc-finished)
                  'multiple-bindings-for-token))
         (node-start subtok_ind () (stored) (void))
         (spread-global
           subtok_ind
           (g_parent g_origin g_hopcount g_version)
           (stored
             (stored_g_parent #0='#f)
             (stored_g_origin #1='#f)
             (stored_g_hopcount #2='#f)
             (stored_g_version #3='#f)
             (ver_2 (void))
             (storedliftoption_3 '#f))
           (if (if (not stored_g_hopcount)
                   '#t
                   (if (= '0 g_hopcount)
                       (if (> g_version stored_g_version)
                           (if (= g_version stored_g_version)
                               (< g_hopcount stored_g_hopcount)
                               '#f)
                           '#f)
                       '#f))
               (begin (if (not storedliftoption_3)
                          (begin (set! storedliftoption_3 '#t)
                                 (set! ver_2 '0)))
                      (set! ver_2 (+ '1 ver_2))
                      (bcast (tok global-tree 0) (my-id) '1 ver_2)
                      (timed-call
                        1000
                        (tok spread-global 0)
                        '#f
                        '#f
                        '0
                        '#f)
                      (if (not (= g_hopcount '0))
                          (begin (set! stored_g_parent g_parent)
                                 (set! stored_g_origin g_origin)
                                 (set! stored_g_hopcount g_hopcount)
                                 (set! stored_g_version g_version))
                          (void)))
               (void)))
         (global-tree
           subtok_ind
           (g_parent g_origin g_hopcount g_version)
           (stored
             (stored_g_parent #0#)
             (stored_g_origin #1#)
             (stored_g_hopcount #2#)
             (stored_g_version #3#))
           (if (if (not stored_g_hopcount)
                   '#t
                   (if (= '0 g_hopcount)
                       (if (> g_version stored_g_version)
                           (if (= g_version stored_g_version)
                               (< g_hopcount stored_g_hopcount)
                               '#f)
                           '#f)
                       '#f))
               (begin (bcast
                        (tok global-tree subtok_ind)
                        (my-id)
                        g_origin
                        (+ '1 g_hopcount)
                        g_version)
                      (if (not (= g_hopcount '0))
                          (begin (set! stored_g_parent g_parent)
                                 (set! stored_g_origin g_origin)
                                 (set! stored_g_hopcount g_hopcount)
                                 (set! stored_g_version g_version))
                          (void)))
               (void)))))))))