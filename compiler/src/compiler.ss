
;;======================================  
(display "Loading main compiler module.  RegionStreams Demo.")
(newline)

(define old-pass-names
  '(object-system-preprocess                        ;;  1
     verify-scheme                                   ;;  2
     rename-var                                      ;;  3
     remove-implicit-begin                           ;;  4
     remove-unquoted-constant                        ;;  5
     remove-one-armed-if                             ;;  6
     remove-complex-constant                         ;;  7
     uncover-symbol-references                       ;;  8
     uncover-settable                                ;;  9
     remove-impure-letrec                            ;; 10
     convert-case-lambda                             ;; 11
     remove-set!                                     ;; 12
     ;;introduce-cps  -- removing for the moment
     ;    rename-k
     ;;;;;;;;;;;reduce-primitives.1
     remove-anonymous-lambda                         ;; 13
     uncover-free                                    ;; 14
     convert-closure                                 ;; 15
     ;optimize-direct-call
     lift-letrec                                     ;; 16
     introduce-closure-primitives                    ;; 17
     convert-excessive-args                          ;; 18
     reduce-primitives.1                             ;; 19
     reduce-primitives.2                             ;; 20
     normalize-context                               ;; 21
     lift-letrec-body                                ;; 22
     uncover-return                                  ;; 23
     uncover-calltype                                ;; 24
     remove-nonunary-let                             ;; 25
     uncover-local                                   ;; 26
     the-return-of-set!                              ;; 27
     introduce-box                                   ;; 28
     flatten-if                                      ;; 29
     introduce-stack                                 ;; 30
     ;clarify-types
     uncover-pushvariant                             ;; 31
     uncover-maxstack                                ;; 32
     generate-MSIL                                   ;; 33
     ))


(test00)
