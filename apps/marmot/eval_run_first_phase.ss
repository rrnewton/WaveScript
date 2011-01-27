#!/usr/bin/mzscheme -r


;;
;;
;;
(require (lib "pretty.ss")
         "../../benchmarks/measure.ss"
         "../../benchmarks/compile.ss"
         "../../benchmarks/compare.ss")


;;
;;
;;
(define plan
  '(
    (ws-filename . "run_marmot2.ws")
    #;(var optimizations ( (fuse) () ))
    (mvar  (optimizations source-code            exe                   )
          ([(fuse)        "./query.fuse-on.cpp"  "./query.fuse-on.exe" ]
           [()            "./query.fuse-off.cpp" "./query.fuse-off.exe"]))

    (disabled-passes)
    (var backend (c++
                  #;mlton
                  ))
    ;(backend . c++)
    (source-code)
    (exe)
    (scheduler . corefit-scheduler-df)
    (num-tuples . -1)
    (compile-logfile)
    (run-logfile)
    ))


;;
;;
;;

;; compile only
(pretty-print
 (compare plan 'compile)
)

;; now run only
(let ([runs (apply append (map (lambda (_) (compare plan 'run)) '(1)))]
      [opt-time 0]
      [reg-time 0])

  (for-each
    (lambda (r) (if (equal? (cdr (assoc 'optimizations r)) '(fuse))
                    (set! opt-time (+ opt-time (cdr (assoc 'user-time r))))
                    (set! reg-time (+ reg-time (cdr (assoc 'user-time r))))))
    runs)

  (printf "*** opt: ~a~n*** reg: ~a~n" opt-time reg-time))

  


