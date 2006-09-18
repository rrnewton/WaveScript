;;; interpret.ss
;;; Copyright (C) 1996 R. Kent Dybvig
;;; from "The Scheme Programming Language, 2ed" by R. Kent Dybvig

(define interpret #f)
(let ()
  ;; primitive-environment is an environment containing a small
  ;; number of primitive procedures; it can be extended easily
  ;; to include additional primitives.
  (define primitive-environment
    (list (cons 'apply apply)
          (cons 'assq assq)
          (cons 'call/cc call/cc)
          (cons 'car car)
          (cons 'cadr cadr)
          (cons 'caddr caddr)
          (cons 'cadddr cadddr)
          (cons 'cddr cddr)
          (cons 'cdr cdr)
          (cons 'cons cons)
          (cons 'eq? eq?)
          (cons 'list list)
          (cons 'map map)
          (cons 'memv memv)
          (cons 'null? null?)
          (cons 'pair? pair?)
          (cons 'read read)
          (cons 'set-car! set-car!)
          (cons 'set-cdr! set-cdr!)
          (cons 'symbol? symbol?)))

  ;; new-env returns a new environment from a formal parameter
  ;; specification, a list of actual parameters, and an outer
  ;; environment.  The symbol? test identifies "improper"
  ;; argument lists.  Environments are association lists,
  ;; associating variables with values.
  (define new-env
    (lambda (formals actuals env)
      (cond
        ((null? formals) env)
        ((symbol? formals) (cons (cons formals actuals) env))
        (else
         (cons (cons (car formals) (car actuals))
               (new-env (cdr formals) (cdr actuals) env))))))

  ;; lookup finds the value of the variable var in the environment
  ;; env, using assq.  Assumes var is bound in env.
  (define lookup
    (lambda (var env)
      (cdr (assq var env))))

  ;; assign is similar to lookup but alters the binding of the
  ;; variable var in the environment env by changing the cdr of
  ;; association pair
  (define assign
    (lambda (var val env)
      (set-cdr! (assq var env) val)))

  ;; exec evaluates the expression, recognizing all core forms.
  (define exec
    (lambda (exp env)
      (cond
        ((symbol? exp) (lookup exp env))
        ((pair? exp)
         (case (car exp)
           ((quote) (cadr exp))
           ((lambda)
            (lambda vals
              (let ((env (new-env (cadr exp) vals env)))
                (let loop ((exps (cddr exp)))
                   (if (null? (cdr exps))
                       (exec (car exps) env)
                       (begin
                          (exec (car exps) env)
                          (loop (cdr exps))))))))
           ((if)
            (if (exec (cadr exp) env)
                (exec (caddr exp) env)
                (exec (cadddr exp) env)))
           ((set!)
            (assign (cadr exp)
                    (exec (caddr exp) env)
                    env))
           (else
            (apply (exec (car exp) env)
                   (map (lambda (x) (exec x env))
                        (cdr exp))))))
        (else exp))))

  ;; interpret starts execution with the primitive environment.
  (set! interpret
    (lambda (exp)
      (exec exp  primitive-environment))))
