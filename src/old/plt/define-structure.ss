
;; [2005.11.04] Don't know what this was for, but it's not in use now.
;; Maybe it was a struct syntax that would make structs printable?

(define-syntax reg:define-struct
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
        (datum->syntax-object template-id
          (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (if (string? x)
                              x
                              (symbol->string
                                (syntax-object->datum x))))
                        args))))))
    (syntax-case x ()
      ((_ (name field1 ...))       
       (andmap identifier? (syntax (name field1 ...)))
       (syntax (reg:define-struct (name field1 ...) ())))
      ((_ (name field1 ...) ((field2 init) ...))
       (andmap identifier? (syntax (name field1 ... field2 ...)))
       (with-syntax
         ((constructor (gen-id (syntax name) "make-" (syntax name)))
          (predicate (gen-id (syntax name) (syntax name) "?"))
          ((access ...)
           (map (lambda (x) (gen-id x (syntax name) "-" x))
                (syntax (field1 ... field2 ...))))
          ((assign ...)
           (map (lambda (x) (gen-id x "set-" (syntax name) "-" x "!"))
                (syntax (field1 ... field2 ...))))
          (structure-length
           (+ (length (syntax (field1 ... field2 ...))) 1))
          ((index ...)
           (let f ((i 1) (ids (syntax (field1 ... field2 ...))))
              (if (null? ids)
                  '()
                  (cons i (f (+ i 1) (cdr ids)))))))
         (syntax (begin
                   (define constructor
                     (lambda (field1 ...)
                       (let* ((field2 init) ...)
                         (vector 'name field1 ... field2 ...))))
                   (define predicate
                     (lambda (x)
                       (and (vector? x)
                            (= (vector-length x) structure-length)
                            (eq? (vector-ref x 0) 'name))))
                   (define access
                     (lambda (x)
                       (vector-ref x index)))
                   ...
                   (define assign
                     (lambda (x update)
                       (vector-set! x index update)))
                   ...)))))))

