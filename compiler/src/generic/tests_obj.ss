;===============================================================================

(define tests_obj_basic
  '(
     '()
     (program '())
     (program
       (define-class MyClass object
         (lambda () (void))
         (fields public x y)
         (fields private a)
         (methods public (f1 (lambda (x) x))))
       '())
     ;(open-package System (System.WriteLine 1))
     (open-package System (System.Console.WriteLine 1))
     (program
       (define-class MyClass object
         (lambda () (void))
         (fields public x))
       (let ([a (new MyClass)])
         (open-instance a MyClass
                        '())))
     (program
       (define-class MyClass object
         (lambda () (void))
         (fields public x))
       (let ([a (new MyClass)])
         (open-instance a MyClass
                        a.x)))
     (program
       (define-class MyClass object
         (lambda () (void))
         (fields public x)
         (methods public (f1 (lambda () 1))))
       (let ([a (new MyClass)])
         (open-instance a MyClass
                        (a.f1))))
     ))

;===============================================================================


;===============================================================================


;===============================================================================
#!eof

(program
  (define-class MyClass object
    (lambda () (void))
    (fields public x y)
    (fields private a)
    (methods public (f1 (lambda (x) x))))
  (let ([f (lambda (x) (add1 x))])
    (f 3)))