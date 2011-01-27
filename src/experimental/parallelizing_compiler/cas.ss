



(load-shared-object "./cas.so")

(define chez_CAS_car (foreign-procedure "chez_CAS_car" (scheme-object scheme-object scheme-object) char))
(define chez_CAS_cdr (foreign-procedure "chez_CAS_cdr" (scheme-object scheme-object scheme-object) char))
(define chez_CAS_box (foreign-procedure "chez_CAS_box" (scheme-object scheme-object scheme-object) char))

(define ls (cons 1 2))

(printf  "chez_CAS test, car of ls : ~s \n" (chez_CAS_car ls 1 99))
(printf "How mangled is the list? ~s\n" ls)

(printf  "chez_CAS test, cdr of ls mismatched old: ~s \n" (chez_CAS_cdr ls 3 103))
(printf "How mangled is the list? ~s\n" ls)
(printf  "chez_CAS test, cdr of ls : ~s \n" (chez_CAS_cdr ls 2 104))
(printf "How mangled is the list? ~s\n" ls)

(exit)


