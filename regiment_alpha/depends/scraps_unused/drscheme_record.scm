;;;                                                                           
;;;  define-record and record-case macros for drscheme 
 
;;; Based on Jens Palsberg's version of 8/21/96 (sources listed 
;;; below), modified by Mitchell Wand for Dr Scheme on 2/3/98. 
 
;;;                                                                           
;;;  Following Duncan Fisher, the syntax of record-case allows the keys       
;;;  of the clauses to be lists of symbols (i.e. more then one type of        
;;;  record, similar to a case statement) as well as the usual single         
;;;  symbol.                                                                  
;;;                                                                           
;;;  Implemented by Jens Palsberg (palsberg@cs.purdue.edu) borrowing heavily  
;;;  from previous work by Duncan Fisher, Brent Benson, Gary Duzan,           
;;;  David McCusker, and Dan Ryan.    
 
; Jens wrote on Wed Aug 21 14:12:54 1996:  
; 
; My code is a quick fix  
; of the macgambit macros to which you pointed me.  My code 
; does not have nice printf and format.  The problem is that 
; the macgambit code uses open-input-string and open-output-string 
; for formatting, and I don't know what they do or where I can 
; find code for them.  In case you should be interested in 
; taking a look, the scm macros are appended. 
 
 
(define format->  
  (lambda (x l)    
    (string-append (symbol->string x) "->" (symbol->string l)))) 
 
(define format? 
  (lambda (x) 
    (string-append (symbol->string x) "?"))) 
 
(define formatmake- 
  (lambda (x) 
    (string-append "make-" (symbol->string x)))) 
 
;;; 
;;; Auxiliary procedures needed for the following record marcos. 
;;; 
;;; record-macros:fog returns the composition of the two functions f and g 
;;; 
;;; record-macros:sos? returns #t if lst is a list of distinct symbols => 
;;; a set of symbols, otherwise #f. Note: (record-macros:sos? '()) => #t 
;;; 
 
(define (record-macros:compose f g) 
  (lambda (x) (f (g x)))) 
 
(define (record-macros:sos? lst) 
  (if (list? lst) 
    (letrec ((set? 
              (lambda (los) 
                (cond 
                 ((null? los) #t) 
                 ((not (symbol? (car los))) #f) 
                 ((memq (car los) (cdr los)) #f) 
                 (else (set? (cdr los))))))) 
      (set? lst)) 
    #f)) 
 
;;; 
;;; define-record macro 
;;; 
;;; Syntax of general form: (define-record name (field-1 ... field-n)) 
;;; 
;;; This macro expansion defines the following procedures: 
;;;    make-name 
;;;    name->field-1 
;;;    ... 
;;;    name->field-n 
;;;    name? 
;;; 
 
; in scm this was 
; (defmacro define-record args   ;; args = (name . fields) 
;  (let ((name (car args)) 
;        (fields (cadr args)))  ...)) 
 
; changed protocol for DrScheme... 
(define-macro define-record  
  (lambda (name fields) 
    ;; The make-readers procedure returns a list of procedures which 
    ;; define the name->field-1 ... name->field-n procedures that are 
    ;; used to access the specific data fields of a name type record. 
    (letrec ((make-readers 
               (lambda (name fields get-field) 
                 (if (null? fields) 
                   '() 
                   (let ((reader 
                           (string->symbol (format-> name (car fields))))) 
                     (cons `(define ,reader (record-macros:compose car 
                                              ,get-field))  
                           (make-readers 
                             name (cdr fields) 
                             (list 'record-macros:compose 'cdr get-field)))))))) 
      (if (and (symbol? name) (record-macros:sos? fields)) 
        (let ((predicate   (string->symbol (format? name))) 
              (constructor (string->symbol (formatmake- name)))) 
          `(begin 
             (define ,predicate (lambda (x) (and (list? x) (eq? (car x) ',name)))) 
             (define ,constructor (lambda args (cons ',name args))) 
             ,@(make-readers name fields 'cdr))) 
        (error 'define-record-macro "illegal syntax in ~a type record" name))))) 
 
;;; 
;;; record-case macro 
;;; 
;;; Syntax of general form: (record-case record-expression 
;;;                           (name-1 field-list-1 consequent-1) 
;;;                           ... 
;;;                           (name-n field-list-n consequent-n) 
;;;                           (else alternative)) 
;;; 
;;; Note: name-i may be a list of distinct symbols in addition to being 
;;; just a symbol. This is more in line with the usual type case definition. 
;;; 
;;; For each clause in the record-case statement, the record-case 
;;; macro expands it to a cond type clause of the form ((pred? value) 
;;; (let-expression)), where the body of the let expression is the 
;;; consequent expressions of the record-case form. The bindings of the 
;;; let form are those between the symbols of the record-case field-list 
;;; and the corresponding values of the record passed in as argument 
;;; record-var to record-case. These bindings are done based on position 
;;; only. 
;;; 
 
; (defmacro record-case args    ;; args = (record-exp . clauses) 
; (let ((record-exp (car args)) 
;       (clauses (cdr args))) ... )) 
 
(define-macro record-case  
  (lambda (record-exp . clauses) 
    (let ((var     (gensym))) 
      ;; make-conds expands all the record-case clauses into a 
      ;; list of appropriate cond style clauses, i.e. each clause 
      ;; list of the form: 
      ;;    (name-i field-list-i consequent-i) 
      ;; gets expanded into: 
      ;;    ((name-i? record-exp) 
      ;;        (let ((field-list-i-1 field-name-1) 
      ;;              (           ...             ) 
      ;;              (field-list-i-n field-name-n)) 
      ;;          consequent-i)) 
      (letrec ((make-conds 
                 (lambda (clause) 
                   (cond 
                     ((null? clause) 
                      `((else (error 'record-case "no clause match: ~a" ,var)))) 
                     ((eq? (caar clause) 'else) 
                      `((else ,@(cdar clause)))) 
                     (else 
                       ;; The make-lets procedure returns the let expression bindings for 
                       ;; one record-case clause only. These in turn will be combined with 
                       ;; the (pred? value) list to form one clause of the cond statement. 
                       (letrec ((make-lets 
                                  (lambda (fields get-field) 
                                    (if (null? fields) 
                                      '() 
                                      (cons 
                                        `(,(car fields) (car ,get-field)) 
                                        (make-lets (cdr fields) 
                                          (list 'cdr get-field))))))) 
                         (let* ((key (caar clause)) 
                                (pred? (if (list? key) 
                                         `(memv (car ,var) ',key) 
                                         `(eqv? (car ,var) ',key))) 
                                (lets (make-lets (cadar clause) `(cdr ,var)))) 
                           (cons 
                             `(,pred? (let ,lets ,@(cddar clause))) 
                             (make-conds (cdr clause)))))))))) 
        (if (record-macros:check-syntax clauses) 
          `(let ((,var ,record-exp)) (cond ,@(make-conds clauses))) 
          ;; this error call should never be processed since 
          ;; check-syntax only returns #t or doesn't return at all 
          (error 'record-case-macro 
            "Unspecified error ~%Record-exp = ~p~%Clauses = ~p" 
            record-exp clauses)))))) 
 
;;; 
;;; check-syntax analyses the syntax of the clauses of the record-case 
;;; statement, e.g. is it a list of lists, are the cars of each list a 
;;; symbol or set of symbols, are the second elements of each list also 
;;; a set of symbols, etc. Also checks to see if the keys to all the 
;;; clauses are unique. Returns #t if there are no syntax errors found, 
;;; otherwise it calls the error procedure and doesn't return. 
;;; 
 
(define (record-macros:check-syntax clauses) 
  ;; clause? returns true if a single record-case 
  ;; clause has the correct syntax, otherwise false 
  (let ((clause? (lambda (c) 
                   (and (list? c) 
                        (not (null? c)) 
                        (or (symbol? (car c)) (record-macros:sos? (car c))) 
                        (if (eq? 'else (car c)) 
                          (not (null? (cdr c))) 
                          (and (> (length c) 2) 
                               (record-macros:sos? (cadr c)))))))) 
  ;; check-clauses returns #t if there are no invalid clauses detected, 
  ;; otherwise the error procedure is invoked and no value is returned 
    (letrec ((check-clauses 
              (lambda (clauses) 
                (cond 
                 ((null? clauses) #t) 
                 ((not (clause? (car clauses))) 
                  (error 'record-macros:check-syntax 
                    "~%Incorrect syntax, clause invalid:~%~p" (car clauses))) 
                 ((and (eq? (caar clauses) 'else) 
                       (not (null? (cdr clauses)))) 
                  (error 'record-macros:check-syntax 
                    "~%Incorrect syntax, clauses after else:~%~p" clauses)) 
                 (else (check-clauses (cdr clauses)))))) 
  ;; case-names returns a list containing all the key symbols 
  ;; of all the clauses of the record-case (including else) 
             (case-names 
              (lambda (clauses) 
                (cond 
                 ((null? clauses) '()) 
                 ((list? (caar clauses)) 
                  (append (caar clauses) (case-names (cdr clauses)))) 
                 (else 
                  (cons (caar clauses) (case-names (cdr clauses)))))))) 
      (let ((keys (case-names clauses))) 
        (if (record-macros:sos? keys) 
          (check-clauses clauses) 
          (error 'record-macros:check-syntax 
            "~%Incorrect syntax, duplicate clause keys in~%~p" keys)))))) 
 
;;; 
;;; make-record-from-name taken from chez-init.scm - used by sllgen.scm 
;;; 
 
(define (make-record-from-name name) 
  (lambda args 
    (cons name args))) 
 
