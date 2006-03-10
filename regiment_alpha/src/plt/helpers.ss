
#cs ;; Case Sensitivity
(module helpers mzscheme 
  (require "iu-match.ss"
           "../generic/reg_macros.ss"
           (lib "include.ss")
           (lib "date.ss")
           (lib "pretty.ss")
           (prefix plt: (lib "process.ss"))
	 ;  (all-except (lib "compat.ss") atom?)
           (all-except (lib "list.ss") filter)
;           (all-except "tsort.ss" test-this these-tests)
           "constants.ss"
           "hashtab.ss"
           "engine.ss"
           "chez_compat.ss"
           (prefix swindle: (lib "misc.ss" "swindle"))
           )

  ;; [2006.02.26] 
  ;; How can it possibly take 20sec on faith to native-code compile this one file?
  ;; What's wrong with PLT's native-code compilation setup?

 (provide     	
  
  ;; Syntax:
   
   ;; Values:
   ;; For chez compatibility:
  (all-from "chez_compat.ss")
  (all-from "../generic/reg_macros.ss")
  
   ;; Meet in the middle with chez:
   ;system/echoed system-to-str 
   ;with-evaled-params
   
   ;; Other values 
   id gnuplot histogram date
   display-progress-meter progress-dots count-nodes
   string-split periodic-display all-equal?   
	  
   set->hashtab hashtab->list
   
   ;; Hmm, not sure what meaning immediate has here...
   ;immediate? 
;   constant? datum? 
;   formalexp? cast-formals fit-formals-to-args
   default-unit-tester tester-eq?
   ;default-unit-tester-retries ;; This is in constants.
   substring?
     
   gaussian

   list-repeat! make-repeats
   mapi for-eachi diff
   set? subset? set-equal? list->set set-cons union intersection difference
   setq? subsetq? set-eq?
   remq-all assq-remove-all list-remove-first list-remove-last! list-remove-after 
   filter list-index snoc rac rdc last 
   list-find-position list-remove-before

   insert-between iota disp
   extract-file-extension remove-file-extension 
   file->string string->file file->slist slist->file file->linelists
   pad-width round-to uppercase lowercase symbol-uppercase symbol-lowercase
   graph-map graph-get-connected-component graph-neighbors graph-label-dists 
   graph:simple->vertical graph:vertical->simple
   deep-assq deep-assq-all deep-member? deep-all-matches deep-filter
   unfold-list average clump
    partition partition-equal split-before
   myequal?
   
   stream? live-stream? stream-empty? stream-cons stream-car stream-cdr
   stream-map stream-filter stream-take stream-take-all 
   counter-stream stream-append ;random-stream 
   
   display-constrained
   symbol-append 

   testhelpers testshelpers test-this these-tests

   
;   (all-except (lib "rutils_generic.ss")
;               list->set union intersection difference set?
;               list-head filter list-index snoc rac rdc 
;               insert-between iota disp)
;   (all-from (lib "rutils_generic.ss") )
   ;   (all-from-except (lib "rutils_generic.ss") 
   ;                    list->set union intersection difference set?) 
   ) ;; End provide
           
; =======================================================================  

  (include (build-path "generic" "helpers.ss"))

; =======================================================================
   
    ;; This is a cludge and not a true implementation of chez's "error-handler"
    ;; Erk, this version doesn't make any sense, we could have all sorts of 
    ;; repeated and parallel calls on different threads: 
  ;; [2004.06.13] THIS WONT WORK.  Giving up and defining with-error-handler
    #;(define (error-handler . arg)
      (let ([display-set #f]
	    [escape-set #f]
	    [s #f] [e #f])
      (if (null? arg) 'umm
	  (error-display-handler
	   (lambda (str exn) (if escape-set 
				 ((car arg) str exn)
				 (begin 
				   (set! display-set)
				   (set! s str) 
				   (set! e exn)))))
	  (error-escape-handler
	   (lambda () 
	     (if display-set
		 ((car arg) s e)
		 (set! escape-set #t)))))))
  #;  (define (error-handler . arg)
      (if (null? arg)
	  (error-display-handler)
	  (begin ;; This is lame and dangerous: <WARNING>
	    ;; Ack, this in particular breaks everything:
  ;          (error-escape-handler (lambda () (void)))
	    (error-display-handler (car arg)))))
  

  )

;(require helpers)
;(time (testhelpers))

