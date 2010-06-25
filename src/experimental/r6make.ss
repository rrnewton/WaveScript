#! /bin/bash
#|
exec $REGIMENTD/depends/chez --threaded --script $0 $*
|#

;; exec $REGIMENTD/depends/chez --script $0 $*

;; Currently this is chez-specific.

;; A simple make style utility for incremental builds with Chez.  This
;; not supposed to be general or correct, rather, it's a temporary fix
;; for this particular project.

;; Usage: r6rsmake.ss file.sls [purge]
;; The optional "purge" argument will destroy dirty objects rather than rebuilding.

;; Ryan Newton [2009.02.17]

;; Note, this build script is now parallel:
;(define-syntax IFPAR (syntax-rules () [(_ a b) a] [(_ a) a])) ;; ENABLE
(define-syntax IFPAR (syntax-rules () [(_ a b) b] [(_ a) (void)])) ;; DISABLE

;; FIXME: BUG [2010.06.25] In parallel mode the 'list action doesn't
;; work.  It prints a number of lines that contain only ".so" (with no
;; file name).

(import (rnrs))

(IFPAR 
 (begin 
   (include "experimental/chez_threaded_utils.ss")
   (import threaded_utils)))

;; Should we compile, or (our alternate behaviour) purge the dirty files.
(define selected-action 'compile)

;; One argument function invoked on a source file:
(define (perform-action f) 
  (define root (remove-file-extension f))
  
  ;; Regiment/WaveScript specific flags.
  (define optlvl (getenv "REGOPTLVL"))
  (if optlvl
    (optimize-level (string->number optlvl))
    (optimize-level 2))
  ;; REGDEBUGMODE is unset to indicate non-debug:
  ;(generate-inspector-information (if (getenv "REGDEBUGMODE") #t #f))
  (generate-inspector-information (not (= 3 (optimize-level))))
  
  ;(generate-inspector-information #t)
  ;(printf "  Compiling: ~a opt-level ~a \n" f (optimize-level))
  (case selected-action
    [(compile) 
      (printf "[optlvl ~a]  " (optimize-level))
      (compile-file f (string-append root ".so"))]
    [(purge)
     (let ((obj (string-append root ".so")))
      (printf "[wiping]  ~s\n" obj)
      (when (file-exists? obj) (delete-file obj)))]
    [(list) (printf "~a.so\n" root)]
    [else (error 'perform-action "this is an internal bug.")]))

;; The (main) file to compile.
;; Also process other command line arguments.
(define source 
  (if (null? (command-line-arguments))
      (begin (display "Enter filename: ") 
	     (let ((x (read)))
	       (if (symbol? x)
		   (symbol->string x)
		   x)))
      (begin
        (unless (null? (cdr (command-line-arguments)))
	  (cond
	    [(equal? (cadr (command-line-arguments)) "purge")   (set! selected-action 'purge)]
	    [(equal? (cadr (command-line-arguments)) "compile") (set! selected-action 'compile)]
	    [(equal? (cadr (command-line-arguments)) "list")    (set! selected-action 'list)]
	    [else (error 'r6make.ss "unknown extra argument ~s" (cadr (command-line-arguments)))]))
        (car (command-line-arguments)))))

;; Turn the module name into a filename.
(define (resolve-name los)
    (case (car los)
      [(rnrs scheme) #f]
      [(except for) (resolve-name (cadr los))]
      [else 
       (string-append
	"."
	(apply string-append 
	       (map (lambda (s) (string-append "/" (symbol->string s))) los))
	".sls")]))


;; Retrieve the (static) import dependencies.
(define (extract-deps sexp)
  (define (cleanup modname)
    (if (pair? modname)
      (case (car modname)
	[(except prefix only) (cleanup (cadr modname))]
	[else modname])
      modname))
  (case (car sexp)
    [(import)  (map cleanup (cdr sexp))]
    [(library)
     ;(printf "library\n")
     (map cleanup (cdr (cadddr sexp)))
     ]))

;; ================================================================================
;; Various generic utility functions:
;; ================================================================================

(define (id x) x)
(define remove-file-extension
  (lambda (filename)
    (let loop ([ls (reverse (string->list filename))])
      (cond
        [(null? ls) filename] ;no extension to remove
        [(eq? (car ls) #\.)
         (list->string (reverse (cdr ls)))]
        [else (loop (cdr ls))]))))

(define intersection
  (case-lambda
    [(set1 set2)
     (let loop ([set1 set1])
       (cond
         [(null? set1) '()]
         [(member (car set1) set2) (cons (car set1) (loop (cdr set1)))]
         [else (loop (cdr set1))]))]
    [all 
     (let loop ([set1 (car all)] [sets (cdr all)])
       (if (null? sets)
           set1
           (loop (intersection set1 (car sets)) (cdr sets))))]))
(define difference
  (lambda (set1 set2)
    (let loop ([set1 set1]
	       [set2 set2])
    (cond
     ((null? set1) '())
     ((member (car set1) set2) (loop (cdr set1) set2))
     (else (cons (car set1)  (loop (cdr set1) set2)))))))

(define remove-file-extension
  (lambda (filename)
    (let loop ([ls (reverse (string->list filename))])
      (cond
        [(null? ls) filename] ;no extension to remove
        [(eq? (car ls) #\.)
         (list->string (reverse (cdr ls)))]
        [else (loop (cdr ls))]))))

;; [2005.11.17] This one is similar 
;; It looks like the chez primitive doesn't provide a handle on stderror.
(define (system-to-str str)
  (let* ([ls (process str)]
	 [in (car ls)] [out (cadr ls)] [id (caddr ls)])
    (let-values (((p extract) (open-string-output-port)))
    (let loop ((c (read-char in)))
      (if (eof-object? c)	  
	  (begin 
	    (close-input-port in)
	    (close-output-port out)
	    (extract))
	  (begin (display c p)
		 (loop (read-char in))))))))

(define (is-changed? file)
  (define (modtime file)
    (read (open-input-string 
	   (system-to-str (string-append "stat " file " -c \"%Y\"")))))
  (define obj  (string-append (remove-file-extension file) ".so"))
  (or (not (file-exists? obj))
      (> (modtime file) (modtime obj))))

;; On lists, port to sets:
;; NOTE: Uses eq? !
(define set-cons:list
  (case-lambda
    [(x set equ?)
     (cond
      [(null? set) (list x)]
      [(equ? x (car set)) set]
      [else (cons (car set) (set-cons:list x (cdr set)))])]
    [(x set) (set-cons:list x set eq?)]))
(define list-rem-dups
  (case-lambda
    [(ls) (list-rem-dups ls eq?)]
    [(ls comp)
    (let loop ([ls ls])
       (if (null? ls) '()
	 (reverse! 
	  (set-cons:list (car ls) (loop (cdr ls)) comp))))]))


;; ================================================================================
;; Main loop
;; ================================================================================

;; Returns a new checked, changed
;;   ls --  a stack of modules to process:
;;   checked -- files already processed
;;   changed -- (checked) files that have changed and need recompile
(define (compile-loop ls checked changed)
  (if (null? ls) 
      (values checked changed)
      (if (member (car ls) checked) ;; Already done.
	  (compile-loop (cdr ls) checked changed)
	  
	  (let ([thisfile (car ls)])
	    (unless (eq? selected-action 'list)
	      (printf "  Checking: ~a\n" thisfile))
	    (let* ([file (open-input-file thisfile)]
		   [deps (extract-deps (read file))])
	      (close-port file)
	      (let ([depfiles (filter id (map resolve-name deps))])
	        ;; Process our dependencies, depth first:
		(let-values ([(chk chng) (compile-loop depfiles checked changed)])
		  ;; The problem with processing dependencies in
		  ;; parallel is a "diamond problem" Two deps may have
		  ;; a race to process a third.  We could catch this
		  ;; at the last moment when the action is issued...
		  (let ([chk2 (cons thisfile chk)]
			[depschanged (intersection depfiles chng)])
		    (if (and (not (is-changed? thisfile)) 
			     (null? depschanged))
			;; Neither we nor our dependencies have changed.  No recompile:
			(compile-loop (cdr ls) chk2 chng)
			(begin (perform-action thisfile)
			       (compile-loop (cdr ls) chk2 (cons thisfile chng))))
		    )))
		    )))))

;; ================================================================================
;; New version -- trying parallel compile.
;; ================================================================================

(define (compile-parallel1 file)
  'unfinished)


;; This separates the problem of stale detection from the actual recompile process.
(define (build-compile-dag file)
  ;; changed is an association list with the dependence tree as the cdr.
  (define (helper ls checked changed)
    (if (null? ls) 
	(values '() checked changed)
	(if (member (car ls) checked) ;; Already done.
	    (let-values ([(existing-entry) (assoc (car ls) changed)]
			 [(rest a b) (helper (cdr ls) checked changed)])
	      (values (if existing-entry
			  (cons existing-entry rest)
			  rest)
		      a b))
	    
	    (let ([thisfile (car ls)])
	      (unless (eq? selected-action 'list)
		(printf "  Checking: ~a\n" thisfile))
	      (let* ([file (open-input-file thisfile)]
		     [deps (extract-deps (read file))])
		(close-port file)
		(let ([depfiles (filter id (map resolve-name deps))])
		  ; (printf "  Following: ~s\n" depfiles)
		  ;; Process our dependencies, depth first:
		  (let-values ([(trees chk chng) (helper depfiles checked changed)])
		    (let ([chk2 (cons thisfile chk)]
			  [depschanged (intersection depfiles (map car chng))])

		      (if (and (not (is-changed? thisfile)) 
			       (null? depschanged))
			  ;; Neither we nor our dependencies have changed.  No recompile:
			  (helper (cdr ls) chk2 chng)
			  (begin
			    (let ([mytree (cons thisfile trees)])			    
			      (let-values ([(t c1 c2) (helper (cdr ls) chk2 (cons mytree chng))])
				(values (cons mytree t) c1 c2))
			      ))))))
		)))))	       
 (let-values ([(a b c) (helper (list file) '() '())])
   (if (null? a) '() (car a))))



#;
;; DEFINITELY BROKEN:
;; ----------------------------------------
;; Reverse the edges:
(define (flip-dag dagls)
  (define processed (make-eq-hashtable))
  (define (loop dag kont); ([dag dag] [kont '()])
    (if (null? dag) '()
     (let ([existing (hashtable-ref processed (car dag) #f)])
       ;(printf "    existing ~s ~s\n" (car dag) existing)
       (if existing (list existing)
	   ;; Otherwise we go through all the downstream and flip them.
	   (let ([this (cons (car dag) kont)] ;; Connect myself to my former upstream.
		 [downstream (cdr dag)])
	     ;(printf "  CONNECTED UP ~s downstream ~s\n" this downstream)
	     (hashtable-set! processed (car dag) this)
	     (if (null? downstream)
		 (list this)
		 (apply append (map (lambda (d) (loop d (list this))) downstream))
		 ))))))
  (apply append (map (lambda (x) (loop x '())) dagls)))


;; Very inefficient:
;; This transforms a nested dag/list representation into a flat one:
(define (transitive-closure dagls)
  (define graph (make-eq-hashtable))
  (define (get-deps ls)
    (cond
     [(null? ls) '()]
     [else (cons (caar ls) 
		 (append (get-deps (cdar ls))
			 (get-deps (cdr ls))))]))
  (let loop  ([ls dagls])
      (if (null? ls)
          (let-values ([(keys vals) (hashtable-entries graph)])
	    (map cons (vector->list keys) (vector->list vals)))
	  (begin	   
	    (hashtable-set! graph (caar ls) 
			    (append (remq (caar ls) (list-rem-dups (get-deps (list (car ls)))))
				    (hashtable-ref graph (caar ls) '())))
	    ;(printf "JUST SET ~s ~s\n" (car ls) (hashtable-ref graph (caar ls) #f))
	    (loop (cdar ls))
	    (loop (cdr ls)))	 
       	  ))
    ;(apply-append (get-dependent dag) )
    ;(get-deps dagls)
  )

;; Takes a flat graph-as-table representation:
(define (breadth-first-slices dagls)
    (let loop ((dagls dagls) (already-done '()))
      (if (null? dagls) '()
	  (let* ([this-round (apply append (filter (lambda (l) (null? (cdr l)))dagls))]
		 [newdone (append this-round already-done)]
		 [notdone? (lambda (x) (not (member x newdone)))]
		 [whats-left (map (lambda (entry) (cons (car entry) (filter notdone? (cdr entry))))
			       (filter (lambda (e) (notdone? (car e))) dagls))])
	   ;(printf "THIS ROUND ~s, whats left ~s, already done ~s\n" this-round whats-left newdone)
	    (cons this-round  (loop whats-left newdone))
	    ))))

#;
(define (breadth-first-slices dag)
  (define processed (make-eq-hashtable))
  (let loop ((ls (flip-dag (list dag))))
    (if (null? ls) '()
	;; Find (new) children:
	(let* ([not-processed 
		(fold-left (lambda (acc tree) 
			     (define already-there? (hashtable-contains? processed tree))
			     (hashtable-set! processed tree #t)
			     (if already-there? acc (cons tree acc)))
			   '() ls)]				 
	       [rest (loop (apply append (map cdr not-processed)))])

	   ;(printf "unprocessed children: ~s    processed: ~s\n" (map car not-processed) (hashtable-keys processed))
	     	   
	   (if (null? not-processed) rest
	       (cons (map car not-processed)  rest))
	   ))))

(define (par-opportunity-profile dagls) 
  (map length (breadth-first-slices (transitive-closure dagls))))

;;====================================================================================================

(define test0
  '("regiment.ss"
   #0=("./main_r6rs.sls"
       #1=("./ws/passes/wavescope_bkend/emit-c2.sls")
       ("./ws/passes/wavescope_bkend/emit-tbb.sls" #1#)
       ("./ws/passes/wavescope_bkend/emit-tinyos.sls" #1#)
       ("./ws/passes/wavescope_bkend/emit-java.sls" #1#))
   #2=("./main.sls" #0# ("./ws/testing/system_tests.sls" #0#))
   ("./ws/shortcuts.sls" #0# #2#)))


(define FULLDAG
  '("regiment.ss"
    #0=("./main_r6rs.sls"
	#1=("./ws/common.sls" #2=("./ws/compat/compat.sls") #3=("./ws/globals.sls" #2#)
	    #4=("./ws/util/hashtab.sls")
	    #5=("./ws/util/iu-match.sls" #2#)
	    #6=("./ws/util/reg_macros.sls" #2# #3# #5#)
	    #7=("./ws/util/helpers.sls" #2# #3# #4# #5# #6#)
	    ("./ws/util/streams.sls" #2# #3# #7# #6# #5#)
	    #8=("./ws/compiler_components/prim_defs.sls" #2# #3# #5# #4#
                #7#)
	    #9=("./ws/compiler_components/regiment_helpers.sls" #2# #3# #8# #4# #7# #5# #6#)
	    #10=("./ws/compiler_components/type_environments.sls" #2# #3# #5# #4# #7# #6# #8#)
	    #11=("./ws/compiler_components/reg_core_generic_traverse.sls" #2# #3# #5# #4# #7# #6# #8# #9# #10#)
	    #12=("./ws/compiler_components/hm_type_inference.sls" #2# #3# #7# #6# #4# #8# #11# #10# #9# #5#)
	    #13=("./ws/grammars/grammar_checker.sls" #2# #3# #5# #7# #6#
                 #8# #9# #10#)
	    #14=("./ws/passes/pass-mechanism_basic.sls"
                 #2#
                 #3#
                 #5#
                 #13#)
	    ("./ws/passes/pass-mechanism.sls" #2# #3# #5# #7# #6# #9#
             #11# #12# #14#))
	#15=("./ws/util/scheme_fft.sls" #2# #3# #7# #6#)
	#16=("./ws/util/slib_fft.sls" #2# #7# #3#)
	#17=("./ws/util/fft.sls" #1# #3# #7# #16# #15#)
	#18=("./ws/util/tsort.sls" #1#)
	#19=("./ws/util/hash.sls" #3#)
	#20=("./ws/util/slib_hashtab.sls" #2# #3# #19# #7#)
	#21=("./ws/util/bos_oop.sls" #1#)
	#22=("./ws/util/imperative_streams.sls" #2# #3# #7# #6# #5#)
	#23=("./ws/compiler_components/annotations.sls" #1#)
	#24=("./ws/compiler_components/c_generator.sls" #1#)
	("./ws/compiler_components/source_loader.sls" #1#)
	("./ws/passes/small-ws-passes.sls"
	 #1#
	 #25=("./ws/passes/normalize_query/ws-remove-complex-opera.sls"
              #1#
              #26=("./ws/passes/normalize_query/ws-remove-letrec.sls"
		   #1#
		   #27=("./ws/passes/normalize_query/reduce-primitives.sls"
			#1#
			#28=("./ws/passes/static_elaborate/static-elaborate.sls"
			     #2#
			     #1#
			     #29=("./ws/passes/normalize_source/remove-unquoted-constant.sls"
				  #1#
				  #30=("./ws/passes/normalize_source/eta-primitives.sls"
				       #1#
				       #31=("./ws/passes/normalize_source/rename-vars.sls"
					    #1#
					    #32=("./ws/passes/normalize_source/ws-label-mutable.sls"
						 #1#))))))
		   #18#))
	 #33=("./ws/passes/optimizations/rewrite_opts.sls" #1#))
	#34=("./ws/passes/partition-graph.sls"
	     #1#
	     #35=("./ws/passes/wavescope_bkend/nominalize-types.sls"
                  #1#
                  #18#))
	("./ws/passes/graphviz.sls" #1# #24#)
	#36=("./depends/matpak.sls" #1#)
	#37=("./ws/langs/lang_wavescript.sls" #1# #24#)
	#38=("./ws/testing/lang_wavescript_tests.sls"
	     #1#
	     #3#
	     #39=("./ws/sim/wavescript_sim_library_push.sls" #1# #20#
                  #17# #37# #36# #22#))
	#39#
	("./ws/passes/normalize_source/verify-regiment.sls" #1#)
	#40=("./ws/passes/normalize_source/typecheck.sls" #1#)
	("./ws/passes/normalize_source/desugar-pattern-matching.sls"
	 #1#)
	("./ws/passes/normalize_source/resolve-varrefs.sls" #1#)
	#32# #31# #30#
	("./ws/passes/normalize_source/desugar-misc.sls" #1# #30#)
	#29# #28# #27#
	("./ws/passes/normalize_query/records-to-tuples.sls" #1#)
	#26# #25#
	#41=("./ws/passes/normalize_query/ws-lift-let.sls" #1# #25#)
	#42=("./ws/passes/normalize_query/remove-complex-constant.sls"
	     #1#
	     #27#)
	("./ws/passes/normalize_query/uncover-free.sls" #1# #42#)
	("./ws/passes/normalize_query/lift-letrec.sls" #1#)
	("./ws/passes/normalize_query/lift-letrec-body.sls" #1#)
	("./ws/passes/normalize_query/remove-lazy-letrec.sls"
	 #1#
	 #18#)
	("./ws/passes/normalize_query/verify-core.sls" #1#)
	("./ws/passes/normalize_query/ws-normalize-context.sls" #1#)
	("./ws/passes/static_elaborate/interpret-meta.sls" #1# #28# #35# #26# #37# #38#)
	#43=("./ws/passes/static_elaborate/degeneralize-arithmetic.sls"
	     #1#
	     #40#
	     #28#)
	#44=("./ws/passes/static_elaborate/verify-elaborated.sls"
	     #1#
	     #29#
	     #43#)
	("./ws/passes/static_elaborate/split-union-types.sls"
	 #1#
	 #18#
	 #40#
	 #28#)
	("./ws/passes/optimizations/smoosh-together.sls" #1#) #33#
	("./ws/passes/optimizations/merge-iterates.sls" #1#)
	("./ws/passes/optimizations/merge-iterates2.sls" #1# #7#)
	("./ws/passes/optimizations/simple-merge-iterates.sls"
	 #1#
	 #23#)
	#35#
	#45=("./ws/passes/wavescope_bkend/convert-sums-to-tuples.sls"
	     #1#)
	("./ws/passes/wavescope_bkend/reify-certain-types.sls" #1#)
	("./ws/passes/wavescope_bkend/type-annotate-misc.sls"
	 #1#
	 #41#)
	("./ws/passes/wavescope_bkend/flatten-iterate-spine.sls"
	 #1#)
	("./ws/passes/wavescope_bkend/anihilate-higher-order.sls"
	 #1#
	 #44#)
	("./ws/passes/wavescope_bkend/explicit-stream-wiring.sls"
	 #1#)
	#46=("./ws/passes/wavescope_bkend/emit-c.sls"
	     #1#
	     #35#
	     #45#
	     #24#)
	#47=("./ws/passes/wavescope_bkend/insert-refcounts.sls" #1#)
	#48=("./ws/passes/wavescope_bkend/emit-c2.sls" #2# #1# #47# #46# #34# #24# #21# #45#)
	("./ws/passes/wavescope_bkend/emit-tbb.sls" #2# #1# #47#
	 #46# #24# #21# #24# #48#)
	("./ws/passes/wavescope_bkend/emit-tinyos.sls" #2# #1# #47# #46# #24# #21# #24# #48#)
	("./ws/passes/wavescope_bkend/emit-java.sls" #2# #1# #47#
	 #46# #24# #21# #24# #48#)
	#49=("./ws/passes/ocaml_bkend/shared-emit-ml.sls" #1# #24#)
	("./ws/passes/ocaml_bkend/emit-caml.sls" #1# #24# #49#)
	("./ws/passes/mlton_bkend/emit-mlton.sls" #2# #1# #24# #49#)
	("./ws/passes/analyze_data_rates/annotate-with-data-rates.sls"
	 #1#
	 #7#
	 #37#))
    #50=("./main.sls" #0# ("./ws/testing/system_tests.sls" #0#))
    ("./ws/shortcuts.sls" #0# #50#)))

(define FULLSLICES
  '(("./ws/util/hashtab.sls" "./ws/compat/compat.sls")
   ("./ws/globals.sls" "./ws/util/iu-match.sls")
   ("./ws/util/reg_macros.sls" "./ws/util/hash.sls")
   ("./ws/util/helpers.sls")
   ("./ws/util/scheme_fft.sls" "./ws/util/slib_fft.sls" "./ws/util/streams.sls"
    "./ws/compiler_components/prim_defs.sls"
    "./ws/util/imperative_streams.sls"
    "./ws/util/slib_hashtab.sls")
   ("./ws/compiler_components/regiment_helpers.sls"
    "./ws/compiler_components/type_environments.sls")
   ("./ws/grammars/grammar_checker.sls"
    "./ws/compiler_components/reg_core_generic_traverse.sls")
   ("./ws/passes/pass-mechanism_basic.sls"
    "./ws/compiler_components/hm_type_inference.sls")
   ("./ws/passes/pass-mechanism.sls") ("./ws/common.sls")
   ("./ws/passes/wavescope_bkend/convert-sums-to-tuples.sls"
    "./ws/passes/normalize_query/ws-normalize-context.sls"
    "./ws/passes/wavescope_bkend/explicit-stream-wiring.sls"
    "./ws/passes/optimizations/merge-iterates.sls"
    "./ws/passes/wavescope_bkend/insert-refcounts.sls"
    "./ws/util/fft.sls"
    "./ws/passes/wavescope_bkend/reify-certain-types.sls"
    "./ws/passes/optimizations/rewrite_opts.sls"
    "./ws/util/bos_oop.sls"
    "./ws/passes/normalize_query/verify-core.sls"
    "./ws/passes/optimizations/smoosh-together.sls"
    "./ws/passes/normalize_query/lift-letrec.sls"
    "./ws/passes/normalize_query/records-to-tuples.sls"
    "./depends/matpak.sls" "./ws/util/tsort.sls"
    "./ws/passes/normalize_query/lift-letrec-body.sls"
    "./ws/passes/wavescope_bkend/flatten-iterate-spine.sls"
    "./ws/passes/normalize_source/verify-regiment.sls"
    "./ws/passes/normalize_source/desugar-pattern-matching.sls"
    "./ws/passes/normalize_source/resolve-varrefs.sls"
    "./ws/compiler_components/source_loader.sls"
    "./ws/compiler_components/annotations.sls"
    "./ws/passes/optimizations/merge-iterates2.sls"
    "./ws/passes/normalize_source/typecheck.sls"
    "./ws/passes/normalize_source/ws-label-mutable.sls"
    "./ws/compiler_components/c_generator.sls")
   ("./ws/langs/lang_wavescript.sls" "./ws/passes/normalize_source/rename-vars.sls"
    "./ws/passes/normalize_query/remove-lazy-letrec.sls"
    "./ws/passes/optimizations/simple-merge-iterates.sls"
    "./ws/passes/ocaml_bkend/shared-emit-ml.sls"
    "./ws/passes/graphviz.sls"
    "./ws/passes/wavescope_bkend/nominalize-types.sls")
   ("./ws/passes/analyze_data_rates/annotate-with-data-rates.sls" "./ws/passes/wavescope_bkend/emit-c.sls"
    "./ws/passes/partition-graph.sls"
    "./ws/passes/normalize_source/eta-primitives.sls"
    "./ws/sim/wavescript_sim_library_push.sls"
    "./ws/passes/ocaml_bkend/emit-caml.sls"
    "./ws/passes/mlton_bkend/emit-mlton.sls")
   ("./ws/passes/normalize_source/remove-unquoted-constant.sls"
    "./ws/passes/wavescope_bkend/emit-c2.sls"
    "./ws/passes/normalize_source/desugar-misc.sls"
    "./ws/testing/lang_wavescript_tests.sls")
   ("./ws/passes/wavescope_bkend/emit-tbb.sls"
    "./ws/passes/wavescope_bkend/emit-tinyos.sls"
    "./ws/passes/static_elaborate/static-elaborate.sls"
    "./ws/passes/wavescope_bkend/emit-java.sls")
   ("./ws/passes/static_elaborate/degeneralize-arithmetic.sls"
    "./ws/passes/static_elaborate/split-union-types.sls"
    "./ws/passes/normalize_query/reduce-primitives.sls")
   ("./ws/passes/normalize_query/ws-remove-letrec.sls"
    "./ws/passes/normalize_query/remove-complex-constant.sls"
    "./ws/passes/static_elaborate/verify-elaborated.sls")
   ("./ws/passes/normalize_query/uncover-free.sls"
    "./ws/passes/normalize_query/ws-remove-complex-opera.sls"
    "./ws/passes/wavescope_bkend/anihilate-higher-order.sls"
    "./ws/passes/static_elaborate/interpret-meta.sls")
   ("./ws/passes/normalize_query/ws-lift-let.sls"
    "./ws/passes/small-ws-passes.sls")
   ("./ws/passes/wavescope_bkend/type-annotate-misc.sls")
   ("./main_r6rs.sls") ("./ws/testing/system_tests.sls")
   ("./main.sls") ("./ws/shortcuts.sls") ("regiment.ss")))
;(2 2 2 1 6 2 2 2 1 1 26 7 7 4 4 3 3 4 2 1 1 1 1 1 1)
;; 25 stages 87 compiles...
;; 3.48X speedup if compile times were uniform... probably aren't.


#|
;(pretty-print (flip-dag '(a (b (c)))))
(define b '(x (y) (z) (w (c) (d))))
(printf "FLIPPED: ~s\n" b)
(pretty-print (flip-dag (list b)))

(printf "DOUBLE FLIPPED: \n")
(pretty-print (flip-dag (flip-dag (list b))))


(printf "TRANS CLOSURE: \n")
(pretty-print (transitive-closure (list b)))

(printf "\n\n Par op prof:\n")
(define parprof (par-opportunity-profile (list b)))
(pretty-print parprof)

(printf "\nPotential parallel speedup: ~s\n" (inexact (/ (apply + parprof) (length parprof))))

|#

;; TODO: make this parallel, a la make -j 

(unless (eq? selected-action 'list)
  (display "Processing dependencies for: ")
  (display source)(newline))

(print-graph #t)

;(compile-loop (list source) '() '())
(let ;((dag (build-compile-dag source)))
     ((dag FULLDAG)
      (DEBUG #f))
  (when DEBUG
     (pretty-print dag) 
     ;(pretty-print (flip-dag dag))
     (printf "\n\n Par op prof:\n"))
  (let ;((slices (time (breadth-first-slices (transitive-closure (list dag))))))
       ((slices FULLSLICES))

    (when DEBUG 
      (pretty-print slices)
      (pretty-print (map length slices)))

    (IFPAR (init-par 4))
    (begin ;time 
     (for-each (lambda (stage)                 
		(IFPAR (par-map perform-action stage) 
		       (map perform-action stage))
		) slices))
    ;; On wasp, it takes 9.4s with 8 threads (38.8s cpu time, 2.5s collecting)
    ;;   (Most of that CPU time must be steal-spinning...)
    ;; With one thread it takes 6.5 seconds real time!!!
    ;; 2 threads -- 7.8
    ;; 4 threads -- 7.2

     (when DEBUG (IFPAR (par-status)))
    )

  ;(pretty-print (par-opportunity-profile (list dag)))
  )


(unless (eq? selected-action 'list)
  (display "Done.\n"))
