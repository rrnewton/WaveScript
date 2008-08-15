#! /bin/bash
#|
exec regiment.ikarus i --script $0 ${1+"$@"}
|#
;;exec mzscheme -qr "$0" ${1+"$@"}

;; This script can't be run with regiment.chez -- that's circular.

;; Running it with regiment.plt currently causes a couple problems, it
;; prints mutable lists with {}, and I get a weird #\alarm character.

(printf "Running script to compact r6rs files into one large Chez compatible file.\n")
(printf "  command line: ~s\n" (command-line))

(define pretty (member "-pretty" (command-line)))

(define spec->filename
  (lambda (spec)
    (match spec
      [,sym (guard (symbol? sym)) sym] ;; HACK
      [(prefix ,[spec] ,sym) spec]
      [(,sym* ...) (guard (andmap symbol? sym*))
       (** "./" (apply ** (insert-between "/" (map symbol->string sym*))) ".sls")])))

(define spec->name
  (lambda (spec)
    (match spec
      [(prefix ,[spec] ,sym) spec]
      [(,sym* ...) (guard (andmap symbol? sym*))
       (apply symbol-append (insert-between '_ sym*))])))

;; Hacky and fragile:
(define export-exceptions
  '(reg:define-struct
    let/ec call/ec
    current-error-port
    inspect
    wavescript-language
    ))

(define good? (lambda (s) (not (memq s export-exceptions))))

(define (exports->names exp)
  (define acc '())
  (define newexp   
    (match exp
      [() '()]
      [((rename ,renames ...) . ,[rest])
       (set! acc (append renames acc))
       (append (filter good? (map cadr renames))
	       ;; PUTTING BOTH SYMBOLS IN:
	       (filter good? (map car renames))	       
	       rest)]
      [(,first . ,[rest]) (guard (symbol? first)) 
       (if (memq first export-exceptions)
	   rest
	   (cons first rest))]))
  ;(unless (null? acc) (inspect acc))
  (values acc newexp))


(define (convert-specs specs)
  (printf "Converting Import specs:\n")
  (when (file-exists? "chez_aggregated.ss") (delete-file "chez_aggregated.ss"))
  (let ([prt (open-output-file "chez_aggregated.ss")])
    (pretty-print '(include "temporary_chez_compat.ss") prt) (newline prt)
    ;; This is REALLY weird, but ifchez is becoming unbound AFTER it's bound at top-level.
    (pretty-print '(define-syntax IFCHEZ
		     (syntax-rules ()
		       [(_ a b) a])) prt) (newline prt) (newline prt)
    (for-each (lambda (fn)
		(printf "  Converting ~a \n" fn)
		(if (eq? fn 'language-mechanism)
		    (fprintf prt "\n\n(include \"ws/langs/language-mechanism.ss\")\n\n")
		  (match (file->slist fn)
		   [((library ,name (export ,exp* ...) (import ,_ ...) ,bod* ...))
		    (define symname (spec->name name))
		    (define-values (renames newexp*) (exports->names exp*))
		    (define aliases
		      (map (lambda (pr) `(alias ,(cadr pr) ,(car pr))) renames))
		    (define imports '())
		    (for-each (lambda (e) (ASSERT symbol? e)) newexp*)		    

		    ;ws_sim_wavescript_sim_library_push
		    (when (equal? name '(ws sim wavescript_sim_library_push))
		      (printf "  ** Special case for sim_wavescript_sim_library_push\n")
		      (set! imports (append '((import (add-prefix scheme s:))
					      (import (add-prefix ws_util_slib_hashtab slib:))
					      )
					    imports)))
		    (cond
#;
		     [(equal? name '(ws util iu-match))
		      (printf " ** Ignoring iu-match, substituting match.ss\n")
		      (pretty-print '(include "old/chez/match.ss") prt) (newline prt)
		      (pretty-print '(import iu-match) prt)(newline prt)(newline prt)]
		     [(equal? name '(ws common)) 
		      (printf " ** Ignoring ws common\n")
		      (void)]		     		     
		     [else  
		      ;; A compromise, we don't waste time pretaty printing, but we do print each def on its own line.
		      
		      (if pretty
			  (pretty-print `(module ,symname ,newexp* ,@imports ,@bod* ,@aliases) prt)
			  (begin (fprintf prt "(module ~a ~s\n" symname newexp*)
				 (for-each (lambda (x) (pp x prt)) imports)				 
				 (for-each (lambda (x) (display "  " prt)(write x prt) (newline prt)) bod*)
				 (for-each (lambda (x) (pp x prt)) aliases)
				 (fprintf prt ") ;; end module\n" ))
			  )		      
		       (unless (equal? name '(ws sim wavescript_sim_library_push))
			 (write `(import ,symname) prt)(newline prt)(newline prt))
		       ])
		     ])
		    ))
      (map spec->filename specs))))

#;
(match (file->slist "main_r6rs.sls")
  [((library ,name (export ,exp ...) (import ,specs ...)))
   (convert-specs )
   ])


(define all-imports
  (filter (lambda (imp) (and (not (equal? imp '(ws common)))
			    (not (equal? imp '(ws compat compat)))
			    ))
   (append (file->slist "common_import_list.sexp")
	   '(language-mechanism)
	   (file->slist "main_r6rs_import_list.sexp")
	   '((main))
	   )))

;(convert-specs (list-head all-imports 72))
(print-graph #f)
(convert-specs all-imports )
