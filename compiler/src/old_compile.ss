;=============================================================================;
;; compiler.ss                                                               ;;
;;  This loads all the passes and defines the necessary utilities.           ;;
;; Jason Grinblat & Ryan Newton                                              ;;
;=============================================================================;


;; Certain compiler settings will not be as mutable as the compiler
;; parameters, hence they are represented simply as global variables:
(define scheme.NET-version (string->symbol "0.0"))

(define SETTINGS:COMPILER_DIR
  (let ([envvar (getenv "SCHEMENET_DIR")])
    (if envvar envvar "./")))

(define PREVIOUS_DIR (current-directory))
(current-directory SETTINGS:COMPILER_DIR)

(define SETTINGS:ARGS_HARDCODED 6)
;; We're assuming that the directory containing compiler.ss is
;; the current directory at load time.  Everything is relative to that:
(define SETTINGS:OUTPUT_DIR
  (string-append SETTINGS:COMPILER_DIR "bin\\"))
(define SETTINGS:BENCHMARK_DIR
  (string-append SETTINGS:COMPILER_DIR "benchmarks\\"))

;; Compiler Parameters:
;=============================================================================;
;; To properly use parameters, the procedures that access them should
;; establish local pointers to the parameter's closure at load-time.
;; Presently, access to these parameters by the compiler is done through
;; their global names -- somewhat defeating the point of parameters.
;; This is something to change when the compiler becomes self-hosting.

;; This controls the optimization level for the compiler:
;; This is also the only parameter that uses '-' as a separator
;; rather than '_'.  That's because this one is destined to be
;; user controllable from inside the scheme system.
(define snet-optimize-level
  (make-parameter
    0
    (lambda (x)
      (if (and (integer? x)
               (<= 0 x 2))
          x
          (error 'snet-optimize-level
                 "this parameter can only be set to a boolean: ~s"
                 x)))))

(define ilasm_command
  (make-parameter
    (if (getenv "ILASM") (getenv "ILASM")
        "ilasm.exe")
    (lambda (x)
      (if (string? x) x
          (error 'ilasm_command "must be a string: ~s" x)))))

;; This parameter stores a closure meant to transform PE filenames
;; into command line calls to whatever bytecode interpreter/compiler
;; is being used.  The default runs the PE directly--the method for
;; using the Microsoft .NET runtime.
(define dotnet_interpreter
  (make-parameter
    (lambda (file) file)
    (lambda (fun)
      (if (procedure? fun) fun
          (error 'dotnet_interpreter
                 "this parameter only accepts closures: ~s" fun)))))

;; These functions set dotnet_interpreter to three likely settings:
(define (ms-mode) (dotnet_interpreter (lambda (file) file)))
;; Mono mode depends on 'sed':
(define (mono-mode) (dotnet_interpreter
                      (lambda (fn)
                        (format "mono.bat ~s | sed -e '$d'" fn))))
(define (mint-mode) (dotnet_interpreter
                      (lambda (fn) (format "mint ~s" fn))))

;;; When this parameter is turned on, the compiler generates MSIL code
;;; that times itself and prints the results.
(define timing_info
  (make-parameter
    #f
    (lambda (x)
      (if (boolean? x) x
          (error 'timing_info
                 "this parameter can only be set to a boolean: ~s"
                 x)))))

;;; When this parameter is set to true, running the output of the
;;; last pass only assembles the MSIL code without executing it.
(define compile_only
  (make-parameter
    #f
    (lambda (x)
      (if (boolean? x) x
          (error 'compile_only
                 "this parameter can only be set to a boolean: ~s"
                 x)))))

;;; When this parameter is set to true, running the output of the
;;; last pass starts up a full Scheme.NET runtime (with symbol table).
;;; This is necessary for accessing library procedures, or for using
;;; primitives as first-class.
(define compiler_load_corelib
  (make-parameter
    #f
    (lambda (x)
      (if (boolean? x) x
          (error 'compile_load_corelib
                 "this parameter can only be set to a boolean: ~s"
                 x)))))

;;; This parameter determines to what files the compiler writes its output.
;;; Omit extensions, foo will result in foo.il, foo.exe
(define compiler_output_file
  (make-parameter
    "out"
    (lambda (x)
      (if (string? x) x
          (error 'compiler_output_file
                 "this parameter can only be set to a string: ~s"
                 x)))))

(define compiler_output_dir
  (make-parameter
    SETTINGS:OUTPUT_DIR
    (lambda (x)
      (if (string? x) x
          (error 'compiler_output_dir
                 "this parameter can only be set to a string: ~s"
                 x)))))

;;; This simple stops the compiler from printing any messages
(define compiler_silent
  (make-parameter
    #f
    (lambda (x)
      (if (boolean? x) x
          (error 'compiler_silent
                 "this parameter can only be set to a boolean: ~s"
                 x)))))

;;; This makes the command line compiler produce a .exe rather than .sdll
(define compiler_target_executable
  (make-parameter
    #f
    (lambda (x)
      (if (boolean? x) x
          (error 'compiler_target_executable
                 "this parameter can only be set to a boolean: ~s"
                 x)))))

;;; This makes the command line compiler produce a .exe rather than .sdll
(define compiler_DEBUGMODE
  (make-parameter
    #f
    (lambda (x)
      (if (boolean? x) x
          (error 'compiler_DEBUGMODE
                 "this parameter can only be set to a boolean: ~s"
                 x)))))

;;; This makes the command line compiler produce a .il rather than .sdll
(define compiler_target_il_file
  (make-parameter
    #f
    (lambda (x)
      (if (boolean? x) x
          (error 'compiler_target_il_file
                 "this parameter can only be set to a boolean: ~s"
                 x)))))

;;; This makes the command line compiler produce a module rather than a
;;; standalone assembly.  This also means that a .dll file will be produced
;;; rather than a .sdll.
(define compiler_target_module
  (make-parameter
    #f
    (lambda (x)
      (if (boolean? x) x
          (error 'compiler_target_module
                 "this parameter can only be set to a boolean: ~s"
                 x)))))

;=============================================================================;
(include "src/utils/match.ss")
(include "src/utils/helpers.ss")
(include "src/utils/driver.ss")
(include "src/utils/tests_noclosure.ss")
(include "src/utils/tests_quick.ss")
(include "src/utils/tests_medium.ss")
(include "src/utils/tests_slow.ss")
(include "src/utils/tests_new.ss")
(include "src/utils/tests_obj.ss")
(include "src/utils/tests.ss")

(include "src/emission.ss")      ;; Contains primitive definitions
;(include  "src/dump_primitives.ss")  -- not ready yet
(include "src/mangle_names.ss")

;; A glance at this will tell you which passes require
;; changes is the language:
(include "src/languages/language-mechanism.ss")
(include "src/languages/lang06.ss")  ;; remove-complex-constant
;                                 ;; uncover-symbol-references
(include "src/languages/lang08.ss")  ;; uncover-settable-language
;                                 ;; remove-impure-letrec!
(include "src/languages/lang10.ss")  ;; convert-case-lambda
;                                 ;; remove-set!
;                                 ;;    <CPS STUFF>
;                                 ;; remove-anonymous lambda
(include "src/languages/lang15.ss")  ;; uncover-free
(include "src/languages/lang16.ss")  ;; convert-closure
;                                 ;; lift-letrec
(include "src/languages/lang18.ss")  ;; introduce-closure-primitives
(include "src/languages/lang19.ss")  ;; convert-excessive-args
;                                 ;; reduce-primitives.1
;                                 ;; reduce-primitives.2
(include "src/languages/lang22.ss")  ;; normalize-context
(include "src/languages/lang23.ss")  ;; lift-letrec-body
(include "src/languages/lang24.ss")  ;; uncover-return
(include "src/languages/lang25.ss")  ;; uncover-calltype
;                                 ;; remove-nonunary-let
(include "src/languages/lang27.ss")  ;; uncover-local
;                                 ;; the-return-of-set!
(include "src/languages/lang29.ss")  ;; introduce-box
(include "src/languages/lang30.ss")  ;; flatten-if
(include "src/languages/lang31.ss")  ;; introduce-stack
;(include  "src/languages/lang32.ss") ;; clarify-types
(include "src/languages/lang33.ss")  ;; uncover-pushvariant
(include "src/languages/lang34.ss")  ;; uncover-maxstack
(include "src/languages/lang35.ss")  ;; generate-MSIL

(include "src/passes/pass00.ss")     ;; object-system-preprocess
(include "src/passes/pass01.ss")     ;; verify-scheme
(include "src/passes/pass02.ss")     ;; rename-var
(include "src/passes/pass03.ss")     ;; remove-implicit-begin
(include "src/passes/pass04.ss")     ;; remove-unquoted-constant
(include "src/passes/pass05.ss")     ;; remove-one-armed-if
(include "src/passes/pass06.ss")     ;; remove-complex-constant
(include "src/passes/pass07.ss")     ;; uncover-symbol-references
(include "src/passes/pass08.ss")     ;; uncover-settable
(include "src/passes/pass09.ss")     ;; remove-impure-letrec
(include "src/passes/pass10.ss")     ;; convert-case-lambda
(include "src/passes/pass11.ss")     ;; remove-set!
;; (include  "src/passes/pass12.ss") ;introduce-cps  -- removing for the moment
;; (include  "src/passes/pass13.ss")  -- Already commented [02.03.06]
(include "src/passes/pass14.ss")     ;; remove-anonymous-lambda
(include "src/passes/pass15.ss")     ;; uncover-free
(include "src/passes/pass16.ss")     ;; convert-closure
(include "src/passes/pass17.ss")     ;; lift-letrec
(include "src/passes/pass18.ss")     ;; introduce-closure-primitives
(include "src/passes/pass19.ss")     ;; convert-excessive-args
(include "src/passes/pass20.ss")     ;; reduce-primitives.1
(include "src/passes/pass21.ss")     ;; reduce-primitives.2
(include "src/passes/pass22.ss")     ;; normalize-context
(include "src/passes/pass23.ss")     ;; lift-letrec-body
(include "src/passes/pass24.ss")     ;; uncover-return
(include "src/passes/pass25.ss")     ;; uncover-calltype
(include "src/passes/pass26.ss")     ;; remove-nonunary-let
(include "src/passes/pass27.ss")     ;; uncover-local
(include "src/passes/pass28.ss")     ;; the-return-of-set!
(include "src/passes/pass29.ss")     ;; introduce-box
(include "src/passes/pass30.ss")     ;; flatten-if
(include "src/passes/pass31.ss")     ;; introduce-stack
;(include  "src/passes/pass32.ss")     ;; clarify-types
(include "src/passes/pass33.ss")     ;; uncover-pushvariant
(include "src/passes/pass34.ss")     ;; uncover-maxstack
(include "src/passes/pass35.ss")     ;; generate-MSIL

;(include  "src/optimizations/opt14a.ss")

;; I've added numbers alongside pass names; look at this chart when you're
;; doing (runcomp <exp> <number-of-passes>)
(define pass-names
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

;=============================================================================;
;; This modifies the scheme-start such that, if a heap is captured, it will
;; function as a command-line compiler.  I put it beneath the load's so that
;; the below code can refer to the module "emission-lib".
(scheme-start
  (lambda args
    ;(printf "Scheme Startup ~s ~s~n" args (length args))
    ;; This environment variable should trump our default setting,
    ;; but be trumped by a /ilasm <file> command-line option.
    (if (getenv "ILASM") (ilasm_command (getenv "ILASM")))
    (let loop ([outfile #f] [args args])
      (cond
        [(null? args)
         (printf "Scheme.NET command-line compiler.  Version ~a~n~n"
                 scheme.NET-version)
         (printf "Usage: snetc [Options] <sourcefile>~n~n")
         (printf "Options:~n")
         (printf "  /o <outputfile>   specify output location~n")
         (printf "  /exe              produce a standalone PE file~n")
         (printf "  /il               produce MSIL, don't assemble~n")
         (printf "  /debug            don't delete intermediate files~n")
         (printf "  /opt <number>     specify optimize level~n")
         (printf "  /dumptests        output a scheme program ~a~n"
                 "for testing the compiler")
         (printf "  /testall          run all tests without loading ~a~n"
                 "the Corelib")
	 (printf "  /mono             use settings for mono")
	 (printf "  /mint             use settings for mono-interpreter")
	 
         (printf "  /ilasm <command>  use <command> to call ilasm~n")
         (exit 1)]
        [(equal? (car args) "/mono") (mono-mode) (loop outfile (cdr args))]
        [(equal? (car args) "/mint") (mint-mode) (loop outfile (cdr args))]
        [(equal? (car args) "/ilasm")
         (when (null? (cdr args))
           (printf "/ilasm must be followed by a filename~n")
           (exit 1))
         (printf "Overriding default ilasm command with: ~s~n" (cadr args))
         (ilasm_command (cadr args))
	 (loop outfile (cddr args))]  ;; Not sure about this line -RRN 2003.09.12
        [(equal? (car args) "/dumptests")
         (printf "~s~n" (make-standalone-test tests))]
        [(equal? (car args) "/testall")
         (test-all) (exit 0)]
        [(equal? (car args) "/silent")
         (compiler_silent #t)
         (loop outfile (cdr args))]
        [(equal? (car args) "/debug")
         (compiler_DEBUGMODE #t)
         (loop outfile (cdr args))]
        [(equal? (car args) "/exe")
         (compiler_target_executable #t)
         (loop outfile (cdr args))]
        [(equal? (car args) "/o")
         (when (null? (cdr args))
           (printf "/o must be followed by an output filename~n")
           (exit 1))
         (loop (cadr args) (cddr args))]
        [(equal? (car args) "/opt")
         (when (or (null? (cdr args))
                   (not (number? (cadr args))))
           (printf "/opt must be followed by an optimize level~n")
           (exit 1))
         (printf "Optimization level set from command-line flag.~n")
         (snet-optimize-level (string->number (cadr args)))
         (loop outfile (cddr args))]
        [(equal? (car args) "/il")
         (compiler_target_il_file #t)
         (loop outfile (cdr args))]
        [(equal? (car args) "/module")
         (compiler_target_module #t)
         (loop outfile (cdr args))]
        
        ;-----------------------------------------------
        ; These flags are undocumented, for my use only.
        ;;; This allows a name to be specified for the toplevel class
        ;;; into which the generated code is dumped.  Right now I just
        ;;; use this for giving SchemeCoreLib.dll a different name than
        ;;; "SchemeObjectFile".  In general, this option only makes sense
        ;;; when /exe is also enabled; for an SDLL to be loaded, the class
        ;;; name must be "SchemeObjectFile".
        [(equal? (car args) "/name")
         (when (null? (cdr args))
           (printf "/name must be followed by an intended class name~n")
           (exit 1))
         (let ()
           (import emission-lib)
           (set! MSIL-emission-namespace (cadr args)))
         (loop outfile (cddr args))]
        [(equal? (car args) "/outdir")
         (when (null? (cdr args))
           (printf "/outdir must be followed by a directory~n")
           (exit 1))
         (compiler_output_dir (cadr args))
         (loop outfile (cddr args))]
        
        ;-----------------------------------------------
        [else
          (if outfile
              (snet-compile-file (car args) outfile)
              (snet-compile-file (car args)))
          (exit 0)]))))

(suppress-greeting #t)
;=============================================================================;

(define repl
  (lambda ()
    (printf "~n# ")
    (let ([input (read)])
      (if (eq? input 'exit)
          (printf "~n")
          (begin
            (printf "~a~n"
                    (eval (runcomp input)))
            (repl))))))

;; (runcomp <prog> <flags> ...)
;;  Possible flags:
;;    A number n -- only runs the first n passes.
;;    'timing    -- generates an executable that times itself.
;;    '(output <filename-string>) -- writes .il output to specified file.
(define runcomp
  (letrec ([runcomploop
             (lambda (pgm passes)
               (cond
                 [(null? passes) pgm]
                 [else (runcomploop ((car passes) pgm)
                                    (cdr passes))]))])
    (case-lambda
      [(pgm) (runcomploop pgm (map eval pass-names))]
      ;; If a flag is passed, it is one of two things:
      ;; 1) a number specifying how many passes to run.
      ;; 2) 'timing to denote a desire that the final
      ;;    MSIL code include a mechanism for printing
      ;;   program runtime (realtime milleseconds).
      [(pgm . flags)
       ;; Verify that all are valid flags:
       (for-each
         (lambda (flag)
           (match flag
             [,n (guard (integer? n)) #t]
             [timing #t]
             [(output ,file) (guard (string? file)) #t]
             [,unmatched
               (error 'runcomp "invalid flag: ~a" unmatched)]))
         flags)
       (let ([passes
               (let ([number-flags (filter number? flags)])
                 (cond
                   [(null? number-flags) pass-names]
                   [(= 1 (length number-flags))
                    (list-head pass-names (car number-flags))]
                   [else (error 'runcomp
                                "You can't provide two numeric flags: ~n~a"
                                `(runcomp <pgm> ,@flags))]))])
         (parameterize
           ([timing_info
              (if (memq 'timing flags) #t #f)]
            [compiler_output_file
              (let ([output-flags
                      (filter
                        (lambda (x)
                          (match x
                            [(output ,s) s]
                            [,other #f]))
                        flags)])
                (cond
                  [(null? output-flags) (compiler_output_file)]
                  [(= 1 (length output-flags)) (car output-flags)]
                  [else (error 'runcomp
                               "You can't provide two output-files: ~n~a"
                               `(runcomp <pgm> ,@flags))]))])
           ;---------------------------------------
           (runcomploop pgm (map eval passes))
           ;---------------------------------------
           ))])))

;=============================================================================;
;; I don't know how to write this to provide a proper return value:
(define silent-system
  (lambda (cmd)
    (let ([input (car (process cmd))])
      (let loop ()
        (unless (eof-object? (read-char input))
          (loop)))
      0)))

(define snet-compile-file
  (let ([do-it
          (lambda (infile middlefile outfile)
	    (printf "DEBUG: Running snet compile file ...\n")
	    (flush-output-port (current-output-port))

            (when (not (compiler_silent))
              (if (compiler_target_il_file)
                  (printf "snet-compile-file: ~a -> ~a~n"
                          infile middlefile)
                  (printf "snet-compile-file: ~a -> ~a~n"
                          infile outfile)))
            (if (file-exists? infile)
                (let ([in (open-input-file infile)]
                      [out (open-output-file middlefile 'replace)])
                  (dynamic-wind
                    (lambda () (void))
                    (lambda () (snet-compile-port in out))
                    (lambda () (close-port in) (close-port out)))

		  (let ([command 
			 ((dotnet_interpreter)
			  (format "~a /QUIET ~a ~a /out=~a"
				  (ilasm_command)
				  (if (compiler_target_executable)
				      "/EXE" "/DLL")
				  middlefile
				  outfile))])
		    (unless (compiler_target_il_file)
                    ;;; How can I use silent and still find out if ilasm failed?
			    (printf "DEBUG: running ilasm... ~s\n" command)
			    (flush-output-port (current-output-port))
			    
			    (let ([result (if (compiler_silent) 
					      (silent-system command) 
					      (system command))])
			      ;; If ilasm exited with an error, we exit with an error:
			      (unless (zero? result)
				      (printf "ILASM exited with error code: ~s~n" result)
				      (flush-output-port (current-output-port))
				      (exit result)))
			    ;; If we're targetting SDLL or EXE, delete .il file:
			    (unless (compiler_DEBUGMODE) (delete-file middlefile)))))
		  (error 'snet-compile-file
			 "Input file does not exist: ~a" infile)))])
    (define extract-extension
      (lambda (filename)
        (let loop ([charls (reverse (string->list filename))]
                   [acc '()])
          (cond
            [(null? charls) ""]
            [(or (eq? (car charls) #\\)
                 (eq? (car charls) #\/)) ""]
            [(eq? (car charls) #\.) (list->string acc)]
            [else (loop (cdr charls) (cons (car charls) acc))]))))
    (define replace-extension
      (lambda (filename newext)
        (let loop ([charls (reverse (string->list filename))])
          (cond
            [(null? charls) ""]
            [(or (eq? (car charls) #\\)
                 (eq? (car charls) #\/)) ""]
            [(eq? (car charls) #\.)
             (list->string (reverse
                             (append (reverse (string->list newext))
                                     charls)))]
            [else (loop (cdr charls))]))))
    ;;============================
    (case-lambda
      [(infile)
       (let ([infile
               (cond
                 [(equal? (extract-extension infile) "ss") infile]
                 ;[(file-exists? infile) infile]
                 [else (string-append infile ".ss")])])
         (do-it infile
                (replace-extension infile "il")
                (replace-extension infile
                                   (if (compiler_target_executable)
                                       "exe"
                                       (if (compiler_target_module)
                                           "dll"       ;; module
                                           "sdll")))))] ;; Scheme DLL
      [(infile outfile)
       (let ([middle
               (if (equal? (extract-extension infile) "ss")
                   (replace-extension infile "il")
                   (string-append infile ".il"))])
         (do-it infile middle outfile))])))

(define snet-compile-port
  (lambda (in out)
    (let loop ([exp (read in)]
               [classes '()]
               [loadcode '()]
               [sym-acc '()])
      (if (eof-object? exp)
          ;;-------------------------------------------------------
          (let ()  ;; Write the header and be done:
            ;;;; PULL OUT THIS DUPLICATED CODE!!! (see combine)
            (import emission-lib)
            
            (load-string-emission) ;; Correct mode for this job.
            (clear-buffer!)
            
            (unless (compiler_target_module)
              (emit ".assembly ~a {}" MSIL-emission-namespace))
            
            (emit "")
            (emit ".assembly extern SchemeRuntime {}") (emit "")
            (emit ".assembly extern mscorlib {}")
            (emit ".class public ~a" MSIL-emission-namespace)
            (emit "{") (indent!)
            (emit ".field static private class ~a"
                  "[SchemeRuntime]Runtime theruntime")
            (emit ".field static private object flipper")
            (emit "")
            
            (emit ".class nested public ~a {" symref-subclass)
            (indent!)
            (for-each
              (lambda (sym)
                (emit ".field public static class ~a~a"
                      "[SchemeRuntime]ScmSymbol " sym))
              sym-acc)
            (unindent!)(emit "}")(emit "")
            
            ;; Each class def is a list of lines:
            (for-each (lambda (c) (for-each emit c) (emit "")) classes)
            
            ;;    --------------------------------
            (emit ".method public static object Load~a"
                  "(class [SchemeRuntime]Runtime theruntime) {")
            (indent!)
            (emit ".maxstack 8")  ;;; Make sure this is enough.
            (emit ".locals ( object ret_v, ~a"
                  "class [SchemeRuntime]Symboltable thesymtab )")
            (emit "")
            
            (emit "ldarg theruntime")
            (emit "stsfld class [SchemeRuntime]Runtime ~a::theruntime"
                  MSIL-emission-namespace)
            (emit "")
            
            ;; Wire top level variable references:
            (emit "// Wire up top level varrefs to the symbol table:")
            (emit "ldarg theruntime")
            (emit "ldfld class [SchemeRuntime]Symboltable ~a"
                  "[SchemeRuntime]Runtime::symtable")
            (emit "stloc thesymtab")
            ;; Fill in our local sym fields:
            (for-each
              (lambda (sym)
                (let ([rawsym (unmangle-name sym)])
                  (emit "ldloc thesymtab")
                  (emit "ldstr \"~a\"" rawsym)
                  (emit "callvirt void [SchemeRuntime]Symboltable~a"
                        "::NullBind(string)")
                  (emit "ldloc thesymtab")
                  (emit "ldstr \"~a\"" rawsym)
                  (emit "callvirt instance object [SchemeRuntime]~a"
                        "Symboltable::get_Item(object)")
                  (emit "castclass [SchemeRuntime]ScmSymbol")
                  (emit "stsfld class [SchemeRuntime]ScmSymbol ~a/~a::~a"
                        MSIL-emission-namespace symref-subclass sym)))
              sym-acc)
            (emit "")
            
            ;; Here we strip the last instruction, which is a 'pop'
            ;; Thus we return the value of the last expression in the
            ;; scheme file, as the value of this Load method.
            ;; (This is used for the 'eval' primitive.)
            (if (not (null? loadcode))
                (for-each emit (rdc loadcode))) ;;; <- MAIN LOAD STUFF
            (emit "ret")
            (unindent!) (emit "}")(emit "")
            ;;    --------------------------------
            
            (emit ".method public static void Run() {")
            (indent!) (emit ".entrypoint")
            (emit "ldc.i4.1")
            (emit "newobj instance void [SchemeRuntime]Runtime::.ctor(bool)")
            (emit "call object ~a::Load(class [SchemeRuntime]Runtime )"
                  MSIL-emission-namespace)
            (emit "pop")
            (emit "ret")
            (unindent!)(emit "}")
            (unindent!) (emit "}")
            (display (get-buffer) out))
          ;;-------------------------------------------------------
          
          ;; If exp is not eof-object, keep recurring and compiling:
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;; SYNTAX EXPANSION                            ;;;
          ;;; Here we use Chez Scheme's syntax expander;  ;;;
          ;;; this should change when we have our own.    ;;;
          ;;; We must parameterize chez's optimize level, ;;;
          ;;; otherwise it could #% our primitives for us.;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (let ([exp
                  (begin
                    ;;; This is unfortunate, but here I nuke chez scheme's
                    ;;; \#primitive syntax, because we're using chez's expander
                    ;;; and it wouldn't expand #%prim if prim is not a
                    ;;; chez primitive.
                    (eval '(define-top-level-value  '\#primitive 393))
                    (eval '(define-syntax
                             \#primitive (syntax-rules ()
                                           [(_ x) (\#primitive2 x)]
                                           [(_ n x) (\#primitive2 n x)])))
                    (eval '(define \#primitive2 (case-lambda [(x) x] [(n x) x])))
                    ;;; I don't want to abandon it for my own syntax though,
                    ;;; because I like writing #%prim.
                    ;;; Unfortunately, my abuse of chez here produces odd results:
                    ;;;  (expand '#%blahblah) -> (\#primitive2 blahblah)
                    ;;;  (expand '#%car) -> (\#primitive2 #%car)
                    ;;; Pass00 cleans these up.
                    (parameterize ([optimize-level 0])
                      (expand exp)))])
            (match exp  ;; cheesily handle (define (f x) ...) for now:
              [,exp (match (runcomp exp)
                      [(generate-MSIL-language
                         ',sym* ',pkg* ',class-defns* ',lambda-classes ',code)
                       (loop (read in)
                             (append classes lambda-classes)
                             (append loadcode '("") code '("pop"))
                             (union sym* sym-acc))])]))))))

;==============================================================================;

(define test-to-file
  (lambda (x file)
    (parameterize ([pretty-maximum-lines #f]
                   [print-level #f]
                   [print-length #f]
                   [print-graph #f])
      (let ([out (open-output-file file 'replace)]
            [oldout (current-output-port)]
            [oldtrace (tracer)])
        (dynamic-wind
          (lambda ()
            (current-output-port out)
            (tracer #t))
          (lambda ()
            (test-one x))
          (lambda ()
            (current-output-port oldout)
            (close-output-port out)
            (tracer oldtrace)
            ))))))

(define (ttf x)
  (test-to-file x
                (string-append SETTINGS:COMPILER_DIR "temp.ss")))

(define outonly
  (lambda (x)
    (let ([o (open-output-file
               (string-append SETTINGS:OUTPUT_DIR "out.il")
               'replace)])
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          (let ((res (runcomp x)))
            (display (cadr res) o)))
        (lambda () (close-port o))))))

(define run (lambda (x) (ttf x) (out x)))

(current-directory PREVIOUS_DIR)
