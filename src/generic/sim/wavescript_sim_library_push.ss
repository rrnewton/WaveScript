
;;;; TODO: Replace output-queue with real queue!! 
(module wavescript_sim_library_push mzscheme
  (require 
           "../constants.ss"
	   "../../plt/iu-match.ss"
           "../util/fft.ss"
           "../langs/lang_wavescript.ss"
           "../../plt/engine.ss"
           "../../plt/hashtab.ss"
	   "../../../depends/matpak.ss"
           (prefix slib: "../util/slib_hashtab.ss")
	   (all-except "../util/imperative_streams.ss" test-this )
	   (all-except "../util/helpers.ss" test-this these-tests for inspect break)	   
	   (all-except "../compiler_components/regiment_helpers.ss" test-this these-tests for inspect break)
           "../compiler_components/type_environments.ss"
	   )
  (provide
;                 make-sigseg sigseg-start sigseg-end sigseg-vec sigseg-timebase
		 valid-sigseg?
		 app foreign-app let Mutable:ref deref static statref

       annotations
		 
		 wscase construct-data 

		 run-stream-query reset-wssim-state! print-wssim-state

		 __readFile 
		 __foreign __foreign_source inline_C inline_TOS
					;__syncN
		 ;; Just stubs that give errors:
		 ensBoxAudio ensBoxAudioF ensBoxAudioAll

		 ;dump-binfile 
		 ;audio 
		 timer timer-bench
		 show __show_ARRAY __backtoSTR
		 gnuplot_array gnuplot_array_stream gnuplot_sigseg_stream
		 gnuplot_array2d gnuplot_array_stream2d gnuplot_sigseg_stream2d
		 gnuplot_process spawnprocess
		 prim_window

		 ;; Unmarshall from strings:
		 to-uint16 to-int16 uint16->string

		 wsequal?

		 gint
		 g+ g- g* g/ g^
		 _+_ _-_ *_ /_ ^_
		 _+. _-. *. /. ^.
		 _+: _-: *: /: ^:
		 _+D _-D *D /D ^D
		 _+I16 _-I16 *I16 /I16 ^I16
		 _+I32 _-I32 *I32 /I32 ^I32
		 _+I64 _-I64 *I64 /I64 ^I64

		 _+U16 _-U16 *U16 /U16 ^U16

		 sqrtF sqrtC sqrtI moduloI
		 absI absF absD absC absI16 absI64
		 randomI
		 logD logF ;logI 
		 exptI exptD exptF
		 ;modI modF 
		 roundF		 
		 
		 makeComplex
		 
		 cast_num __cast_num

		 ;uint16ToInt     uint16ToInt64  uint16ToFloat uint16ToDouble uint16ToComplex

		 int16ToInt     int16ToInt64  int16ToFloat int16ToDouble int16ToComplex
		 int64ToInt16   int64ToInt    int64ToFloat int64ToDouble int64ToComplex

		 intToInt16     intToInt64     intToFloat   intToDouble   intToComplex 
		 floatToInt16   floatToInt64   floatToInt   floatToDouble floatToComplex 
		 doubleToInt16  doubleToInt64   doubleToInt   doubleToFloat doubleToComplex 
		 complexToInt16 complexToInt64 complexToInt complexToDouble complexToFloat
		 
		 stringToInt stringToFloat stringToDouble stringToComplex
		 __stringToInt_ARRAY __stringToFloat_ARRAY __stringToDouble_ARRAY __stringToComplex_ARRAY
		 intToChar charToInt
		 String:length String:explode String:implode
		 
		 ;toComplex toFloat  ;; Generic versions
		 ;toInt ;; Truncate
		 ;roundToInt

		 realpart imagpart 

		 nullseg Array:null nulltimebase
		 tuple tupref
		 Array:length Array:make Array:makeUNSAFE
		 Array:ref Array:set Array:map Array:build Array:fold Array:toList Array:andmap
		 internString uninternString
		 
		 HashTable:make HashTable:contains HashTable:get HashTable:set HashTable:set_BANG HashTable:rem HashTable:rem_BANG
		 HashTable:foreach

		 List:ref List:append List:reverse List:map List:fold List:length List:make List:is_null
		 List:head List:tail head tail
		 List:assoc List:assoc_update
		 List:build List:toArray List:zip

		 joinsegs subseg seg_get width start end timebase Secret:newTimebase
		 toArray toSigseg 

		 assert-type
		 
		 wserror __wserror_ARRAY inspect 
		 emit return
		 ;smap sfilter
		 iterate break ;deep-iterate

       iterate-bench
       
		 feedbackloop
		 ;; TODO: nix unionList.
		 _merge _merge-bench unionN unionN-bench unionList unionList-bench
		 ;zip2
		 ; union2 union3 union4 union5
		 fftC ifftC fftR2C ifftC2R memoized_fftR2C
		 
		 ;; Misc, ad-hoc, and Temporary
		 m_invert ;; A matrix inversion.

		 while

		 ;; A foreign procedure for freeing external memory:
		 C-free exclusivePtr getPtr nullPtr ptrIsNull ptrMakeNull

		 readFile-wsearly FILE_EXISTS GETENV SHELL SETCPU SETCPUDEEP
		 clock realtime getID

		 IFPROFILE

		 ;HACK_O_RAMA

		 )
    (chezprovide (for for-loop-stack )
;		 letrec 
		 print

		 ;+ - * / ^
                                 
                 ;parmap
                 
		 ;; We reexport these *module names* so that they can be imported subsequently.
		 mod_scheme  mod_helpers  mod_constants
;		 quasiquote unquote lambda
		 ;; import itself--this is so we can use import-only:
		 import
		 )
    (chezimports (only scheme scheme import)
		 constants
		 helpers
		 (except streams test-this)
		 (only lang_wavescript ws-show
		       ))

    
    (IFCHEZ
     (begin 
       (alias mod_helpers helpers)
       (alias mod_scheme scheme)
       (alias mod_constants constants)  
       ;  (alias quasiquote quasiquote)  
       ;;  (alias unquote unquote) 
       ;; (alias lambda lambda)  
       (alias let let) ;; We assume type info has been stripped.      
       
       (import (add-prefix scheme s:))
       ;(alias begin s:begin)       
       )
     (begin 
	    (require (prefix s: mzscheme))
	    (require (prefix s: "../../plt/chez_compat.ss"))
;	    (provide (all-from "../util/imperative_streams.ss"))
	    ))

    ;; Used to bind wavescript primitives to equivalent identifiers in Scheme.
    (define-syntax define-alias 
      (syntax-rules ()
	[(_ v e) (define v e)]))


  ;; ================================================================================
  ;; TYPES USED 

  ;; Sources :: 'peek -> time | 'pop -> DataElement
  ;; Sink    ::  DataElement -> ()
  ;; Stream  ::  Sink -> ()

  ;; Streams take sinks and register them.
  ;; Sources allow either "peeking" the time of their next element, or
  ;; popping off thatelement.

  ;; This is annoying.
  ;; Generic equality doesn't actually work for records.
  ;;
  ;; And because we canc have sigsegs down inside lists or vectors, we
  ;; need to handle those cases too.
  (define (wsequal? a b)   
    (cond
     [(sigseg? a) 
      (and (= (sigseg-start a) (sigseg-start b))
	   (s:equal? (timebase-num (sigseg-timebase a)) 
		     (timebase-num (sigseg-timebase b)))
	   ;; This may not recursively contain sigsegs:
	   (s:equal? (sigseg-vec a) (sigseg-vec b)))]     
     [(pair? a) (and (pair? b) 
		     (wsequal? (car a) (car b)) 
		     (wsequal? (cdr a) (cdr b)))]
     [(vector? a) (and (vector? b) 		       
		       (fx= (vector-length a) (vector-length b))
		       (let loop ([i 0])
			 (or (fx= i (vector-length a))
			     (and (wsequal? (vector-ref a i) (vector-ref b i))
				  (loop (fx+ 1 i))))))]
     [(tuple? a) (and (tuple? b)
		      (wsequal? (tuple-fields a) (tuple-fields b)))]
     ;; Even more inefficiency:
     [(number? a) (= a b)] ;; Matters for 0.0 == -0.0
     [else (s:equal? a b)]))
  ;(define equal? wsequal?)
  

  ;(reg:define-struct (wsbox outports))
  ;(reg:define-struct (wsevent time source))
  ;; This structure represents a simulation.
  ;(reg:define-struct (wssim current-vtime data-sources))


  ;; ================================================================================    

  ;; Global queue of input events in virtual time.  
  ;; Currently pairs of (time . source)
  (define event-queue   'wslp-uninit1)    
  (define current-vtime 'wslp-uninit2)   ;; virtual microseconds 
  (define data-sources  'wslp-uninit3)   ;; A list of all data sources in the query graph
  (define output-queue  'wslp-uninit4)   ;; Outputs from the query graph
  (define global-eng    'wslp-uninit5)   ;; Engine for running stream graph. 

  ;; [2007.08.02] There's currently no mechasism for backpressure, and
  ;; readFile's are now in the interior of the stream graph rather
  ;; than being sources.  So currently we just shut the whole thing
  ;; down when any readFile runs out of data.
  ;; Mutable state, this needs to be handled very carefully!!!
  (define still-running? #t) ;; Has the query shut down yet?
  (define (stop-WS-sim! msg) 
    (eprintf "\nStopping WS Sim: ~s\n" msg)
    ;(reset-state!)
    (set! still-running? #f))

  ;; Reset the global state.
  (define (reset-wssim-state!)
    ;(print-wssim-state)
    ;(printf "RESETTING GLOBAL SIM STATE!\n")
    (set! event-queue '())
    (set! current-vtime 0) 
    (set! data-sources '())
    (set! output-queue '())
    (set! still-running? #t))

  (define (print-wssim-state)
    (printf "  Event-queue: ~s\n  Vtime: ~s\n  data-sources: ~s\n  output-queue: ~s\n"
	    event-queue current-vtime data-sources output-queue))

  (define output-sink (lambda (x) (set! output-queue (cons x output-queue))))
  
  ;; Launch a stream element to all sinks.
  (define-syntax fire!
    (syntax-rules ()
      ((_ elem sinks) (for-each (lambda (f) (f elem)) sinks))))
  (define (profiled-fire! tuple sinks bench-rec output-type sumdecls)
    (bench-stats-bytes-add!  bench-rec (datum->width output-type tuple sumdecls))
    (bench-stats-tuples-add! bench-rec 1)
    (fire! tuple sinks))

  ;; These do nothing.  Scheme's bindings are mutable to start.
  (define-syntax Mutable:ref (syntax-rules () [(_ x) x]))
  (define-syntax deref (syntax-rules () [(_ x) x]))
  (define-syntax static (syntax-rules () [(_ x) x]))
  (define-syntax statref (syntax-rules () [(_ x) x]))

  (define-syntax annotations (syntax-rules () [(annotations . x) '(annotations . x)]))

  ;;
  (define (bench-stats-bytes-add!    bs bytes) (set-bench-stats-bytes!    bs (+ bytes (bench-stats-bytes bs))))
  (define (bench-stats-tuples-add!   bs bytes) (set-bench-stats-tuples!   bs (+ bytes (bench-stats-tuples bs))))
  (define (bench-stats-cpu-time-add! bs bytes) (set-bench-stats-cpu-time! bs (+ bytes (bench-stats-cpu-time bs))))

  ;; ================================================================================    
  ;;; Type tests for WaveScript types embedded in Scheme.

  ;;; These are a bit sketchy because they make specific assumptions
  ;;; about the *representations* used by Scheme for numbers.  (I.e. a
  ;;; number is floating point) rather than the meaning (a number is non-integral).
  
  ;;; In particular, we assume that inexact arithmetic yields is closed.
  ;;; (Even if the output is an integer, it's still represented as floating point.)
  ;;; We also assume that complex arithmetic is closed for the complex representation.

  (define ws-int?     fixnum?)
  (define ws-float?   flonum?)
  ;; Problems in PLT!
  ;; Hacking this to #T
  (IFCHEZ (define ws-complex? cflonum?)
	  (define (ws-complex? n) #t))
 
  ;; ================================================================================    

  ;; May be more useful than (time ...) for measuring small computations.
  (define (measure-thunk-ticks th)
    (let loop ([eng (make-engine th)]
	       [acc 0])
      (eng 1000000 
	   (lambda (remaining res)
	     (+ acc (fx- 1000000 remaining)))
	   (lambda (e2) (loop e2 (+ acc 1000000)))
	   )))

  ;; converts hertz to microseconds:
  (define (rate->timestep freq)
    (when (zero? freq) (error 'rate->timestep "sampling rate of zero is not permitted"))
    (flonum->fixnum (s:* 1000000 (s:/ 1.0 freq))))
  
  ;; This takes a push-based stream and wraps it in an engine to make
  ;; a psuedo pull-based stream (a la streams.ss).
  ;;
  ;; run-stream-query :: prog -> Stream('a)
  (define (run-stream-query prog)     

    ;; Note on time-slice granularity:
    ;;   I experimented with this using demo7b.
    ;;     10^0 -> 27 sec
    ;;     10^1 -> 3.8 sec
    ;;     10^2 -> 1.3 sec
    ;;     10^3 -> .95 sec
    ;;     10^4 -> .9  sec
    ;;     10^5 -> .9  sec
    ;(define time-slice 100000)
    (define time-slice (IFDEBUG 200 700))
    
    ;(ASSERT procedure? prog)
    
    (if #t;(procedure? prog)
	(begin  
	  (prog output-sink) ;; Register data sources, connect to output sink.
	  (set! global-eng 
		(make-engine
		 (lambda ()
		   (define (compose-fork f g) (lambda (x y) (f (g x) (g y))))
		   (define cmpr-entry (compose-fork < car))

		   ;; Seed the event queue:
		   (set! event-queue (map (lambda (s) (cons (s 'peek) s)) data-sources))
		   
		   (let global-loop ()	       
					;(printf "ENG LOOP: time:~s out:~s  evts:~s\n" current-vtime output-queue event-queue)
		     (if (null? event-queue)
			 '();(engine-return '())

			 (let* ([next (car event-queue)]
				[src (cdr next)])
			   (set! event-queue (cdr event-queue))
			   (set! current-vtime (car next))
			   ;; Fire one more element from this data source.
			   (src 'pop)
			   ;; Add the next event for this source to the queue.
			   (let ([newentry (cons (src 'peek) src)])
			     ;; Only add new events if we're still running:
			     (if (and (car newentry) still-running?)
				 (set! event-queue (merge! cmpr-entry (list newentry) event-queue)))
			     (global-loop)
			     ))))
		   )))
	  ;; Return a stream:
	  (delay
	    (let loop ()
					;(printf "STREAM LOOP: out:~s  evts:~s\n" output-queue event-queue)
	      (if (null? output-queue)
		  ;; Run the query some more.
		  (if global-eng
		      (begin (turn-crank! time-slice)
			     (loop))
		      ;; Otherwise, all done:
		      '())
		  (let-values ([(x rest) (rac&rdc! output-queue)])
		    (set! output-queue rest)
		    (cons x (delay (loop)))
		    )))))
	;; Otherwise, it's not a stream, just return the value!
	prog)
    )

  ;; Run the engine for a bit.
  (define (turn-crank! ticks)
    (global-eng ticks
	 (lambda (val ticks) (set! global-eng #f))
	 (lambda (neweng) 
	   ;(printf "   Queued ~s outputs\n" (s:length output-queue))
	   (set! global-eng neweng)
	   )))

  ;; ================================================================================
  ;; Stream processing primitives:

  ;; Stream source:
  (define (timer annot freq) 
    ;; milliseconds:
    (define timestep (rate->timestep freq))
    (define our-sinks '())
    (define src (let ([t 0]
                      [n 0])
		  (lambda (msg)
		    (s:case msg
		      ;; Returns the next time we run.
		      [(peek) t]
		      [(pop)
		       ;; Release one stream element.
		       (set! t (s:+ t timestep))
             (fire! unit-representation our-sinks)
		       ]))))
    ;; Register ourselves globally as a leaf node:
    (set! data-sources (cons src data-sources))
    (lambda (sink)
      ;; Register the sink to receive this output:
      (set! our-sinks (cons sink our-sinks))))

  (define (timer-bench annot output-type box-name edge-counts-table sum-type-declarations freq num-tuples)
    ;; milliseconds:
    (define timestep (rate->timestep freq))
    (define our-sinks '())
    (define bench-rec (make-bench-stats 0 0 0))
    (define src (let ([t 0]
                      [n 0])
        (lambda (msg)
          (s:case msg
            ;; Returns the next time we run.
            [(peek) t]
            [(pop)
             (if (= n num-tuples) (stop-WS-sim! (format "reached ~a tuples!" num-tuples)))
             ;; Release one stream element.
             (set! t (s:+ t timestep))
             (set! n (+ n 1))
             (profiled-fire! unit-representation our-sinks bench-rec output-type sum-type-declarations)
             ]))))
    ;; Register ourselves globally as a leaf node:
    (set! data-sources (cons src data-sources))
    (hashtab-set! edge-counts-table box-name bench-rec)
    (lambda (sink)
      ;; Register the sink to receive this output:
      (set! our-sinks (cons sink our-sinks))))

  ;;(define smap stream-map)
  ;;(define sfilter stream-filter)
  
  (define (iterate annotations fun src)
    (define our-sinks '())
    (define wsbox
      (lambda (msg)
        (let ([outputs (reverse! (unbox (fun msg (virtqueue))))])
          ;(inspect outputs)
          (for-each (lambda (elem)
                      (fire! elem our-sinks))
            outputs))))
    (lambda (sink)
      ;; Register ourselves with our source:
      ;(printf "Registering: ~s ~s \n" annotations sink)
      (src wsbox) ;; <- rrn: Changing this to happen lazily.
      (set! our-sinks (cons sink our-sinks))))

  (define (iterate-bench annotations output-type box-name edge-counts-table sum-type-declarations fun src)
    (define our-sinks '())
    (define bench-rec (make-bench-stats 0 0 0))
    (define wsbox
      (lambda (msg)
        (let ([outputs '()]
              [stats-pre1 '()]
              [stats-pre2 '()]
              [stats-post '()])
          
          ;; run the box and update CPU time
	  ;; rrn: should do this in a neater way, but for now just ripping this out for PLT:
	  (IFCHEZ
	   (begin           
	     (set! stats-pre1 (statistics))
	     (set! stats-pre2 (statistics))
	     (set! outputs (reverse! (unbox (fun msg (virtqueue)))))
	     (set! stats-post (statistics))
	     (bench-stats-cpu-time-add! bench-rec (- (sstats-cpu stats-post) (sstats-cpu stats-pre2)
						     (- (sstats-cpu stats-pre2) (sstats-cpu stats-pre1)))))
	   (void))

          ;; fire!
          (for-each (lambda (elem) (profiled-fire! elem our-sinks bench-rec output-type sum-type-declarations))
            outputs))))
    (hashtab-set! edge-counts-table box-name bench-rec)
    (lambda (sink)
      ;(printf "Registering: ~s ~s \n" annotations sink)
      ;; Register ourselves with our source:
      (src wsbox) ;; <- rrn: Changing this to happen lazily.
      (set! our-sinks (cons sink our-sinks))))



  (define (feedbackloop src fun)
    ;; Let-n-set to build a cyclic structure.
    (define our-sinks '())
    (define (joiner msg) (fire! msg our-sinks))
    (define (registrar sink)
      (set! our-sinks (cons sink our-sinks)))
    ;; Pass this stream representation to the function:
    (define resultstrm (fun registrar))

    ;; Register ourselves with our source:
    (src joiner)
    ;; Connect the result stream back to the joiner.
    (resultstrm joiner)
    ;; Return the final result
    resultstrm)

  ;; This is the functional version of iterate.
  ;; Untested
  (define (integrate fun zero src)
    (define our-sinks '())
    (define state zero)
    (define wsbox
      (lambda (msg)
	(let ([tup (reverse! (unbox (fun msg state)))])
	  (set! state (tupref 1 2 tup))
          (let ([v (tupref 0 2 tup)])
            (fire! v our-sinks)))))
    ;; Register ourselves with our source:
    (src wsbox)
    (lambda (sink)
      (set! our-sinks (cons sink our-sinks))))

     ;; Very simple queue:
     ;; TODO: Could copy sigsegs on output here and see what the impact is.
     (define (emit vq x) (set-box! vq (cons x (unbox vq))))
     ;;(emission (cons v (emission))))
     (define (virtqueue) (box '()))
     (define (return x) x)

     (define (prim_window src winsize)
       (let ([start 0]
	     [samp  0]
	     [i     0]
	     [vec (make-vector winsize)]
	     [our-sinks '()])
	 (define wsbox
	   (lambda (tup)
	     (vector-set! vec i tup)
	     (set! samp (add1 samp))
	     (set! i (fx+ 1 i))	     	     
	     (when (= i winsize)
	       (let ([result (make-sigseg start (sub1 samp) vec nulltimebase)])
		 (set! start samp)
		 (set! i 0)
		 (set! vec (make-vector winsize))
		 (fire! result our-sinks)))))
	 ;; Register ourselves with our source:
	 (src wsbox)
	 (lambda (sink)
	   (set! our-sinks (cons sink our-sinks)))))

     ;; This current version will run the function multiple times for overlapping areas.
#;
     (define (deep-iterate f s)
       (stream-map 
;	(let ([start 0] ;; Record the range covered thusfar.
;	      [end 0])
	  (lambda (w)
	    (make-sigseg (sigseg-start w)
			 (sigseg-end w)
			 ;; The function for deep-iterate had better be one-to-one.  I.e. it's really a map!
			 (vector-map (lambda (x) 
				       (let ([ls (unbox (f x (virtqueue)))])
					 (unless (and (not (null? ls)) (null? (cdr ls)))
					   ;; Wish I could give source location:
					   (error 'deep-iterate 
						  "for now deep-iterate only allows one-to-one functions to be applied, got result: ~a"
						  ls))
					 (car ls)))
				     (sigseg-vec w))
			 (sigseg-timebase w)))))

  ;; We read in "blocks" to reduce the overhead of all those thunks!
  ;; (Actually, this didn't speed things up much, just a little.)
  (define DATAFILE_BATCH_SIZE 500)
  
  ;; Should have batched data file...
  ; FIXME: last arg. is a hack
  ; FIXME: need to implement a real output-type; it's broken right now, using types
  (define (__readFile annot file srcstrm mode repeat skipbytes offset winsize types . bench)
    ;; TODO: implement skipbytes and winsize!!!

    ;; chez scheme doesn't have define-values -- d'oh
    (define bench? (not (null? bench)))
    (define output-type           (if bench? (car bench)    #f))
    (define box-name              (if bench? (cadr bench)   #f))
    (define edge-counts-table     (if bench? (caddr bench)  #f))
    (define sum-type-declarations (if bench? (cadddr bench) #f))


    ;; This implements the text-mode reader.
    ;; This is not a fast implementation.  Uses read.
    ;; Interestingly, it's currently [2006.12.03] running better in opt-level 2 than 3!
    ;; This is one playback of the file:
    (define (textsource)
      ;; TODO: In debug mode this should check the types of what it gets.       
      (define len (List:length types))
      (define inp (open-input-file file))
      (define tyvec (list->vector types))
      (define (parse-line str)
        (define p (open-input-string str))
        (let ([ls (list-build len 
                              (lambda (i)
                                ;; Note, this doesn't work for spaces, and doesn't expect quotes around strings.
                                (s:case (vector-ref tyvec i)
                                        [(String) (symbol->string (read p))]
                                        [(Int Int16) (let ([v (read p)])
                                                       (unless (ws-int? v)
                                                         (error 'readFile "cannot read ~s as integer type" v))
                                                       v)]
                                        [(Float)    (let ([v (read p)])
                                                      (unless (ws-float? v)
                                                        (error 'readFile "cannot read ~s as float type" v))
                                                      v)]
                                        [else (read p)])
                                ))])
          (if (fx= 1 (length ls))
              (car ls)
              (make-tuple ls))))
      (define our-sinks '())  
      (define bench-rec (make-bench-stats 0 0 0))
      (define iswindowed (> winsize 0))
      (define pos 0)
      ;; Reads a whole bunch of lines.
      (define (read-window n)
        (let loop ([x (read-line inp)] [batch (sub1 n)] [acc '()])
	       (if (or (not x) (eof-object? x))
              (reverse! acc)
              (if (fxzero? batch)
                  (reverse! (cons (parse-line x) acc))
                  (loop (read-line inp) (fx- batch 1) (cons (parse-line x) acc))))))
      (define wsbox 
        (lambda (msg)
          (if iswindowed
              (let ([win (list->vector (read-window winsize))])
                (if (not (= (vector-length win) winsize))
                    ;; How do we signal end of file?
                    (stop-WS-sim! "readFile: didn't get enough data on window read")
                    (let* ([newpos (+ winsize pos -1)]			
                           [result (make-sigseg pos newpos win nulltimebase)])
                      
                      (set! pos (+ 1 newpos))
                      (if bench?
                          (profiled-fire! result our-sinks bench-rec output-type sum-type-declarations)
                          (fire! result our-sinks)))))
              
              ;; Inefficient:
              (let ([win (read-window 1)])
                (if (null? win) 
                    (stop-WS-sim! "readFile: out of data")
                    (if bench?
                        (profiled-fire! (car win) our-sinks bench-rec output-type sum-type-declarations)
                        (fire! (car win) our-sinks)))))))

      ;; Register ourselves with the parent operator:
      (srcstrm wsbox)
      (if bench? (hashtab-set! edge-counts-table box-name bench-rec))
      (lambda (sink)
        ;; Register the sink to receive this output:
        (set! our-sinks (cons sink our-sinks))))


    ;; Read a binary stream with a particular tuple format.
    (define (binsource)
      (define source (read-binary-file-stream file srcstrm
                                              (apply + (map type->width types)) ;; Read N bytes at a time.
                                              (types->reader types)
                                              (if (> winsize 0) winsize 1) ;; Length of "window"				
                                              0 ;; Overlap
                                              skipbytes
                                              offset
                                              (if (> winsize 0) bench? #f)
                                              output-type
                                              box-name
                                              edge-counts-table
                                              sum-type-declarations))
      ;; winsize 0 or -1 indicates non windowed stream, thus strip that sigseg:
      ;; This is inefficient because we allocate a one-element sigseg then discard it:
      (if (<= winsize 0)
          (if bench?
              (iterate-bench annot output-type box-name edge-counts-table sum-type-declarations
                             (lambda (x vq) (emit vq (seg_get x 0)) vq) source)
              (iterate annot (lambda (x vq) (emit vq (seg_get x 0)) vq) source))
          source))

    (define thestream
      (cond 
       [(s:equal? mode "text") (textsource)]
       [(s:equal? mode "binary") (binsource)]
       [else (error 'dataFile "this mode is not supported yet: ~s" mode)]))
    ;; This records the stream the first time through then keeps repeating it.
    (define (repeat-stream repeats)
      (let ([whole-stream #f] [buf '()] [first-run? #t])
        (define (loop)
          (cond
           [first-run? 
            (let ([x (thestream)])
              (if (eq? x stream-empty-token)
                  (begin 
                    (set! buf (reverse! buf))
                    (set! whole-stream buf)
                    (set! first-run? #f)
                    (set! thestream #f)
                    (loop))
                  (begin (set! buf (cons x buf))
                         x)))]
           [(null? buf)
            (if (= 0 repeats)
                stream-empty-token
                (begin (set! buf whole-stream)
                       (set! repeats (fx- repeats 1))
                       (loop)))]
           [else
            (let ([x (car buf)])
              (set! buf (cdr buf))
              x)]))
        loop))

    (printf "  Opening stream datafile ~s\n" file)

    ;; __readFile body:
    (s:case repeat
            [(0) thestream]
            [else (error 'datafile "no repeats yet")]
                                        ;[(-1) (repeat-stream -1)]                                  
                                        ;[else (ASSERT (> repeat 0)) (repeat-stream repeat)]
            )

    ) ; End __readFile
  
  (define (ensBoxAudioAll . args)
    (error 'ensBoxAudioAll "can't run inside scheme emulator!"))
  (define (ensBoxAudio . args)
    (error 'ensBoxAudioAll "can't run inside scheme emulator!"))
  (define (ensBoxAudioF . args)
    (error 'ensBoxAudioAll "can't run inside scheme emulator!"))

  ;; Internal helper.  Returns a Stream, which is a registrar for Sinks.
  (define (read-binary-file-stream file srcstrm wordsize sample-extractor len overlap skipbytes offset
                                   bench? output-type box-name edge-counts-table sum-type-declarations)
    (define chunksize 32768) ;; How much to read from file at a time.
    (define infile (open-input-file file))
    (define buffer1 (make-string chunksize #\_))
    (define count1 0)
    (define ind1 0)
    (define winsize (* len (+ wordsize skipbytes)))
    (define remainder #f) ;; The unprocessed left-over from a batch.

    (define counter 0)
    (define total 0)
    (define print-every 500000)

    ;; This version is simpler than my previous attempt and just
    ;; reads at the granularity of the window-size.  However it has
    ;; a CORRECTNESS problem.  It depends on block-read getting all
    ;; the data every read.  
    ;; TODO: FIX THIS.
    ;;
    ;; This version takes 750 ms (opt lvl 3) with no alloc or fill.
    ;; 1.6 s with alloc, 2.0 s with alloc & constant fill.
    ;; And 22 seconds with alloc, fill, and sample parsing!
    ;; Tried forcing read-sample/to-int16 to inline, but that bumped it to 35s!
    ;; (Oops.  Got it back down to 24.8s by linearizing the pattern var usages.)
    ;;   Inlining only to-int16 makes it 26s... not helping...       
    ;;
    ;; NOTE: Performance got vastly better when I only did one
    ;; channel at a time instead of every sample being a 4-vector.
    (define (read-window)
      (define (return-it string)
        (let ([win (make-vector len)])
          (let readloop ([i 0] [pos 0])
            ;;(printf "READING UNTIL ~s word ~s skip ~s\n" winsize wordsize skipbytes)
            (unless (= i len)
              (vector-set! win i (sample-extractor string pos))
              (readloop (fx+ i 1) (fx+ pos wordsize skipbytes))
              ))  
          win))
      (set! count1 (block-read infile buffer1 winsize))
      (cond
       [(eof-object? count1)  #f]
                                        ;[(fx> count1 winsize) (error 'read-window "got too much at once, should never happen.")]
       [(fx< count1  winsize)
        ;; If we got an incomplete window we just keep reading:
        ;; Generally speaking this only happens in PLT.  My block-read is working very poorly.
        (let loop ([count count1]
                   [acc (list (substring buffer1 0 count1))])
                                        ;(printf "read-window: retrying read to get whole window (~s only got ~s).\n" winsize count)
          (let ([newcount (block-read infile buffer1 (fx- winsize count))])
            (if (eof-object? newcount) 
                (begin (warning 'read-window 
                                "discarding incomplete window of data, probably end of stream.  Got ~a, wanted ~a" 
                                count1 winsize) 
                       #f)
                (let ([total (+ count newcount)]
                      [newacc (cons (substring buffer1 0 newcount) acc)])
                  (if (fx= total winsize)
                      (return-it (apply string-append (reverse! newacc)))
                      (loop total newacc))))))] 
       [else (return-it buffer1)]))

    (define _ 
      ;; This returns the stream representing the audio channel (read in from disk):
      ;; TODO: HANDLE OVERLAP:
      (unless (zero? overlap)
        (error 'read-binary-file-stream "currently does not support overlaps, use rewindow")))

    (define our-sinks '())
    (define bench-rec (if bench? (make-bench-stats 0 0 0) #f))
    (define pos 0)
    (define (wsbox msg)
      (let ([win (read-window)])
        (if win
            (let* ([newpos (+ len pos -1)]
                   [result (make-sigseg pos newpos win nulltimebase)])
              
              (unless (<= (regiment-verbosity) 0)
                (set! counter (fx+ counter len))
                (when (fx>= counter print-every)
                  (set! counter (fx- counter print-every))
                  (set! total (+ total print-every))
                  (fprintf (current-error-port) "Read ~a tuples from file ~a.\n"
                           (+ total counter)
                           file)))
              
              (set! pos (+ 1 newpos))

              (if bench?
                  (profiled-fire! result our-sinks bench-rec output-type sum-type-declarations)
                  (fire! result our-sinks)))
            (begin 		    
              #;
              (error 'read-binary-file-stream
                     "don't know how to handle eof right now.")
              (stop-WS-sim! "readFile: hit eof")
              (void)))))
    
    ;; Scan ahead in the file to the offset:
       (let scan ([offset offset])
	 ;; Would be nice if we had a seek command instead of having
	 ;; to read this out by blocks:
	 (unless (zero? offset)
	   ;; Don't read more than we have room for.
	   (scan (fx- offset (block-read infile buffer1 (min offset chunksize))))))

       ;; Register with our parent stream.
       (srcstrm wsbox)
       (if bench? (hashtab-set! edge-counts-table box-name bench-rec))
       (lambda (sink)
	 ;; Register the sink to receive this output:
	 (set! our-sinks (cons sink our-sinks))))
     

     ;; This is just for testing.  IT LEAKS.
     (define (unionList ls)
       (define our-sinks '())
       ;; Register a receiver for each source:       
       (for-eachi (lambda (i src)
                      (src (lambda (x)
                             (fire! (tuple i x) our-sinks))))
                  ls)
       (lambda (sink)
         (set! our-sinks (cons sink our-sinks))))

     (define (unionList-bench output-type box-name edge-counts-table sum-type-declarations ls)
       (define our-sinks '())
       (define bench-rec (make-bench-stats 0 0 0))
       ;; Register a receiver for each source:
       (for-eachi (lambda (i src)
                    (src (lambda (x)
                           (let ((datum (tuple i x)))
                             (profiled-fire! x our-sinks bench-rec output-type sum-type-declarations)))))
                  ls)
       (hashtab-set! edge-counts-table box-name bench-rec)
       (lambda (sink)
         (set! our-sinks (cons sink our-sinks))))


     (define (unionN annotations . args) (unionList args))

     (define (unionN-bench annotations output-type box-name edge-counts-table sum-type-declarations . args)
       (unionList-bench output-type box-name edge-counts-table sum-type-declarations args))
     
     (define (_merge annotations s1 s2)
       (define our-sinks '())
       (define (wsbox x) (fire! x our-sinks))
       ;; Register a receiver for each source:       
       (lambda (sink) 
	 (s1 wsbox)  (s2 wsbox) ;; rrn: doing this lazily now
	 (set! our-sinks (cons sink our-sinks))))

     ; NOTE: doesn't matter for us, but not re-entrant
     (define (_merge-bench annotations output-type box-name edge-counts-table sum-type-declarations s1 s2)
       (define our-sinks '())
       (define bench-rec (make-bench-stats 0 0 0))
       ;; Register a receiver for each source:
       (define wsbox
         (lambda (x)
           (profiled-fire! x our-sinks bench-rec output-type sum-type-declarations)))
       (hashtab-set! edge-counts-table box-name bench-rec)
       (lambda (sink) 
	 (s1 wsbox)  (s2 wsbox) ;; rrn: doing this lazily now
	 (set! our-sinks (cons sink our-sinks))))



  ;; ================================================================================
  (define (valid-sigseg? w)
    (or (nullseg? w)
	(and (sigseg? w)
	     (<= (sigseg-start w) (sigseg-end  w))
	     (s:equal? (vector-length (sigseg-vec w))
		     (s:+  (s:- (sigseg-end w) (sigseg-start  w)) 1))
	     )))

  (define (valid-timebase? tb)
    ;; This is the only implemented timebase right now ;)
    ;(eq? tb nulltimebase)
    (timebase? tb)
    )
     
  (define-syntax app
    (syntax-rules ()
      [(_ f args ...) (f args ...)]))
  (define-syntax foreign-app
    (syntax-rules ()
      [(_ realname f args ...) (f args ...)]))

  (define-syntax construct-data
    (syntax-rules ()
      [(_ variant) (make-uniontype 'variant (void))]
      [(_ variant arg) (make-uniontype 'variant arg)]))

  ;; For these programs, need letrec*.
#;
  (define-syntax ws-letrec
    (syntax-rules ()
      ;; We assume type info has already been stripped.
      [(_ x ...) (letrec* x ...)]))

  ;(define-for-syntax for-loop-stack (reg:make-parameter '()))
  (define for-loop-stack (reg:make-parameter '()))
  
  (define-syntax for
       (syntax-rules ()
	 [(_ (i st en) bod ...)
	  (call/1cc (lambda (escape)
                      (parameterize ([for-loop-stack (cons escape (for-loop-stack))])
		       (let ([endpoint en])
			 (let loop ([i st])
			   (unless (> i endpoint)
			     (let ()
			       bod ...
			       (loop (add1 i)))))))))]))

  (define-syntax while
    (syntax-rules ()
      [(_ tst bod) (let while-loop () (if tst (begin bod (while-loop))))]))


  (define-syntax wscase
    (syntax-rules ()
      [(_ x [TC* fun*] ...)
       (let* ([ls `((TC* ,fun*) ...)]
	      [entry (assq (uniontype-tag x) ls)])		 
	 ;(printf "WSCASE OF: ~s ~s\n" x ls)
	 (if entry
	     ;; A void value indicates that it really carries no value.
	     (if (eqv? (uniontype-val x) (void))
		 ((cadr entry))
		 ((cadr entry) (uniontype-val x)))
	     ;(begin (inspect x)(inspect entry))
	     (let ([entry2 (assq default-case-symbol ls)])
	       (if entry2 
		   (cadr entry2) ;((cadr entry2))
		   (error 'wscase "unmatched case in case construct (with no default): ~s" (uniontype-tag x))))
	     ))]))

  ;; We just call the continuation, the fluid-let worries about popping the stack.
  (define (break) ((car (for-loop-stack)) (void)))



  ;; ================================================================================
  ;; Reading binary data:

  ;; Read two bytes from a string and build a uint16.
  (define (to-uint16 str ind)  ;; Internal helper function.
    (fx+ (fxsll (char->integer (string-ref str (fx+ 1 ind))) 8)
	        (char->integer (string-ref str (fx+ 0 ind)))
	 ))
  ;; The signed version
  (define (to-int16 str ind) 
    (let ([unsigned (to-uint16 str ind)])
      (if (fxlogbit? 15 unsigned)	  
	  ;(fx- (fxlogbit0 15 unsigned))
	  (fx- unsigned (expt 2 16))
	  unsigned)))
  
  ;; Might not be a fixnum:
  (define (to-uint32 str ind)  ;; Internal helper function.
    (s:+ (s:* (char->integer (string-ref str (fx+ 3 ind))) (* 256 256 256))
       (fxsll (char->integer (string-ref str (fx+ 2 ind))) 16)
       (fxsll (char->integer (string-ref str (fx+ 1 ind))) 8)
              (char->integer (string-ref str (fx+ 0 ind)))))
  (define (to-int32 str ind) 
    (let ([unsigned (to-uint32 str ind)])
      (if (logbit? 31 unsigned)	 
	  (logbit1 31 unsigned)
	  unsigned)))

  (define (types->reader types)
     (define (type->reader t)
       (match t
	 [Int16 to-int16]
	 [Int   to-int32]
	 ;; HACK, the reader is the same:
	 [(Sigseg #(,t* ...)) (types->reader t*)]
	 [(Sigseg ,t)         (type->reader t)]
	;[Float ]
	;[Complex ]
	 [,other (error 'type->reader "can't support binary reading of type ~s yet." other)]))
     (define readers (list->vector (map type->reader types)))
     (define widths (list->vector (map type->width types)))
     (define len (s:length types))
     (cond 
      [(= 0 len) (error 'types->reader "can't read unit type (zero-length tuple) from a file!")]
      [(= 1 len) (type->reader (car types))]
      [else (lambda (str ind)
	      (apply tuple
	       (let unmarshallloop ([i 0] [pos ind])
		 (if (= i len)
		     '()
		     (cons ((vector-ref readers i) str pos)
			   (unmarshallloop (fx+ 1 i) (fx+ pos (vector-ref widths i))))))))]))

  ;; Currently unused.
  (define (uint16->string n)
    (if (>= n 65536)
	(error 'uint16->string "input is too large: ~s" n))
    (let* ([lowbyte (fxmodulo n 256)]
	   [highbyte (fx/ (fx- n lowbyte) 256)])
      (list->string (list (integer->char highbyte)
			  (integer->char lowbyte)))))



  ;; ================================================================================

;      (define nullseg (gensym "nullseg"))
;      (define Array:null (gensym "Array:null"))
;      (define nulltimebase (gensym "nulltimebase"))
  ;(define nullseg 'nullseg)
  (define Array:null #())
  (define nulltimebase (make-timebase 12345654321))
  (define nullseg (make-sigseg 0 -1 #() nulltimebase))
  ;(define nullseg special-nullseg-object)
  ;(define (nullseg? x) (eq? x nullseg))
  (define (nullseg? x) (fx= 0 (vector-length (sigseg-vec x))))

  (define (gint x) x)

  (define g+ s:+) (define g- s:-) (define g* s:*) 
  ;; Special behavior for division.
  ;; This should only be invoked with ws.early.
  (define (g/ a b)
    ;; We use the physical representation of the number to determine what its type is.
    (cond
;     [(exact? a) (quotient a b)]
     [(fixnum? a) (fxquotient a b)]
;     [(flonum)]
     ;; Floats and complex just fall through to the normal scheme division.
     [else (s:/ a b)]
     ))

  (define ws+ fx+)   (define ws- fx-)   (define ws* fx*)   (define ws/ fx/)

  ;; Ok, ints are 32 bit so fx+ can't be used from now on:
  (define _+_ s:+)    (define _-_ s:-)    (define *_ s:*)  (define (/_ x y) (floor (s:/ x y)))

  ;(define _+_ fx+)    (define _-_  fx-)    (define *_ fx*)    (define /_ fx/)
  (define _+I16 fx+)  (define _-I16 fx-)  (define *I16 fx*)  (define /I16 fx/)
  (define _+U16 fx+)  (define _-U16 fx-)  (define *U16 fx*)  (define /U16 fx/)
  (define _+I32 s:+)  (define _-I32 s:-)  (define *I32 s:*)  (define (/I32 a b) (floor (s:/ a b)))
  (define _+I64 s:+)  (define _-I64 s:-)  (define *I64 s:*)  (define (/I64 a b) (floor (s:/ a b)))

  (define _+. fl+)    (define _-. fl-)    (define *. fl*)    (define /. fl/)
  (define _+D fl+)    (define _-D fl-)    (define *D fl*)    (define /D fl/)
  (define _+: cfl+)   (define _-: cfl-)   (define *: cfl*)   (define /: cfl/)

  ;; [2008.02.22] New strategy: cast_num does nothing, whereas
  ;; assert-type ensures conformance with our numeric representation
  ;; policy.
  ;(define (cast_num x) (error 'cast_num "should not be called at metaprogram eval"))
  (define (cast_num x) x)
  (define-syntax assert-type
    ;; TODO Rewrite this with syntax-case and remove code duplication.
    (syntax-rules (Int Int16 Int32 Int64 Uint16 
		       Float Double Complex)      
      [(_ Int   e)   (__cast_num #f 'Int   e)]
      [(_ Int16 e)   (__cast_num #f 'Int16 e)]
      [(_ Int32 e)   (__cast_num #f 'Int32 e)]
      [(_ Int64 e)   (__cast_num #f 'Int64 e)]
      [(_ Uint16 e)  (__cast_num #f 'Uint16 e)]
      [(_ Float e)   (__cast_num #f 'Float e)]
      [(_ Double e)  (__cast_num #f 'Double e)]
      [(_ Complex e) (__cast_num #f 'Complex e)]
      [(_ other e) e]
      ))

  ;; Currently we "overflow" by just changing the number to zero.
  ;; That is, overflow behavior is not well defined across platforms.
  (define (__cast_num from to num) 
    (case to
      [(Int Uint16 Int16 Int64)
       (let ([x (inexact->exact (floor num))]
	     [pred (match to
		     [Int16 int16?]
		     [Uint16 uint16?]
		     ;[Int   int32?]  ;; Ints are not defined as 32 bits
		     [Int   fixnum?]
		     [Int32 int32?]
		     [Int64 int64?])])
	 (if pred x 0))]
      [(Float Double) (exact->inexact num)]
      [(Complex) (+ num 0.0+0.0i)]
      [else (error '__cast_num "cast to unhandled numeric type: ~s" to)]))


  (define ws^ expt)
  (define g^ expt)
  (define ^_ expt)
  (define ^I16 expt)
  (define ^U16 expt)
  (define ^I32 expt)
  (define ^I64 expt)
  (define ^D expt)
  (define ^. expt)
  (define ^: expt)

  (define (sqrtI n) (flonum->fixnum (sqrt n)))
  (define sqrtF sqrt)
  (define sqrtC sqrt)
  (define moduloI fxmodulo)
     
     ;; These shouldn't be implemented because they should be desugared earlier!
     ; (define (toFloat n)
;        (cond
; 	[(fixnum? n) (fixnum->flonum n)]
; 	[(ws-float? n) n]
; 	[else (error 'toFloat "may only be used for upcast, given: ~s" n)]))
;      (define (toComplex n) (s:+ n 0.0+0.0i))

  (define realpart cfl-real-part)
  (define imagpart cfl-imag-part)
  
  (define absI16 fxabs)
  (define absI64 s:abs)
  (define absI fxabs)
  (define absF flabs)
  (define absD flabs)
  (define absC (IFCHEZ s:abs 
		       (lambda (c) 
			 (let ([x (cfl-real-part c)] 
			       [y (cfl-imag-part c)])					   
			   (sqrt (+ (* x x) (* y y)))))))

  (define randomI s:random)

  (define (logF n) ;(real-part (log n))
    (if (< n 0) (wserror "logF: cannot accept negative numbers")
	(log n)))
  (define (logD n) 
    (if (< n 0) (wserror "logD: cannot accept negative numbers")
	(log n)))

  (define exptI expt)
  (define exptF expt)
  (define exptD expt)

  ;(define (makeComplex re im) (s:fl-make-rectangular re im))
  (define (makeComplex re im) (s:make-rectangular re im))

  (define int16ToInt    (lambda (x) x))
  (define int16ToInt64  (lambda (x) x))
  (define int16ToFloat   fixnum->flonum)
  (define int16ToDouble   fixnum->flonum)
  (define (int16ToComplex n) (s:+ n 0.0+0.0i))

  (define int64ToInt16  (lambda (x) (ASSERT int16? x) x))
  (define int64ToInt    (lambda (x) (ASSERT int32? x) x))
  (define int64ToFloat    exact->inexact)
  (define int64ToDouble   exact->inexact)
  (define (int64ToComplex n) (s:+ n 0.0+0.0i))

  (define intToInt16 (lambda (x) (ASSERT int16? x) x))
  (define intToInt64 (lambda (x) x))
  (define intToFloat fixnum->flonum)
  (define intToDouble fixnum->flonum)
  (define intToComplex int16ToComplex)

  ;; Should do a range check here:
  (define floatToInt16 flonum->fixnum)
  (define floatToInt64 (lambda (x) (exact->inexact (floor x))))
  (define floatToInt   flonum->fixnum)
  (define (floatToDouble x) x)
  (define (floatToComplex f) (s:make-rectangular f 0.0))

  (define doubleToInt16 floatToInt16)
  (define doubleToInt64 floatToInt64)
  (define doubleToInt    floatToInt)
  (define (doubleToFloat x) x)
  (define doubleToComplex floatToComplex)

  (define (complexToInt c) (flonum->fixnum (realpart c)))
  (define complexToInt16 complexToInt)
  (define complexToInt64 (lambda (x) (exact->inexact (floor (realpart x)))))
  (define complexToDouble realpart)
  (define complexToFloat realpart)

  ;; TODO: MERGE THIS WITH DUPLICATED CODE IN STATIC-ELABORATE!!

  (define stringToInt (lambda (v) 
		 (let ([x (string->number v)])
		   (if x 
		       (ASSERT ws-int? x)
		       (error 'stringToInt "couldn't convert string: ~s" v)))))
  (define stringToFloat (lambda (v) 
		   (let ([x (string->number v)])
		     (if x 
			 (ASSERT ws-float? x)
			 (error 'stringToFloat "couldn't convert string: ~s" v)))))
  (define stringToDouble stringToFloat)
  (define stringToComplex (lambda (v) 
		   (ASSERT string? v)
		   (let ([x (string->number v)])
		     (cond
		      [(not x) (error 'stringToComplex "couldn't convert string: ~s" v)]
		      [(ws-float? x) (s:fl-make-rectangular x 0.0)]
		      [else (ASSERT ws-complex? x)]))))

  (define (ArrayStringWrapper fn) (lambda (x) (fn (list->string (vector->list x)))))
  (define __stringToInt_ARRAY     (ArrayStringWrapper stringToInt))
  (define __stringToFloat_ARRAY   (ArrayStringWrapper stringToFloat))
  (define __stringToDouble_ARRAY  (ArrayStringWrapper stringToDouble))
  (define __stringToComplex_ARRAY (ArrayStringWrapper stringToComplex))

  (define String:length string-length)
  (define String:explode string->list)
  (define String:implode list->string)
  (define intToChar integer->char)
  (define charToInt char->integer)
  
  (define (roundF f) ((IFCHEZ flround round) f))

  ;; [2006.08.23] Lifting ffts over sigsegs: 
  ;; Would be nice to use copy-struct for a functional update.
  (define (fftR2C arr)
    (DEBUGASSERT (curry vector-andmap ws-float?) arr)
    (DEBUGMODE (if (s:equal? arr #()) (error 'fft "cannot take fft of Array:null"))
	       (let ([log2 (lambda (n) (s:/ (log n) (log 2)))])
		 (if (or (= 0 (vector-length arr))
			 (not (integer? (log2 (vector-length arr)))))
		     (error 'fft "only window sizes that are powers of two are supported: length ~s" 
			    (vector-length arr)))))
    (let* ([double (dft arr)]
	   [fulllen (vector-length double)]
	   [halflen (add1 (quotient fulllen 2))]
	   [half (make-vector halflen)])
      ;(vector-blit! double half 0 0 halflen)
      (vector-set! half 0 (vector-ref double 0))
      (let loop ([i 1])
	(unless (fx= i halflen)
	  ;; Fill from the front:
	  (vector-set! half i (vector-ref double i))
	  ;; Fill from the back:
	  ;(vector-set! half i (vector-ref double (fx- fulllen i)))
	  (loop (fx+ 1 i))))
      
      ;; Currently the output must be all cflonums.
      (DEBUGASSERT (curry vector-andmap ws-complex?) half)
      half))

  ;; This is only relevent to fftw:
  (define memoized_fftR2C fftR2C)

  ;; As long as we stick with the power of two constraint, the output
  ;; of this should be the same size as the original (i.e. we can
  ;; round-trip without changing length).
  (define (ifftC2R vec)
    (DEBUGASSERT (curry vector-andmap ws-complex?) vec)
    (let* ([len (vector-length vec)]
	   [len2 (fx* 2 (fx- len 1))]
	   [double (make-vector len2 0)])

      ;; Fill in half of the spacious one:
      (vector-blit! vec double 0 0 len)
      ;; Now fill in the other half, backwards:
      (do ([i 1 (fx+ i 1)]) ((= i (fx- len 1)) (void))
	(vector-set! double (fx- len2 i) (cfl-conjugate (vector-ref vec (fx- i 0)))))
      
      ;; Now run the ifft, and convert the numbers to floats:
      (let* ([result (inverse-dft double)])
	;; Just eyeballing... some of these looked suspiciously
	;; non-floatlike (nontrivial imaginary components).  Is
	;; something messed up?
	(vector-map! cfl-real-part result)
	result
	)))

  (define (fftC arr)
    (DEBUGASSERT (curry vector-andmap ws-complex?) arr)
    (DEBUGMODE (if (s:equal? arr #()) (error 'fftC "cannot take fft of Array:null"))
	       (let ([log2 (lambda (n) (s:/ (log n) (log 2)))])
		 (if (or (= 0 (vector-length arr))
			 (not (integer? (log2 (vector-length arr)))))
		     (error 'fft "only window sizes that are powers of two are supported: length ~s" 
			    (vector-length arr)))))
    (dft arr))

  (define (ifftC arr)
    (DEBUGASSERT (curry vector-andmap ws-complex?) arr)
    (inverse-dft arr))

  (define (wserror str) ((wserror-handler) str))
  (define (__wserror_ARRAY vec) (wserror (list->string (vector->list vec))))

  (IFCHEZ (define inspect inspect/continue)
             ;; Don't know of an interactive object inspector in PLT:
	  (define (inspect x) x))      

     (define (tuple . args) (make-tuple args))
     (define (tupref ind _ tup) (list-ref (tuple-fields tup) ind))

     (define-alias List:length s:length)
     (define-alias List:is_null s:null?)
     (define-alias List:ref list-ref)

     (define-alias List:make make-list)
     (define-alias List:head car)
     (define-alias List:tail cdr)

     (define-alias head car)
     (define-alias tail cdr)

     (define-alias List:reverse reverse)
     (define-alias List:append append)
     (define-alias List:map map)

     (define-alias List:toArray list->vector)

     (define (List:zip a b) (map (lambda args (make-tuple args)) a b))

     ;; These should really be defined in the language.  They aren't
     ;; currently [2006.10.26] because the elaborator isn't ready to
     ;; *not* inline their recursive definitions.
     (define (List:fold f zero ls)
       (let loop ([acc zero] [ls ls])
	 (if (null? ls) acc
	     (loop (f acc (car ls)) (cdr ls)))))
     (define (List:assoc ls x)
       (let loop ([ls ls])
	 (cond
	  [(null? ls) '()]
	  [(s:equal? (tupref 0 2 (car ls)) x) ls]
	  [else (loop (cdr ls))])))
     (define (List:assoc_update origls x y)
       (let loop ([ls origls] [acc '()])
	 (cond
	  [(null? ls) (cons (vector x y) origls)]
	  [(s:equal? (tupref 0 2 (car ls)) x)
	   (append (reverse! acc) (cons (tuple x y) (cdr ls)))]
	  [else (loop (cdr ls) (cons (car ls) acc))])))

     (define Array:make make-vector)
     (define (Array:makeUNSAFE len) (make-vector len 'uninitialized!))
     (define Array:ref  vector-ref)
     (define Array:set vector-set!)
     (define Array:length   vector-length)
     (define Array:map vector-map)
     (define Array:fold vector-fold)
     (define Array:toList vector->list)
     (define Array:andmap (lambda (f v) (andmap f (vector->list v))))
     (define Array:build vector-build)

     (define List:build list-build)

     (define internString string->symbol)
     (define uninternString symbol->string)

     ;; EQ? based hash tables:
#;
     (begin
       ;; If we cared we could use some kind of balanced tree for functional maps.
       (define (copy-hash-table ht)
	 ;; This is terrible, we don't know how big it is.
	 (let ([newtab (make-hash-table 100)])
	   (hash-table-for-each ht
	    (lambda (k v) (put-hash-table! newtab k v)))
	   newtab))
       (define HashTable:make #%make-hash-table)
       (define (HashTable:contains ht k) (#%get-hash-table ht k #f))
       (define (HashTable:get ht k) (#%get-hash-table ht k #f))
       ;; Pretty useless nondestructive version:
       (define (HashTable:set ht k v)
	 (define new (copy-hash-table ht))
	 (put-hash-table! new k v)
	 new)
       (define (HashTable:set_BANG ht k v)
	 (#%put-hash-table! ht k v)
	 ht)
       ;(define HashTable:rem )
       ;(define HashTable:rem_BANG )

       (define HashTable:foreach #%hashtab-for-each)
       )

     ;; EQUAL? based hash tables:
     (begin
       (define HashTable:set_BANG (slib:hash-associator s:equal?))
       
       (define (copy-hash-table ht)
	 ;; This is terrible, we don't know how big it is.
	 (let ([newtab (slib:make-hash-table (vector-length ht))])
	   (slib:hash-for-each ht
	    (lambda (k v) (HashTable:set_BANG newtab k v)))
	   newtab))

       (define HashTable:make slib:make-hash-table)
       (define HashTable:contains 
	 (let ([getfun (slib:hash-inquirer s:equal?)])
	   (lambda (ht k) (if (getfun ht k) #t #f))))       
       (define HashTable:get 
	 (let ([getfun (slib:hash-inquirer s:equal?)])
	   (lambda (ht k)
	     (let ([result (getfun ht k)])
	       (unless result
		 (error 'HashTable:get "couldn't find key: ~s" k))
	       result
	       ))))
       ;; Pretty useless nondestructive version:
       ;; If we cared we could use some kind of balanced tree for functional maps.
       (define (HashTable:set ht k v)
	 (define new (copy-hash-table ht))
	 (HashTable:set_BANG new k v)
	 new)

       (define HashTable:rem_BANG (slib:hash-remover s:equal?))
       (define (HashTable:rem ht k) 
	 (define new (copy-hash-table ht))
	 (HashTable:rem_BANG new ht k)
	 new)
       
       (define HashTable:foreach slib:hash-for-each)
       )

     (define show ws-show)
     ;; Null terminate:
     (define (__show_ARRAY x) 
       (let* ((str (ws-show x))
	      [arr (make-vector (fx+ 1 (string-length str)))])
	 (vector-set! arr (fx- (vector-length arr) 1) (integer->char 0))
	 (let loop ([i 0])
	  (when (< i (fx- (vector-length arr) 1))
	    (vector-set! arr i (string-ref str i))
	    (loop (fx+ 1 i))))
	 arr))
     (define (__backtoSTR arr)
       (list->string (rdc (vector->list arr))))


     ;; Inefficient, show should be defined in terms of print, not vice-versa
     (define (ws-print x)
       (parameterize ([current-output-port (ws-print-output-port)])
	 (cond
	  [(string? x) (display (ws-show x))]
	  [(and (vector? x) (> (vector-length x) 0) (char? (vector-ref x 0)))
	   (vector-for-each display x)]
	  [else (display-constrained (list (ws-show x) 300))])
	 (IFDEBUG (flush-output-port) (void))
	 ))

     (define (gnuplot_array arr)   (gnuplot (vector->list arr)))
     (define (gnuplot_array2d arr) (gnuplot (map vector->list (vector->list arr))))

     (define gnuplot-helper
       (lambda (extract)
	 (lambda (src) 
	   (define our-sinks '())
	   (define plotter (gnuplot_pipe))
	   (define wsbox
	     (lambda (msg)
	       (plotter (extract msg))
	       (fire! msg our-sinks)))
	   ;; Register ourselves with our source:
	   (src wsbox)
	   (lambda (sink) (set! our-sinks (cons sink our-sinks))))))

     (define gnuplot_array_stream  (gnuplot-helper (lambda (arr) (vector->list arr))))
     (define gnuplot_sigseg_stream (gnuplot-helper (lambda (ss) (vector->list (sigseg-vec ss)))))

     (define gnuplot_array_stream2d  (gnuplot-helper (lambda (arr) (vector->list arr))))
     (define gnuplot_sigseg_stream2d (gnuplot-helper (lambda (ss) (vector->list (sigseg-vec ss)))))

     (define gnuplot_process 
      (IFCHEZ     
       (lambda (ctrl datastrm)
          
       (define (filtnewline s) (list->string (remq-all #\newline (string->list s))))
       (define gnuplotoutput
	 (format "/tmp/_gnuplot_process_~s.out"
		 (filtnewline (system-to-str "date +%s"))
		 #;
		 (+ (real-time) (random 100000))))
       (define fn2 (format "/tmp/_temp_gnuplot.dat.~a.pipe" 
			   (filtnewline (system-to-str "date +%s"))
			   #;
			   (+ (real-time) (random 100000))))
       ;; FIXME: FINISH THIS:
       (define (mungestring str)
	 (if (memq #\~ (string->list str))
	     (format str fn2)
	     str))
       
       (printf "Making fifo...\n")
       (system (format "mkfifo ~s" fn2))
       (printf "Fifo made opening it from one end.\n")
       (let ([datapipe #f]) ;; Have to mutate this... blocks on open.
	 (printf "Opening process...\n")
	 (let-match ([(,inp ,outp ,pid) (process (format "gnuplot - &> ~a" gnuplotoutput))])
	  
	   #;
	   (define (try-output)
	     (let loop ()
	       (when (char-ready? inp)
		 (display (read-char inp))
		 (loop))))

	   (eprintf "gnuplot process running...\n")
	   	   
	   ;; So how about flushing?
	   (ctrl     (lambda (str) 
;		       (try-output)
		       (printf "Got ctrl strm message: ~s\n" str)
		       (display (mungestring str) outp) (flush-output-port outp)))
	   (datastrm (lambda (str) 
;		       (try-output)
		       (unless datapipe (set! datapipe (open-output-file fn2 'append)))
		       (printf "Got data strm message: ~s\n" str)
		       (display str datapipe) (flush-output-port outp)))))
       (lambda (sink) (void)))
       (lambda _ (error 'gnuplot_process "not implemented in PLT"))))
      
     ;; This only produces output
     (define spawnprocess
       (IFCHEZ
       (lambda (command instrm)          
       (define our-sinks ())
	 (let-match ([(,inp ,outp ,pid) (process command)])	   
	   (define (try-output)
	     (let loop ()
	       (if (char-ready? inp)
		   (cons (read-char inp) (loop))
		   ())))
	   (eprintf "Spawnprocess: Spawned ~s\n" command)
	   	   
	   ;; So how about flushing?
	   (instrm  (lambda (str) 
;		      (printf "Got message for child process: ~s\n" str)
		      (display str outp) (flush-output-port outp)
		      (let ([processoutput (try-output)])
			(unless (null? processoutput)
			  (fire! (list->string processoutput) our-sinks)))
;		      (fire! "Sent stuff to subprocess...\n" our-sinks)
		      )))
	 (lambda (sink) 
	   (set! our-sinks (cons sink our-sinks))))
       (lambda _ (error 'spawnprocess "not implemented in PLT"))
       ))

     (define m_invert ws-invert-matrix)

     ;;================================================================================

     ;; This is a bit silly.  Since we don't use an actual time-series
     ;; implementation, this just makes sure the overlap is EQUAL.
     (define (joinsegs w1 w2)
       (DEBUGASSERT (valid-sigseg? w1))
       (DEBUGASSERT (valid-sigseg? w2))
       (DEBUGASSERT valid-sigseg?
	(cond 
	 [(nullseg? w1) w2]
	 [(nullseg? w2) w1]
	 [else 
	  (let ([a (sigseg-start w1)]
		[b (sigseg-end w1)]
		[x (sigseg-start w2)]
		[y (sigseg-end w2)])
	   (cond
	    [(not (eq? (timebase-num (sigseg-timebase w1)) 
		       (timebase-num (sigseg-timebase w2))))
	     (error 'joinsegs "Cannot handle different TimeBases!")]

	    ;; In this case the head of w2 is lodged in w1:
	    ;; OR they line up precisely.
	    [(and (<= a x) (<= x (+ b 1)))
	     (DEBUGASSERT (sigseg? w1))
	     (DEBUGASSERT (sigseg? w2))
	     ;(printf "JOINING: ~a:~a and ~a:~a\n" (sigseg-start w1) (sigseg-end w1) (sigseg-start w2) (sigseg-end w2))
	     	     
	     (let ([new (make-vector (add1 (s:- (max b y) a)))])
	       (for (i a (max b y))
		 (define (first) (vector-ref (sigseg-vec w1) (s:- i a)))
		 (define (second) (vector-ref (sigseg-vec w2) (s:- i x)))
					;		  (printf "i ~a\n" i)
		 (vector-set! new (s:- i a)
			      (cond
			       ;; Still in the first window:
			       [(< i x) (first)]
			       ;; We're in the overlap:
			       [(and (<= i b) (<= x i) (<= i y))
				(if (eqv? (first) (second))
				    (first)
				    (error 'joinsegs "overlapping segs had different values: ~s vs. ~s at seq# ~a"
					   (first) (second) i))]
			       ;; We're past w2, but still in w1.
			       [(and (< y i) (<= i b)) (first)]
			       ;; Past the first window, therefore still in w2:
			       [(> i b) (second)]
			       [else (error 'joinsegs "hmm... broken")])))
					;`(Success ,(make-sigseg a (max b y) new (sigseg-timebase w1)))
	       (make-sigseg a (max b y) new (sigseg-timebase w1)))]

	    ;; In this case there is a gap!
	    [(< b (sub1 x)) 
	     ;'(Gap)
	     (error 'joinsegs "there's a gap between these sigsegs: sample range ~a:~a and ~a:~a\n"
		    (sigseg-start w1) (sigseg-end w1)
		    (sigseg-start w2) (sigseg-end w2))]
	    
	    [else (error 'joinsegs "bug in code, this error should not happen.")]
	    ))])))

     ;; start must be a *sample number* (inclusive), len is the length of the returned seg
     (define (subseg w startind len)
       ;(inspect `(subseg ,w ,startind ,len))
       (DEBUGASSERT (valid-sigseg? w))
       (ASSERT valid-sigseg?
	(cond
	 [(nullseg? w) (error 'subseg "cannot subseg nullseg: ind:~s len:~s" startind len)]
	 [(< len 0) (error 'subseg "length of subseg cannot be negative!: ~s" len)]
	 [(= len 0) nullseg]
	 [(or (< startind (sigseg-start w))
		(> (s:+ startind len -1) (sigseg-end w)))
	   (error 'subseg "cannot take subseg ~a:~a from sigseg ~s" startind (s:+ startind len -1) w)]
	 [else 
	  (let ([vec (make-vector len)])
	    (for (i 0 (fx- len 1))
		(vector-set! vec i 
			     (vector-ref (sigseg-vec w) 
					 (fx+ i (s:- startind (sigseg-start w))))))
	    (make-sigseg startind (s:+ startind len -1) vec (sigseg-timebase w)))])))


     ;; [2007.01.26] Changing this back to be zero-based.
     (define (seg_get w ind) 
       (DEBUGASSERT valid-sigseg? w)
       (if (nullseg? w) (error 'seg_get "cannot get element from nullseg!"))
       (DEBUGMODE (if (or (< ind 0) (>= ind (width w)))
		      (error 'seg_get "index ~a is out of bounds for sigseg:\n~s" ind w)))
       (vector-ref (sigseg-vec w) ind))
     (define (width w) 
       (DEBUGASSERT valid-sigseg? w)
       (if (nullseg? w) 0 (vector-length (sigseg-vec w))))
     (define (start w) 
       (DEBUGASSERT (valid-sigseg? w))
       (if (nullseg? w) (error 'start "cannot get start index from nullseg!"))
       (sigseg-start w))
     (define (end w) 
       (DEBUGASSERT (valid-sigseg? w))
       (if (nullseg? w) (error 'end "cannot get end index from nullseg!"))
       (sigseg-end w))
     (define (timebase w) 
       (DEBUGASSERT (valid-sigseg? w))
       ;; Is this true?  Or does each signal have its own nullseg?? That could be very tricky...
       ;; Well, the main thing we need nullseg for, as I see it, is initializing accumulators.
       (if (nullseg? w) (error 'end "cannot get timebase from nullseg!"))
       (sigseg-timebase w))
     
     (define (Secret:newTimebase x) (make-timebase x))

     ;; [2007.10.30] UNSAFE: This doesn't copy!  It should.  This is
     ;; the safe backend afterall, not the high performance one.
     (define (toArray w) (if (nullseg? w) #() (if (fx= 3 (ws-optimization-level)) (sigseg-vec w) (vector-copy (sigseg-vec w)))))
     (define (toSigseg ar st tb)
       (define en (s:+ st (s:vector-length ar) -1))
       (DEBUGASSERT (or (eq? ar Array:null) (vector? ar)))
       (DEBUGASSERT integer? st)
       (DEBUGASSERT integer? en)
       (DEBUGASSERT valid-timebase? tb)
       (if (not (= (vector-length ar) (s:+ en (s:- st) 1)))
	   (error 'toSigseg "vector's size did not match start/end tags: ~s:~s ~s" st en ar))
       (let ([result (make-sigseg st en ar tb)])
	 (DEBUGASSERT valid-sigseg? result)
	 result))
       

     ;; Export these, they override the default scheme bindings.
     ;; ----------------------------------------
     (IFCHEZ (begin (define + ws+)
		    (define - ws-) 
		    (define * ws*) 
		    (define / ws/)
		    (define ^ ws^) 
		    ;(define-syntax letrec (identifier-syntax ws-letrec))
;		    (define-id-syntax letrec  ws-letrec)
		    ;(define-id-syntax print ws-print)
		    (define print ws-print)
		    )
	     (provide (rename ws+ +) (rename ws- -) (rename ws* *) (rename ws/ /) (rename ws^ ^)
;		      (rename ws-letrec letrec)
		      (rename ws-print print)                      
                      for ;for-loop-stack
                      ))

(define already-loaded-object-files (box ())) ;; When do we reset this?

(define (__foreign_source . _)
  (printf "CALLING FOREIGN_SOURCE\n")
  (lambda (sink) 
    (printf "Hmm... registering foreign-source with sink: ~s\n" sink)
    (error 'foreign_source "Foreign stream sources are not, and will not be, implemented for the scheme backend.")))
(define (inline_C . _)
  (lambda (sink)
    (error 'inline_C "Inline C code is not not, and will not be, implemented for the scheme backend.")))
(define (inline_TOS . _)
  (lambda (sink)   
    (error 'inline_TOS "Inline C code is not not, and will not be, implemented for the scheme backend.")))



;; This provides access to C-functions:
(IFCHEZ

 (begin 

   ;; [2007.08.30] Feature change for the WS foreign interface.  Now
   ;; the user may assume access to all of libc by default.
  (define ensure-libc-loaded!
    (let ([ranyet? #f])
      (lambda ()
	(unless ranyet?
	  (load-shared-object
	   (case (machine-type)
	     [(ti3osx i3osx ppcosx) "libc.dylib"]
	     [(ti3le i3le) "libc.so.6"]
	     [else (error 'ensure-libc-loaded! 
			  "WaveScript foreign interface not supported on platform: ~s"
			  (machine-type))]
	     ))
	  ;; FIXME: there are other places in the code where we might want to know about this:
	  ;; Let's just put it in an environment variable.
	  (putenv "REGLIBCLOADED" "TRUE")
	  (set! ranyet? #t)))))
  ;; NOTE: This isn't working on 64-bit justice.
  (define __foreign
    (let ()

      (define (Convert T)
	(match T
	  [Int     'fixnum]
	  [Float   'single-float]
	  [Double  'double-float]
	  [Boolean 'boolean]
	  [Char    'char]
	  [String  'string]
	  [(Pointer ,_) 'uptr]
					;	[(ExclusivePointer ,_) 'uptr]
	  [#()     'void]
					;[(Char) char]
	  [,else (error '__foreign:Convert "this type is not supported by the Chez foreign interface: ~s" T)]))

      (define (DynamicLink out files)
	(when (file-exists? out) (delete-file out))
	(let* ([files (apply string-append (map (curry format " ~s") files))]
	       [code (s:case (machine-type)
			     [(i3le ti3le)  		      
					;(printf "EXEC: ~a\n" (format "cc -fPIC -shared -o \"~a.so\" ~a" out files))
			      (system (format "cc -fPIC -shared -o \"~a.so\" ~a" out files))]
			     [(i3osx ti3osx) (system (format "cc -fPIC -dynamiclib -o \"~a.so\" ~a" out files))]
			     [else (error 'foreign "don't know how to compile files ~s on machine type ~s: ~s\n" files (machine-type))]
			     )])
	  ;; This is actually not guaranteed to work by the chez spec:
	  (unless (zero? code)
	    (error 'foreign "C compiler returned error code ~s." code))
	  ;; Returns the name of the output file:
	  ;; Should use a dylib extension for mac os X:
	  (string-append out ".so")
	  ))
      
      (define (LoadFile! file)
	(let ([ext (extract-file-extension file)]
	      [sharedobject file])
	  (cond
	   [(or (string=? ext "so") 
		(string=? ext "dylib")
		(substring? ".so." file) ;; This is a hack to handle files like libc.so.6
		) (void)]
	   ;; This is a bit sketchy... because of course the user *COULD* put function definitions in headers.
	   ;; The assumption for now is that headers can be ignored.
	   [(member ext '("h" "hpp")) (set! sharedobject #f)]
	   
	   [(s:equal? ext "o")
	    (printf "  Attempting to convert object (.o) to shared object (.so:) ~s\n" file)
	    (let ([target (remove-file-extension file)])	      
	      (set! sharedobject (DynamicLink target (list file))))]

	   [(s:equal? ext "a")
	    (printf "  Attempting to convert static library (.a) to shared .so: ~s\n" file)
	    (let ([target  (remove-file-extension file)]
		  [tempfile (format ".__tempfile_~a.txt" (random 1000000))])
	      ;; This assumes bash!!
	      (system-to-str (format "ar xv \"~a\" | awk '{ print $3 }' > ~a " file tempfile))
	      (let ([objfiles (filter (lambda (s) (not (s:equal? s "")))
				(file->lines tempfile))])
		;; Now relink the .o files into a shared object:
		(set! sharedobject (DynamicLink target objfiles))
		))]

	   [(member ext '("c" "cpp"))
	    ;; This is really stretching it.  Attempt to compile the file.
	    (let ([target  (remove-file-extension file)])	      
	      (printf "  Attempting to compile ~s to ~s.so\n" file target)
	      (set! sharedobject (DynamicLink target (list file))))]
	   [else (error 'foreign "this type of foreign file not supported in scheme backend: ~s" file)])

	  ;; Load the file containing the C code.
	  (when (and sharedobject (not (member sharedobject (unbox already-loaded-object-files))))
	    (load-shared-object sharedobject)
	    (set-box! already-loaded-object-files (cons sharedobject (unbox already-loaded-object-files)))
	    (printf "  Shared object file (~a) loaded.\n" sharedobject))
	  ))
      (lambda (name files type)

	;; First make sure that the C standard library is loaded.
	(ensure-libc-loaded!)

	(printf "Dynamically loading foreign entry ~s from files ~s.\n" name files)
	(for-each LoadFile! files)
	;; After it's loaded there'd better be access:
	(unless (foreign-entry? name)
	  (error 'foreign "failure to register foreign function in Scheme: ~s" name))
	(match type
	  [(,[Convert -> args] ... -> ,ret)
	   (let ([foreignfun (eval `(foreign-procedure ,name ,args ,(Convert ret)))])
	     foreignfun
	     #;	   
	     (if (match? ret (ExclusivePointer ,_))
		 (lambda args
		   (let ([ret (apply foreignfun args)])
		     ;; Now we add the pointer we get back to our guardian:
		     (fprintf (current-error-port) " !!ADDING TO GUARDIAN!! ~s\n" ret)
		     ((foreign-guardian) (box ret))
		     ret
		     ))
		 foreignfun))
	   ])))))
 (define __foreign (lambda _ (error 'foreign "C procedures not accessible from PLT"))))

;; This tries to match the binary format used in the C backend.
#;
(define (__marshal val ty)
  (let ([port (open-string-port)])
    (match ty
      [(Int) ]
      [#()])
    ))

;; A foreign procedure for freeing foreign storage.
;; We do late-binding here:
(define C-free 
  (IFCHEZ 
   (lambda (ptr)
     (fprintf (current-error-port) "C-free: Loading free function from system's libc.\n")
     (load-shared-object 
      (case (machine-type)
	[(i3osx ppcosx) "libc.dylib"]
	[(i3le )        "libc.so.6"]
	[else (error 'C-free "don't know what shared object to load this from on machine type: ~s" 
		     (machine-type))]))
     ;; This throws an error in Petite if not wrapped in an eval:
     (set! C-free (eval '(foreign-procedure "free" (uptr) void)))
     (C-free ptr))
   (lambda _ (error 'C-free "not implemented in PLT"))))

;    (exclusivePtr   (Pointer) ExclusivePointer)
(define (exclusivePtr p)
  (let ([x (box p)])
;    (fprintf (current-error-port) " !!ADDING TO GUARDIAN!! ~s\n" x)
    ((foreign-guardian) x)
    x))
(define getPtr unbox)

(define (ptrIsNull p) (= p 0))
(define (ptrMakeNull) 0)
(define nullPtr 0)

;; TODO ptrToArray
#;
(define ptrToArray
  )


;    (getPtr         (ExclusivePointer) Pointer)


;; Can convert static to dynamic library:
;  #! /usr/bin/ksh -p
;  # Makes a shared library from a static one
;  #
;  static_library=$1; shared_library=$2
;  /usr/bin/ld -O3 -x -no_excpt -expect_unresolved '*' -rpath /freeware/gcc/alpha/lib -shared -o ${shared_library:-${static_library%%a}so} -all $1

;; ====================================================================================================== ;;
;;                                        FOR WS.EARLY ONLY:                                              ;;
;; ====================================================================================================== ;;

;; Hack for ws.early.  Just a little wrapper to call the real thing.
(define (readFile-wsearly annot fn str src type)
  (match (parse-readFile-modestring annot str type fn src)
    [(__readFile ,annot ,fn ,src ',mode ',repeats ',skipbytes ',offset ',winsize ',types)
     (__readFile annot fn src mode repeats skipbytes offset winsize types)]))

(define FILE_EXISTS file-exists?)
(define (GETENV str) (or (getenv str) ""))
(define SHELL system-to-str)

(define (SETCPU i s) s)
(define (SETCPUDEEP i s) s)

(define (clock) (exact->inexact (cpu-time)))
(define (realtime) (/ current-vtime 1000))

(define (getID) 0) ;; If we're running in Scheme, it's always node 0.

;(define-syntax IFPROFILE (syntax-rules () [(_ a b) b]))
;(define (IFPROFILE a b) b)
(define (IFPROFILE a b) a)

;; [2007.08.16] TEMP: reads a stream of data as we wrote it out of our marmot appilication.
#;
(define (HACK_O_RAMA filename)
  ;; Must be in milleseconds:
  ;; Convert from 48khz to microseconds:
  (define (sample->time n) (s:* n 1000000. 1/48000))
  (define our-sinks '())
  (define port (open-input-file filename))

  (define firsttime #f)
  (define nextid #f)
  (define nexttime #f)
  (define nextdat #f)
  (define (read-one!)
    (define nodeid (read port))
    (define metadat (read port))
    (define payload (read port))
    (if (or (eof-object? nodeid) (eof-object? metadat) (eof-object? payload))
	(stop-WS-sim! "readFile: hit eof")
	(begin 
	  (ASSERT integer? nodeid)
	  (set! nextid nodeid)
	  
	  (match metadat
	    [((,st ,end) ,_ ,__ ,___)       
	     (define ms (sample->time st))
	     (ASSERT integer? st) (ASSERT integer? end)
	     (unless firsttime (set! firsttime ms))
	     (set! nexttime (s:- ms firsttime))
	     (ASSERT (>= nexttime 0))
	     (match payload
	       [(,v1 ,v2 ,v3 ,v4)
		(ASSERT (and (vector? v1) (vector? v2) (vector? v3) (vector? v4)))
		(ASSERT (vector-andmap int16? v1))
		(ASSERT (vector-andmap int16? v2))
		(ASSERT (vector-andmap int16? v3))
		(ASSERT (vector-andmap int16? v4))
		(set! nextdat 
		      (make-tuple
		       (list nextid
			     (list
			      (make-sigseg nexttime (sub1 end) v1 3)
			      (make-sigseg nexttime (sub1 end) v2 3)
			      (make-sigseg nexttime (sub1 end) v3 3)
			      (make-sigseg nexttime (sub1 end) v4 3)
			      ))))])]))
	))
  (define src (begin
		(read-one!)
		(lambda (msg)
		  (s:case msg
		    ;; Returns the next time we run.
		    [(peek) nexttime]
		    [(pop) 
		     (fire! nextdat our-sinks)		     
		     ;; Release one stream element.
		     (read-one!)]))))
  ;; Register ourselves globally as a leaf node:
  (set! data-sources (cons src data-sources))
  (lambda (sink)
    ;; Register the sink to receive this output:
    (set! our-sinks (cons sink our-sinks))))


) ; End module.


