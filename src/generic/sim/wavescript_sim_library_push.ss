
;;;; TODO: Replace output-queue with real queue!! 

(module wavescript_sim_library_push mzscheme
  (require 
           "../constants.ss"
	   "../../plt/iu-match.ss"
           "../util/fft.ss"
           "../langs/lang_wavescript.ss"
           "../../plt/engine.ss"
	   "../../../depends/matpak.ss"
           (prefix slib: "../util/slib_hashtab.ss")
	   (all-except "../util/imperative_streams.ss" test-this )
	   (all-except "../util/helpers.ss" test-this these-tests for inspect break)	   
	   (all-except "../compiler_components/regiment_helpers.ss" test-this these-tests for inspect break)           
	   )
  (provide
                 make-sigseg sigseg-start sigseg-end sigseg-vec sigseg-timebase
		 valid-sigseg?
		 app let 

		 run-stream-query reset-state!

		 __dataFile ;__syncN

		 ;dump-binfile 
		 ;audio 
		 audioFile timer 
		 show 
		 gnuplot_array gnuplot_array_stream gnuplot_sigseg_stream
		 prim_window

		 to-uint16 to-int16 uint16->string

		 gint
		 g+ g- g* g/ g^
		 +_ -_ *_ /_ ^_
		 +. -. *. /. ^.
		 +: -: *: /: ^:
		 +I16 -I16 *I16 /I16 ^I16
		 sqrtF sqrtC sqrtI
		 absI absF absC absI16
		 roundF		 

		 intToFloat floatToInt
		 floatToComplex complexToFloat
		 intToComplex complexToInt

		 intToInt16
		 int16ToInt int16ToFloat int16ToComplex

		 stringToInt stringToFloat stringToComplex

		 ;toComplex toFloat  ;; Generic versions
		 ;toInt ;; Truncate
		 ;roundToInt

		 realpart imagpart 

		 nullseg nullarr nulltimebase
		 tuple tupref
		 makeArray arr-get arr-set! 
		 hashtable hashcontains hashget hashset hashset_BANG hashrem hashrem_BANG

		 List:ref List:append List:reverse List:map List:fold List:length List:make 
		 List:head List:tail
		 List:assoc List:assoc_update

		 joinsegs subseg seg-get width start end timebase
		 toArray toSigseg 

		 assert-type
		 
		 wserror inspect
		 emit return
		 ;smap sfilter
		 iterate break ;deep-iterate
		 ;; TODO: nix unionList.
		 unionN unionList 
		 ;zip2
		 ; union2 union3 union4 union5
		 fft 
		 
		 ;; Misc, ad-hoc, and Temporary
		 m_invert ;; A matrix inversion.
		 )
    (chezprovide (for for-loop-stack)
;		 letrec 
		 length print
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
		 (only lang_wavescript 
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

       (define orig-length #%length)
       (import (add-prefix scheme s:))
       )
     (begin (define orig-length length)
	    (require (prefix s: mzscheme))
	    (require (prefix s: "../../plt/chez_compat.ss"))))


  ;; ================================================================================
  ;; TYPES USED 

  ;; Sources :: 'peek -> time | 'pop -> Elem
  ;; Sink    ::  Elem -> ()
  ;; Stream  ::  Sink -> ()

  ;; Streams take sinks and register them.
  ;; Sources allow either "peeking" the time of their next element, or
  ;; popping off thatelement.

  ;; Contains a start and end SEQUENCE NUMBER as well as a vector.
  (reg:define-struct (sigseg start end vec timebase))

  ;(reg:define-struct (wsbox outports))
  ;(reg:define-struct (wsevent time source))
  ;; This structure represents a simulation.
  ;(reg:define-struct (wssim current-vtime data-sources))


  ;; ================================================================================    

  (define end-token (gensym "wavescript_stream_END"))

  ;; Global queue of input events in virtual time.
  ;; Currently pairs of (time . source)
  (define event-queue   'wslp-uninit1)    
  (define current-vtime 'wslp-uninit2)
  (define data-sources  'wslp-uninit3)   ;; A list of all data sources in the query graph
  (define output-queue  'wslp-uninit4)   ;; Outputs from the query graph
  (define global-eng    'wslp-uninit5)   ;; Engine for running stream graph. 
  
  ;; Reset the global state.
  (define (reset-state!)
    (set! event-queue '())
    (set! current-vtime 0)
    (set! data-sources '())
    (set! output-queue '())
    )

  (define output-sink (lambda (x) (set! output-queue (cons x output-queue))))

  ;; Launch a stream element to all sinks.
  (define-syntax fire!
    (syntax-rules ()
      ((_ elem sinks) (for-each (lambda (f) (f elem)) sinks))))

  ;; Converts hertz to microseconds:
  (define (rate->timestep freq)
    (when (zero? freq) (error 'rate->timestep "sampling rate of zero is not permitted"))
    (flonum->fixnum (s:* 1000000 (s:/ 1.0 freq))))
  
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
		     (if (car newentry)
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

  (define (timer freq) 
    ;; milliseconds:
    (define timestep (rate->timestep freq))
    (define our-sinks '())
    (define src (let ([t 0])
		  (lambda (msg)
		    (case msg
		      ;; Returns the next time we run.
		      [(peek) t]
		      [(pop) 
		       ;; Release one stream element.
		       (set! t (s:+ t timestep))
                       (fire! #() our-sinks)
		       ]))))
    ;; Register ourselves globally as a leaf node:
    (set! data-sources (cons src data-sources))
    (lambda (sink)
      ;; Register the sink to receive this output:
      (set! our-sinks (cons sink our-sinks))))

  ;;(define smap stream-map)
  ;;(define sfilter stream-filter)
  
  (define (iterate fun src)
    (define our-sinks '())
    (define wsbox
      (lambda (msg)
	(let ([outputs (reverse! (unbox (fun msg (virtqueue))))])
	  ;(inspect outputs)
	  (for-each (lambda (elem)
                      (fire! elem our-sinks))
	    outputs))))
    ;; Register ourselves with our source:
    (src wsbox)
    (lambda (sink)
      (set! our-sinks (cons sink our-sinks))))

  ;; This is the functional version of iterate.
  ;; Untested
  (define (integrate fun zero src)
    (define our-sinks '())
    (define state zero)
    (define wsbox
      (lambda (msg)
	(let ([vec (reverse! (unbox (fun msg state)))])
	  (set! state (vector-ref vec 1))
          (let ([v (vector-ref vec 0)]) 
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

  
  ;; Read a stream of Uint16's.
  (define (audioFile file len overlap rate)
    (read-binary-file-stream file 
		      2 ;; Read just 2 bytes at a time.
		      to-uint16
		      len overlap rate))

  ;; We read in "blocks" to reduce the overhead of all those thunks!
  ;; (Actually, this didn't speed things up much, just a little.)
  (define DATAFILE_BATCH_SIZE 500)

  ;; Should have batched data file...
  (define (__dataFile file mode rate repeat types)

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
	(define tup (make-vector len))
	(let loop ([i 0])
	  (if (fx= i len)
	      tup
	      (begin 
		;; Note, this doesn't work for spaces, and doesn't expect quotes around strings.
		(vector-set! tup i 
			     (case (vector-ref tyvec i)
			       [(String) (symbol->string (read p))]
			       [else (read p)]))
		(loop (fx+ 1 i))))))
      (define (get-batch)
	(let loop ([x (read-line inp)] [batch DATAFILE_BATCH_SIZE] [acc '()])
	  (if (or (not x) (fxzero? batch))
	      (reverse! acc)
	      (loop (read-line inp) (fx- batch 1) (cons (parse-line x) acc)))))

       (define our-sinks '())  
       (define timestep (rate->timestep rate))
       (define t 0)
       (define pos 0)
       (define (src msg)
	 (case msg
	   [(peek) t]
	   [(pop) 
	    (set! t (s:+ t timestep))
	    (let ([batch (get-batch)])
	      (if (null? batch)
		  ;(engine-return end-token)		  
		  (set! t #f)
		  (for-each (lambda (elem) 					
                              (fire! elem our-sinks))
		      batch)))
	    ]))
       ;; Register data source globally:
       (set! data-sources (cons src data-sources))
       (lambda (sink)
	 ;; Register the sink to receive this output:
	 (set! our-sinks (cons sink our-sinks))))
    
    ;; Read a binary stream with a particular tuple format.
    (define (binsource)
      (iterate 
       ;; Strip that sigseg:
       (lambda (x vq) (emit vq (seg-get x 0)) vq)
       (read-binary-file-stream file 
				(types->width types) ;; Read N bytes at a time.
				(types->reader types)
				1 ;; Length of "window"
				0 ;; Overlap
				rate)))
    (define thestream
      (cond 
       [(equal? mode "text") (textsource)]
       [(equal? mode "binary") (binsource)]
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
		       (set! repeats (- repeats 1))
		       (loop)))]
	   [else
	    (let ([x (car buf)])
	      (set! buf (cdr buf))
	      x)]))
	loop))

    (printf "Reading stream datafile ~s\n" file)

    ;; __dataFile body:
    (case repeat
      [(0) thestream]
      [else (error 'datafile "no repeats yet")]
      ;[(-1) (repeat-stream -1)]                                  
      ;[else (ASSERT (> repeat 0)) (repeat-stream repeat)]
      )

    ) ; End __dataFile

#;
  ;; This is a hack to load specific audio files:
  ;; It simulates the four channels of marmot data.
  (define (audio chan len overlap rate)
    (define (read-sample str index)
	 (let ([s str] [ind index])
	   ;; Just the requested channel:

	   (fixnum->flonum ;; For now this returns float.
	    (to-int16 s (fx+ ind (fx* chan 2))))
	   ;; All 4 channels:
	   ;; NIXING: This allocation of little vectors is really painful performance wise.

#;	   (vector (to-int16 s ind)
		   (to-int16 s (fx+ 2 ind))
		   (to-int16 s (fx+ 4 ind))
		   (to-int16 s (fx+ 6 ind)))
	   ))
       (read-binary-file-stream (default-marmotfile) 8 read-sample len overlap rate))

  ;; Internal helper.  Returns a Stream, which is a registrar for Sinks.
  (define (read-binary-file-stream file wordsize sample-extractor len overlap rate)
    (define chunksize 32768) ;; How much to read from file at a time.
    (define infile (open-input-file file))
    (define buffer1 (make-string chunksize #\_))
    (define count1 0)
    (define ind1 0)
    (define winsize (* wordsize len))
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
	 (set! count1 (block-read infile buffer1 winsize))
	 (cond
	  [(eof-object? count1)     #f]
	  ;[(fx> count1 winsize) (error 'read-window "got too much at once, should never happen.")]
	  [(fx< count1  winsize)
	     (warning 'read-window 
		      "discarding incomplete window of data, probably end of stream.  Got ~a, wanted ~a" 
		      count1 winsize)
	     ;(warning 'read-window "this version depends on block-read always 
	     ; getting all the chars, got ~a, wanted ~a"	count1 winsize)
	     ;(printf "Better get eof next... THE PROBLEM OF LEFTOVERS!\n")
	     (ASSERT (eof-object? (block-read infile buffer1 winsize)))
	     #f]
	  [else
	   (let ([win (make-vector len)])
	     (for (i 0 (fx- len 1))
		  (vector-set! win i (sample-extractor buffer1 (fx* i wordsize)))
		  (void)
		  )
	     win)]))

       (define _ 
	 ;; This returns the stream representing the audio channel (read in from disk):
	 ;; TODO: HANDLE OVERLAP:
	 (unless (zero? overlap)
	   (error 'read-binary-file-stream "currently does not support overlaps, use rewindow")))

       (define our-sinks '())  
       (define timestep (rate->timestep rate))
       (define t 0)
       (define pos 0)
       (define (src msg)
	 (case msg
	   [(peek) t]
	   [(pop) 
	    (set! t (s:+ t timestep))
	    
	    (let ([win (read-window)])
	      (if win
		  (let* ([newpos (+ len pos -1)]
			 [result (make-sigseg pos newpos win nulltimebase)])
		    
		    (unless (regiment-quiet)
		      (set! counter (fx+ counter len))
		      (when (fx>= counter print-every)
			(set! counter (fx- counter print-every))
			(set! total (+ total print-every))
			(printf "Read ~a tuples from file ~a.\n"
				(+ total counter)
				file)))

		    (set! pos (+ 1 newpos))
                    (fire! result our-sinks)
		    )
		  (begin 		    
#;
		    (error 'read-binary-file-stream
			   "don't know how to handle eof right now.")
		    (set! t #f))))
	    ]))

       ;; Register data source globally:
       (set! data-sources (cons src data-sources))
       (lambda (sink)
	 ;; Register the sink to receive this output:
	 (set! our-sinks (cons sink our-sinks))))
     

     ;; This is just for testing.  IT LEAKS.
     (define (unionList ls)
       (define our-sinks '())
       ;; Register a receiver for each source:       
       (for-eachi (lambda (i src)
                      (src (lambda (x)
                             (fire! (vector i x) our-sinks))))
                  ls)
       (lambda (sink)
         (set! our-sinks (cons sink our-sinks))))

     (define (unionN . args)  (unionList args))

#;
     (define (zip2 s1 s2)
       (delay 	 
	 (let loop ([s1 s1] [s2 s2])
	   (cond
	    [(stream-empty? s1) '()]
	    [(stream-empty? s2) '()]
	    [else (stream-cons (vector (stream-car s1) (stream-car s2))
			       (loop (stream-cdr s1) (stream-cdr s2))
			       )]
	    ))))














  ;; ================================================================================
  (define (valid-sigseg? w)
    (or (eq? w nullseg)
	(and (sigseg? w)
	     (<= (sigseg-start w) (sigseg-end  w))
	     (equal? (vector-length (sigseg-vec w))
		     (+  (- (sigseg-end w) (sigseg-start  w)) 1))
	     )))

  (define (valid-timebase? tb)
    ;; This is the only implemented timebase right now ;)
    (eq? tb nulltimebase))
     
  (define-syntax app
    (syntax-rules ()
      [(_ f args ...) (f args ...)]))
  (define-syntax assert-type
    (syntax-rules ()
      [(_ t e) e]))

  ;; For these programs, need letrec*.
#;
  (define-syntax ws-letrec
    (syntax-rules ()
      ;; We assume type info has already been stripped.
      [(_ x ...) (letrec* x ...)]))

  ;(define-for-syntax for-loop-stack (make-parameter '()))
  (define for-loop-stack (make-parameter '()))
  
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

  ;; We just call the continuation, the fluid-let worries about popping the stack.
  (define (break) ((car (for-loop-stack)) (void)))



  ;; ================================================================================
  ;; Reading binary data:

  ;; Read two bytes from a string and build a uint16.
  (define (to-uint16 str ind)  ;; Internal helper function.
    (fx+ (fx* (char->integer (string-ref str (fx+ 1 ind))) 256)
	 (char->integer (string-ref str (fx+ 0 ind)))
	 ))

  ;; The signed version
  (define (to-int16 str ind) 
    (let ([unsigned (to-uint16 str ind)])
      (if (fxzero? (fxlogand unsigned 32768))
	  unsigned
	  (begin 
					;(inspect unsigned)
	    (fx- unsigned 65536)))))
  
  (define to-int32 'to-int32_unimplemented!)  

  (define (type->width t)
    (match t
      [Int16 2]
      [Int 4] ;; INTS ARE 16 BIT FOR NOW!!! FIXME FIXME
      ;;[Float 32]
      ;;[Complex ]    
      [,other (error 'type->width "can't support binary reading of ~s yet." other)]
      ))
  (define (types->width types)
    (apply + (map type->width  types)))
  (define (types->reader types)
     (define (type->reader t)
       (match t
	 [Int16 to-int16]
	 [Int to-int32]
	;[Float ]
	;[Complex ]
	 [,other (error 'type->reader "can't support binary reading of ~s yet." other)]))
     (define readers (list->vector (map type->reader types)))
     (define widths (list->vector (map type->width types)))
     (define len (s:length types))
     (cond 
      [(= 0 len) (error 'types->reader "can't read unit type (zero-length tuple) from a file!")]
      [(= 1 len) (type->reader (car types))]
      [else (lambda (str ind)
	      (let ([vec (make-vector len)])
		(let unmarshallloop ([i 0] [pos ind])
		  (if (= i len)
		      vec
		      (begin (vector-set! vec i ((vector-ref readers i) str pos))
			     (unmarshallloop (fx+ 1 i) (fx+ pos (vector-ref widths i)))))
		  )))]))

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
;      (define nullarr (gensym "nullarr"))
;      (define nulltimebase (gensym "nulltimebase"))
  (define nullseg 'nullseg)
  (define nullarr #())
  (define nulltimebase 'nulltimebase)

  (define (gint x) x)

  (define g+ s:+) (define g- s:-) (define g* s:*) (define g/ s:/)

  (define ws+ fx+)   (define ws- fx-)   (define ws* fx*)   (define ws/ fx/)
  (define +_ fx+)    (define -_ fx-)    (define *_ fx*)    (define /_ fx/)
  (define +I16 fx+)  (define -I16 fx-)  (define *I16 fx*)  (define /I16 fx/)
  (define +. fl+)    (define -. fl-)    (define *. fl*)    (define /. fl/)
  (define +: cfl+)   (define -: cfl-)   (define *: cfl*)   (define /: cfl/)

  (define ws^ expt)
  (define g^ expt)
  (define ^_ expt)
  (define ^I16 expt)
  (define ^. expt)
  (define ^: expt)

  (define (sqrtI n) (flonum->fixnum (sqrt n)))
  (define sqrtF sqrt)
  (define sqrtC sqrt)
     
     ;; These shouldn't be implemented because they should be desugared earlier!
     ; (define (toFloat n)
;        (cond
; 	[(fixnum? n) (fixnum->flonum n)]
; 	[(flonum? n) n]
; 	[else (error 'toFloat "may only be used for upcast, given: ~s" n)]))
;      (define (toComplex n) (s:+ n 0.0+0.0i))

  (define realpart cfl-real-part)
  (define imagpart cfl-imag-part)
  
  (define absI16 fxabs)
  (define absI fxabs)
  (define absF flabs)
  (define absC s:abs)

  (define intToFloat fixnum->flonum)
  (define floatToInt flonum->fixnum)

  (define (intToComplex n) (s:+ n 0.0+0.0i))
  (define (floatToComplex f) (s:fl-make-rectangular f 0.0))
  (define (complexToInt c) (flonum->fixnum (realpart c)))
  (define complexToFloat realpart)

  (define intToInt16 (lambda (x) x))
  (define int16ToInt (lambda (x) x))
  (define int16ToFloat   intToFloat)
  (define int16ToComplex intToComplex)

  ;; TODO: MERGE THIS WITH DUPLICATED CODE IN STATIC-ELABORATE!!

  (define stringToInt (lambda (v) 
		 (let ([x (string->number v)])
		   (if x 
		       (ASSERT fixnum? x)
		       (error 'stringToInt "couldn't convert string: ~s" v)))))
  (define stringToFloat (lambda (v) 
		   (let ([x (string->number v)])
		     (if x 
			 (ASSERT flonum? x)
			 (error 'stringToFloat "couldn't convert string: ~s" v)))))
  (define stringToComplex (lambda (v) 
		   (ASSERT string? v)
		   (let ([x (string->number v)])
		     (cond
		      [(not x) (error 'stringToComplex "couldn't convert string: ~s" v)]
		      [(real? x) (s:fl-make-rectangular x 0.0)]
		      [else (ASSERT cflonum? x)]))))
  
  (define (roundF f) (flonum->fixnum ((IFCHEZ flround round) f)))

  ;; [2006.08.23] Lifting ffts over sigsegs: 
  ;; Would be nice to use copy-struct for a functional update.
  (define (fft ss)
					;(import scheme) ;; Use normal arithmetic.
    (define (log2 n) (s:/ (log n) (log 2)))
    (DEBUGASSERT (valid-sigseg? ss))
    (DEBUGMODE 
     (if (eq? ss nullseg) (error 'fft "cannot take fft of nullseg"))
     (if (or (= 0 (vector-length (sigseg-vec ss)))
	     (not (integer? (log2 (vector-length (sigseg-vec ss))))))
	 (error 'fft "only window sizes that are powers of two are supported: length ~s" 
		(vector-length (sigseg-vec ss)))))
    (let* ([double (dft (sigseg-vec ss))]
	   [halflen (add1 (quotient (vector-length double) 2))]
	   [half (make-vector halflen)])
      (vector-blit! double half 0 0 halflen)
      (make-sigseg 0 (sub1 halflen) half (sigseg-timebase ss))
      ))


  (define (wserror str) (error 'wserror str))
     (IFCHEZ (define inspect inspect/continue)
             ;; Don't know of an interactive object inspector in PLT:
             (define (inspect x) x))             

     (define tuple vector)
     (define (tupref ind _ v)
       (DEBUGMODE (unless (vector? v) (error 'tupref "this is not a tuple: ~s" v)))
       (vector-ref v ind))

     (define List:length orig-length)
     (define List:ref list-ref)

     (define List:make make-list)
     (define List:head car)
     (define List:tail cdr)

     (define List:reverse reverse)
     (define List:append append)
     (define List:map map)

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
	  [(equal? (vector-ref (car ls) 0) x) ls]
	  [else (loop (cdr ls))])))
     (define (List:assoc_update origls x y)
       (let loop ([ls origls] [acc '()])
	 (cond
	  [(null? ls) (cons (vector x y) origls)]
	  [(equal? (vector-ref (car ls) 0) x)
	   (append (reverse! acc) (cons (vector x y) (cdr ls)))]
	  [else (loop (cdr ls) (cons (car ls) acc))])))

     (define makeArray make-vector)
     (define arr-get  vector-ref)
     (define arr-set! vector-set!)
     (define ws-length   vector-length)

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
       (define hashtable #%make-hash-table)
       (define (hashcontains ht k) (#%get-hash-table ht k #f))
       (define (hashget ht k) (#%get-hash-table ht k #f))
       ;; Pretty useless nondestructive version:
       (define (hashset ht k v)
	 (define new (copy-hash-table ht))
	 (put-hash-table! new k v)
	 new)
       (define (hashset_BANG ht k v)
	 (#%put-hash-table! ht k v)
	 ht)
       ;(define hashrem )
       ;(define hashrem_BANG )
       )

     ;; EQUAL? based hash tables:
     (begin
       (define hashset_BANG (slib:hash-associator equal?))
       
       (define (copy-hash-table ht)
	 ;; This is terrible, we don't know how big it is.
	 (let ([newtab (slib:make-hash-table (vector-length ht))])
	   (slib:hash-for-each ht
	    (lambda (k v) (hashset_BANG newtab k v)))
	   newtab))

       (define hashtable slib:make-hash-table)
       (define hashcontains 
	 (let ([getfun (slib:hash-inquirer equal?)])
	   (lambda (ht k) (if (getfun ht k) #t #f))))       
       (define hashget 
	 (let ([getfun (slib:hash-inquirer equal?)])
	   (lambda (ht k)
	     (let ([result (getfun ht k)])
	       (unless result
		 (error 'hashget "couldn't find key: ~s" k))
	       result
	       ))))
       ;; Pretty useless nondestructive version:
       ;; If we cared we could use some kind of balanced tree for functional maps.
       (define (hashset ht k v)
	 (define new (copy-hash-table ht))
	 (hashset_BANG new k v)
	 new)

       (define hashrem_BANG (slib:hash-remover equal?))
       (define (hashrem ht k) 
	 (define new (copy-hash-table ht))
	 (hashrem_BANG new ht k)
	 new)
       )

     (define (ws-print x)
       (parameterize ([current-output-port (ws-print-output-port)])
	 (if (string? x)
	     (display x)
	     (display-constrained (list x 300)))))

     ;; Show is like display, should have something else like write:
     (define (show x) (format "~a" x))

     (define (gnuplot_array arr)   (gnuplot (vector->list arr)))

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

  
     (define m_invert ws-invert-matrix)

     ;;================================================================================

     ;; This is a bit silly.  Since we don't use an actual time-series
     ;; implementation, this just makes sure the overlap is EQUAL.
     (define (joinsegs w1 w2)
       (DEBUGASSERT (valid-sigseg? w1))
       (DEBUGASSERT (valid-sigseg? w2))
       (DEBUGASSERT valid-sigseg?
	(cond 
	 [(eq? w1 nullseg) w2]
	 [(eq? w2 nullseg) w1]
	 [else 
	  (let ([a (sigseg-start w1)]
		[b (sigseg-end w1)]
		[x (sigseg-start w2)]
		[y (sigseg-end w2)])
	   (cond
	    [(not (eq? (sigseg-timebase w1) (sigseg-timebase w2)))
	     (error 'joinsegs "Cannot handle different TimeBases!")]

	    ;; In this case the head of w2 is lodged in w1:
	    ;; OR they line up precisely.
	    [(and (<= a x) (<= x (+ b 1)))
	     (DEBUGASSERT (sigseg? w1))
	     (DEBUGASSERT (sigseg? w2))
	     ;(printf "JOINING: ~a:~a and ~a:~a\n" (sigseg-start w1) (sigseg-end w1) (sigseg-start w2) (sigseg-end w2))
	     	     
	     (let ([new (make-vector (add1 (- (max b y) a)))])
	       (for (i a (max b y))
		   (define (first) (vector-ref (sigseg-vec w1) (- i a)))
		 (define (second) (vector-ref (sigseg-vec w2) (- i x)))
					;		  (printf "i ~a\n" i)
		 (vector-set! new (- i a)
			      (cond
			       ;; Still in the first window:
			       [(< i x) (first)]
			       ;; We're in the overlap:
			       [(and (<= i b) (<= x i) (<= i y))
				(if (eq? (first) (second))
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
	 [(eq? w nullseg) (error 'subseg "cannot subseg nullseg: ind:~s len:~s" startind len)]
	 [(< len 0) (error 'subseg "length of subseg cannot be negative!: ~s" len)]
	 [(= len 0) nullseg]
	 [(or (< startind (sigseg-start w))
		(> (+ startind len -1) (sigseg-end w)))
	   (error 'subseg "cannot take subseg ~a:~a from sigseg ~s" startind (+ startind len -1) w)]
	 [else 
	  (let ([vec (make-vector len)])
	    (for (i 0 (fx- len 1))
		(vector-set! vec i 
			     (vector-ref (sigseg-vec w) 
					 (+ i (- startind (sigseg-start w))))))
	    (make-sigseg startind (+ startind len -1) vec (sigseg-timebase w)))])))


     ;; [2007.01.26] Changing this back to be zero-based.
     (define (seg-get w ind) 
       (DEBUGASSERT valid-sigseg? w)
       (if (eq? w nullseg) (error 'seg-get "cannot get element from nullseg!"))
       (DEBUGMODE (if (or (< ind 0) (>= ind (width w)))
		      (error 'seg-get "index ~a is out of bounds for sigseg:\n~s" ind w)))
       (vector-ref (sigseg-vec w) ind))
     (define (width w) 
       (DEBUGASSERT (valid-sigseg? w))
       (if (eq? w nullseg) 0 (vector-length (sigseg-vec w))))
     (define (start w) 
       (DEBUGASSERT (valid-sigseg? w))
       (if (eq? w nullseg) (error 'start "cannot get start index from nullseg!"))
       (sigseg-start w))
     (define (end w) 
       (DEBUGASSERT (valid-sigseg? w))
       (if (eq? w nullseg) (error 'end "cannot get end index from nullseg!"))
       (sigseg-end w))
     (define (timebase w) 
       (DEBUGASSERT (valid-sigseg? w))
       ;; Is this true?  Or does each signal have its own nullseg?? That could be very tricky...
       ;; Well, the main thing we need nullseg for, as I see it, is initializing accumulators.
       (if (eq? w nullseg) (error 'end "cannot get timebase from nullseg!"))
       (sigseg-timebase w))
     (define (toArray w) (if (eq? w nullseg) #() (sigseg-vec w)))
     (define (toSigseg ar st tb)
       (define en (fx+ st (s:vector-length ar) -1))
       (DEBUGASSERT (or (eq? ar nullarr) (vector? ar)))
       (DEBUGASSERT integer? st)
       (DEBUGASSERT integer? en)
       (DEBUGASSERT valid-timebase? tb)
       (if (not (= (vector-length ar) (+ en (- st) 1)))
	   (error 'toSigseg "vector's size did not match start/end tags: ~s:~s ~s" st en ar))
       (DEBUGASSERT valid-sigseg?
		    (make-sigseg st en ar tb)))
       

     ;; Export these, they override the default scheme bindings.
     ;; ----------------------------------------
     (IFCHEZ (begin (define + ws+)
		    (define - ws-) 
		    (define * ws*) 
		    (define / ws/)
		    (define ^ ws^) 
		    ;(define-syntax letrec (identifier-syntax ws-letrec))
;		    (define-id-syntax letrec  ws-letrec)
		    (define length ws-length)
		    ;(define-id-syntax print ws-print)
		    (define print ws-print)
		    )
	     (provide (rename ws+ +) (rename ws- -) (rename ws* *) (rename ws/ /) (rename ws^ ^)
;		      (rename ws-letrec letrec)
		      (rename ws-length length)
		      (rename ws-print print)                      
                      for ;for-loop-stack
                      ))



) ; End module.


