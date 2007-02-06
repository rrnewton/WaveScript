;; [2007.02.03] Updating this version to use a new representation of
;; streams.  Streams are just thunks, called repeatedly.  Less risk of
;; memory leak.

(module wavescript_sim_library_NEW mzscheme
  (require 
           "../constants.ss"
           "../../plt/iu-match.ss"
           "../util/fft.ss"
           "../langs/lang_wavescript.ss"
           (prefix slib: "../util/slib_hashtab.ss")
	   (all-except "../util/imperative_streams.ss" test-this )
	   (all-except "../util/helpers.ss" test-this these-tests for inspect break)	   
	   (all-except "../compiler_components/regiment_helpers.ss" test-this these-tests for inspect break)	   
	   )
  (provide
                 make-sigseg sigseg-start sigseg-end sigseg-vec sigseg-timebase
		 valid-sigseg?
		 app let 

		 ;__dataFile 
		 ;__syncN

		 ;dump-binfile 
		 ;doubleFile audioFile  audio timer
		 stockStream
		 ; read-file-stream
		 show
		 window

		 to-uint16 to-int16 uint16->string

		 gint
		 g+ g- g* g/ g^
		 +_ -_ *_ /_ ^_
		 +. -. *. /. ^.
		 +: -: *: /: ^:
		 sqrtF sqrtC sqrtI
		 roundF		 

		 intToFloat floatToInt
		 floatToComplex complexToFloat
		 intToComplex complexToInt

		 ;toComplex toFloat  ;; Generic versions
		 ;toInt ;; Truncate
		 ;roundToInt

		 realpart imagpart 
		 ;cnorm
		 absI absF absC

		 nullseg nullarr nulltimebase
		 tuple tupref
		 makeArray arr-get arr-set! 
		 hashtable hashcontains hashget hashset hashset_BANG hashrem hashrem_BANG

		 listRef listLength makeList head tail
		 fold alist_lookup alist_update

		 joinsegs subseg seg-get width start end timebase
		 to_array toSigseg 

		 assert-type
		 
		 wserror inspect
		 emit 
		 ;smap sfilter
		 iterate break ;deep-iterate
		 ;; TODO: nix unionList.
		 ;unionN unionList 
		 ;zip2
		 ; union2 union3 union4 union5
		 fft 
		 
		 ;; Misc, ad-hoc, and Temporary
		 ;m_invert ;; A matrix inversion:
		 )
    (chezprovide (for for-loop-stack)
		 letrec length print
		 + - * / ^
                                 
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
		 (only lang_wavescript default-marmotfile))

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
	    (require (prefix s: mzscheme))))

  
  ;; Contains a start and end SEQUENCE NUMBER as well as a vector.
  (reg:define-struct (sigseg start end vec timebase))
     ;(define-record timeseries (timebase))
     
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

     ;; Currently unused.
     (define (uint16->string n)
       (if (>= n 65536)
	   (error 'uint16->string "input is too large: ~s" n))
       (let* ([lowbyte (fxmodulo n 256)]
	      [highbyte (fx/ (fx- n lowbyte) 256)])
	 (list->string (list (integer->char highbyte)
			     (integer->char lowbyte)))))

#;
     (define (dump-binfile file stream pos)
       (let ([port (open-output-file file 'replace)])
	 (time 
	  (progress-dots
	   (lambda () 
	     (let loop ()
	       (if (stream-empty? stream)
		   (printf "Finished, dumped ~a stream elements.\n" pos)
		   (let process ([elem (stream-car stream)])		 
		     (cond
		      [(integer? elem)
		       (display (uint16->string elem) port)]
		      [(sigseg? elem)	
		       (vector-for-each (lambda (x) (display (uint16->string x) port))
					(sigseg-vec elem))]
		      ;; This is a lame hack to circumvent generativity of records:
		      [(reg:struct? elem) (process (apply make-sigseg (cdr (reg:struct->list elem))))]

		      [else (error 'dump-binfile "Cannot handle stream element: ~s" elem)])

		       (set! pos (add1 pos))
		       (set! stream (stream-cdr stream))
		       (loop)
		       ))))
	     5000000
	     (lambda () (printf "  POS# ~a dumped...\n" pos))))))


#;
     ;; FIXME: this is inefficient; keeping a ptr to the tail of the stream
     ;;        and modifying that would be better
     (define (doubleFile file len overlap)
       ;; Ignoring overlap for now!!
       (unless (zero? overlap)
	 (error 'doubleFile "does not currently support overlaps, use rewindow!"))
       (delay ;; Don't read file till we get a pull.
	 (let ((infile (open-input-file file)))
         (let strmloop ([t 0])
	   (let ([vec (make-vector len)])
	     (let inner ([cnt 0])
	       (if (fx= cnt len)
		   ;; Vector is full.
		   (stream-cons (make-sigseg t (+ t len -1) vec nulltimebase)
				(strmloop (+ t len))) ;; time could overflow fixnum		 
		   (let ((n (read infile))) ;; would read-line be faster?
		     (if (eof-object? n)
			 ;; Do we make a partial sigseg?  Nah.  Throw it out.
			 '()
			 (begin 
			   (vector-set! vec cnt n)
			   (inner (fx+ 1 cnt))
			   )
			 )))))))))

     ;; Read a stream of Uint16's.
     (define (audioFile file len overlap)
       (read-file-stream file 
			 2 ;; Read just 2 bytes at a time.
			 to-uint16
			 len overlap))

     ;; We read in "blocks" to reduce the overhead of all those thunks!
     ;; (Actually, this didn't speed things up much, just a little.)
     (define DATAFILE_BATCH_SIZE 500)

     ;; This is not a fast implementation.  Uses read.
     ;; Interestingly, it's currently [2006.12.03] running better in opt-level 2 than 3!
     (define (__dataFile file mode repeat types)
       ;; This is one playback of the file:
       (define thestream
	 (let ()
	   ;; TODO: In debug mode this should check the types of what it gets.       
	   (define len (listLength types))
	   (define inp (open-input-file file))
	   (define tyvec (list->vector types))
	   (define (parse-line str)
	     (define p (open-input-string str))
	     (define tup (make-vector len))
	     (let loop ([i 0])
	       (if (fx= i len)
		   tup
		   (begin 
		     (vector-set! tup i 
				  (case (vector-ref tyvec i)
				    [(String) (symbol->string (read p))]
				    [else (read p)]))
		     (loop (fx+ 1 i))))))
	   (cond 
	    [(equal? mode "text")
	     (let loop ([x (read-line inp)] [batch DATAFILE_BATCH_SIZE])
	       (cond
		[(not x) '()]
		;; Insert a delay:
		[(fxzero? batch) 		 
		 (stream-cons (parse-line x) (loop (read-line inp) DATAFILE_BATCH_SIZE))]
		[else   (cons (parse-line x) (loop (read-line inp) (fx- batch 1)))]))]
	    [else (error 'dataFile "this mode is not supported yet: ~s" mode)]	)))
       ;(inspect thestream)
       (case repeat
	 [(0) thestream]
	 [(-1) (letrec ([fix (stream-append thestream (delay fix))]) fix)]
	 [else (ASSERT (> repeat 0))
	       ;; Foldl and foldr have about the same (bad) performance here.
	       (foldl stream-append '()
		      (make-list repeat thestream))])
       )

     ;; This makes an infinite stream of fake tick/split info:
     ;; Tuple is of one of two forms:
     ;;  Tick:  #(sym,t,vol,price)
     ;;  Split: #(sym,t,-1,factor)
     (define (stockStream)
       (define (random-sym) 
	 (vector-get-random 
	  #("IBM" "AKAM" "MS" "GOOG" "AMAZ" "YHOO" "ORAC" 
		)))
       (define t (random 100.0))
       (lambda ()
	 (let ([result (if (fx= (random 500) 0)
			   ;; A split:
			   (vector (random-sym) t -1 (random 2.0))
			   ;; A tick:
			   (vector (random-sym) t (fx+ 1 (random 100)) (random 300.0))
			   )])

	   (set! t (fl+ t (random 10.0)))
	   result
	   )))

     ;; This is a hack to load specific audio files:
     ;; It simulates the four channels of marmot data.
     (define (audio chan len overlap)
       (define (read-sample str index)
	 (let ([s str] [ind index])
	   ;; Just the requested channel:
	   (fixnum->flonum ;; For now this returns float.
	    (to-int16 s (fx+ ind (fx* chan 2))))
	   ;; All 4 channels:
	   ;; NIXING: This allocation of little vectors is really painful performance wise.
	   #;
	   (vector (to-int16 s ind)
		   (to-int16 s (fx+ 2 ind))
		   (to-int16 s (fx+ 4 ind))
		   (to-int16 s (fx+ 6 ind)))
	   ))
       (read-file-stream (default-marmotfile) 8 read-sample len overlap))

     ;; Internal helper.
     (define (read-file-stream file wordsize sample-extractor len overlap)
       (define chunksize 32768) ;; How much to read from file at a time.
       (define infile (open-input-file file))
       (define buffer1 (make-string chunksize #\_))
       (define count1 0)
       (define ind1 0)
       (define winsize (* wordsize len))
       (define remainder #f) ;; The unprocessed left-over from a batch.

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
	     ;(warning 'read-window "this version depends on block-read always getting all the chars, got ~a, wanted ~a"	count1 winsize)
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

       ;; This returns the stream representing the audio channel (read in from disk):
       ;; TODO: HANDLE OVERLAP:
       (unless (zero? overlap)
	 (error 'read-file-stream "currently does not support overlaps, use rewindow"))

       
       (delay 
	 (let loop ([pos 0]) 
	   (let ([win (read-window)])
	     (if win
		 (let ((newpos (+ len pos -1)))
		   (stream-cons (make-sigseg pos newpos win nulltimebase)
				(loop (+ 1 newpos))))
		 ()))))

#;       
       (delay 
	 (let loop ([pos 0]) 
	   (let ([win (read-window)])
	     (if win
		 (let ((newpos (+ len pos -1)))
		   (stream-cons (make-sigseg pos newpos win nulltimebase)
				(loop (+ 1 newpos))))
		 ()))))
       )

     ;; This is meaningless in a pull model:
     (define (timer freq)
       (let loop () (stream-cons #() (loop))))

;      (define nullseg (gensym "nullseg"))
;      (define nullarr (gensym "nullarr"))
;      (define nulltimebase (gensym "nulltimebase"))
     (define nullseg 'nullseg)
     (define nullarr #())
     (define nulltimebase 'nulltimebase)

     (define (gint x) x)

     (define g+ s:+) (define g- s:-) (define g* s:*) (define g/ s:/)

     (define ws+ fx+) (define ws- fx-) (define ws* fx*) (define ws/ fx/)
     (define +_ fx+) (define -_ fx-) (define *_ fx*) (define /_ fx/)

     (define +. fl+)    (define -. fl-)    (define *. fl*)    (define /. fl/)
     (define +: cfl+)   (define -: cfl-)   (define *: cfl*)   (define /: cfl/)

     (define ws^ expt)
     (define g^ expt)
     (define ^_ expt)
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
     
     (IFCHEZ
      (begin (define realpart cfl-real-part)
             (define imagpart cfl-imag-part))
      (begin (define realpart real-part)
             (define imagpart imag-part)))
      
     (define absI fxabs)
     (define absF flabs)
     (define absC s:abs)

     (define intToFloat fixnum->flonum)
     (define floatToInt flonum->fixnum)

     (define (intToComplex n) (s:+ n 0.0+0.0i))
     (define (floatToComplex f) 
       (IFCHEZ (s:fl-make-rectangular f 0.0)
	       (s:+ f 0.0+0.0i)))
     (define (complexToInt c) (flonum->fixnum (realpart c)))
     (define complexToFloat realpart)
    
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
       (make-sigseg (sigseg-start ss)
		    (sigseg-end ss)
		    (dft (sigseg-vec ss))
		    (sigseg-timebase ss)))


#;
     ;; Making this a primitive for the emulator.
     (define __syncN
       (let ()
         
         ;; The policy here is that we read from the ctrl stream.
         ;; Then we read what data we need from the data streams to meet the request.
         (define (outer ctrl accs) 
	   ;(if (zero? (random 10000)) (call/cc inspect))	  
	   ;(call/cc inspect)
	   (if (zero? (random 10000))
	       (printf "  OUTER LOOP ~s\n" 
		       (map (lambda (vec) (width (vector-ref vec 1)))
			 accs)))
           (call/1cc
            (lambda (exit-this)
              (if (stream-empty? ctrl) '()
                  (let-match ([#(,flag ,st ,en) (stream-car ctrl)])               
                    
		    (define (isgood? ss) 
		      (and (not (equal? ss nullseg))
			   (or (<= (start ss) st) (not flag))
			   (>= (end ss) en)))
		    ;; When we have data on all accs, proceed:
		    (define (doit accs)
		      (when flag (printf "DATA READY: ~s\n" (map (lambda (v) (list (start (vector-ref v 1)) (end (vector-ref v 1)))) accs)))
		      (match accs
			[(#(,strm* ,seg*) ...)
			 (let ([newctrl (stream-cdr ctrl)]
			       [newaccs (map (lambda (strm seg)
					; 						(printf "NOW EXTRACTING: ~s ~s ~s\n"
					; 							(add1 en)
					; 							(s:- (width seg) (s:- (add1 en) (start seg)))
					; 							(s:+ 1 (s:- (end seg) (add1 en))))
						     (vector strm
							     (subseg seg (add1 en) 
								     (s:+ 1 (s:- (end seg) (add1 en))))))
						strm* seg*)])
			   (if flag
			       (begin
				 ;; (printf "YEAH POSITIVE FLAG\n");
				 (cons (list->vector (map (lambda (ss) (subseg ss st (- en st -1))) seg*))
				       (delay (outer newctrl newaccs))))
			       (outer newctrl newaccs)))]))
		    (define (readit strm seg) 
		      (if (stream-empty? strm)
			  (exit-this '()) ;; We're done, can't go any further.
			  (vector (stream-cdr strm)
				  (joinsegs seg (stream-car strm)))
			  ))

		    ;; Keep pulling on those input streams until we're ready to proceed.
		    (define (inner remaining ready)
					;	 (printf "    INNER ~s ~s\n" (s:length remaining) (s:length ready))
		      (match remaining
			[() (doit ready)]
			[(#(,strm ,seg) . ,rest)
			 (if (isgood? seg) 
			     ;; This input channel is ready, check the next.
			     (inner rest (cons (car remaining) ready))
			     ;; Otherwise we read ours & go to the end of the line.
			     (inner (snoc (readit strm seg) rest)
				    ready))]))

		    (when flag (printf "PROCESSING REQUEST: ~s\n" (stream-car ctrl)))
                    (inner accs '())
                    )))))

	 (lambda (ctrl strms)
	   (outer ctrl (map (lambda (s) (vector s nullseg)) strms))
	   ;(inspect 3)
	   ;'(900 901 902)
	   )))


;; FINISH
#;
     ;; [2006.09.01] Crap, how do we do this in a pull model, eh?
     ;; USES ZERO-BASED INDICES.
     (define (unionList ls)
       ;; There are all kinds of weird things we could do here.
       ;; We could pull all the streams in parallel (with engines or threads) 
       ;; and nondeterministically interleave.
	 
       ;; TEMP: this strategy just assumes they're all the same rate and round-robins them:
       (let loop ([streams (mapi vector ls)])
	 (if (null? streams) '()
	     (let ([batch (map (lambda (v) (vector (vector-ref v 0) (stream-car (vector-ref v 1))))
			    streams)])
	       (stream-append-list batch
			      (loop (filter (lambda (v) (not (stream-empty? (vector-ref v 1))))
				      (map (lambda (v) (vector (vector-ref v 0) (stream-cdr (vector-ref v 1))))
					streams)))))))
       )

#;
     ;; [2006.11.23] Experimenting with engine based version:
     (define (unionList ls)
       (let* ([output #f] ;; Mutable var for output.
	      [engs (mapi (lambda (ind strm)
			  (make-engine 
			   (lambda ()
			     (let strmloop ([strm strm])
			       (when (stream-empty? strm)
				 (engine-return '()))
			       (set! output (vector ind (stream-car strm)))
			       (engine-block) ;; Don't return more than one value.
			       (strmloop (stream-cdr strm))
			       ))))
			  ls)])
	 ;; Now we need to do the equivalent of a "select".
	 ;; We run each engine until we get a value.
	 (let loop ([engs engs] [acc '()])
	   ;; Process output, if there was any in the last run.
	   (cond
	    [output
	     (cons output 
		   (delay (begin (set! output #f) (loop engs acc))))]
	    [(null? engs)
	     (if (null? acc) ;; All streams finished.
		 '()
		 (loop (reverse! acc) '()))]
	    [else
	     ;; RUN engine:
	     ((car engs) 100
	      (lambda (ticks val)
		;; This stream is finished, continue with rest.
		(loop (reverse! acc) '())
		)
	      (lambda (neweng)		  
		;; Put us at the end of the queue.
		(loop (cdr engs) (cons neweng acc))
		))]
	    ))))

#;
     ;; [2007.01.30]  This is a hacky little experiment.  Prioritizes the first stream in the list.
     (define (unionList ls)
       (let* ([output #f] ;; Mutable var for output.
	      [engs (mapi (lambda (ind strm)
			  (make-engine 
			   (lambda ()
			     (let strmloop ([strm strm])
			       (when (stream-empty? strm)
				 (engine-return '()))
			       (set! output (vector ind (stream-car strm)))
			       (engine-block) ;; Don't return more than one value.
			       (strmloop (stream-cdr strm))
			       ))))
			  ls)])
	 ;; Now we need to do the equivalent of a "select".
	 ;; We run each engine until we get a value.
	 (let loop ([special (car engs)] [engs (cdr engs)] [acc '()])	   	   
	   ;; Process output, if there was any in the last run.
	   (cond
	    [output
	     (cons output 
		   (delay (begin (set! output #f) (loop special engs acc))))]
	    [(null? engs)
	     (if (null? acc) ;; All (other) streams finished.
		 (if special 
		     (loop #f (list special) '())
		     '())
		 (if special 
		     ;; Now we run the special engine.		     
		     (special 100
			      (lambda (ticks val)
				(loop #f (reverse! acc) '())
				)
			      (lambda (neweng)
				(if output 
				    (loop neweng '() acc)
				    ;; Only after the special one putters do we get back to the others.
				    (loop neweng (reverse! acc) '())
				    )))
		     ;; Otherwise just keep going with these other engines.
		     (loop #f (reverse! acc) '())
		     ))]
	    [else
	     ;; RUN engine:
	     ((car engs) 100
	      (lambda (ticks val)
		;; This stream is finished, continue with rest.
		(loop special (reverse! acc) '())
		)
	      (lambda (neweng)		  
		;; Put us at the end of the queue.
		(loop special (cdr engs) (cons neweng acc))
		))]
	    ))))

#;  
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

     (define (wserror str) (error 'wserror str))
     (IFCHEZ (define inspect inspect/continue)
             ;; Don't know of an interactive object inspector in PLT:
             (define (inspect x) x))             

     (define tuple vector)
     (define (tupref ind _ v)
       (DEBUGMODE (unless (vector? v) (error 'tupref "this is not a tuple: ~s" v)))
       (vector-ref v ind))

     (define listLength orig-length)
     (define listRef list-ref)

     (define makeList make-list)
     (define head car)
     (define tail cdr)

     ;; These should really be defined in the language.  They aren't
     ;; currently [2006.10.26] because the elaborator isn't ready to
     ;; *not* inline their recursive definitions.
     (define (fold f zero ls)
       (let loop ([acc zero] [ls ls])
	 (if (null? ls) acc
	     (loop (f acc (car ls)) (cdr ls)))))
     (define (alist_lookup ls x)
       (let loop ([ls ls])
	 (cond
	  [(null? ls) '()]
	  [(equal? (vector-ref (car ls) 0) x) ls]
	  [else (loop (cdr ls))])))
     (define (alist_update origls x y)
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

     (define (show x) (format "~s" x))

     (define (window sig winsize)
       (let (
	     [start 0]
	     [samp  0]
	     [i     0]
	     [vec (make-vector winsize)])
	 (lambda ()
	   (let window-loop ([x (sig)])
	     (cond 
	      [(eq? x stream-empty-token) stream-empty-token]
	      [(= i winsize)
	       (let ([result (make-sigseg start (sub1 samp) vec nulltimebase)])
		 (set! start samp)
		 (set! samp samp)
		 (set! i 0)
		 (set! vec (make-vector winsize))
		 result)]
	      [else 
	       (vector-set! vec i x)
	       (set! samp (add1 samp))
	       (set! i (fx+ 1 i))
	       (window-loop (sig))]
	      )))))

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
	       (make-sigseg a (max b y) new (sigseg-timebase w1))
	       )]

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
       (DEBUGASSERT (valid-sigseg? w))
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
     (define (to_array w) (if (eq? w nullseg) #() (sigseg-vec w)))
     (define (toSigseg ar st en tb)
       (DEBUGASSERT (or (eq? ar nullarr) (vector? ar)))
       (DEBUGASSERT integer? st)
       (DEBUGASSERT integer? en)
       (DEBUGASSERT valid-timebase? tb)
       (if (not (= (vector-length ar) (+ en (- st) 1)))
	   (error 'toSigseg "vector's size did not match start/end tags: ~s:~s ~s" st en ar))
       (DEBUGASSERT valid-sigseg?
		    (make-sigseg st en ar tb)))
       
     ;(define smap stream-map)
     (define sfilter stream-filter)

     ;(define emission (make-parameter '()))
     ;; Doesn't use stream-map because iterate may produce variable output.
     (define (iterate f s)
       (let* ([outputs '()]
	      [produce! (lambda () 
			  (let ([next (rac outputs)])
			    (set! outputs (rdc! outputs))
			    next))])
	 (lambda ()
	   (let iter-loop ()
	     (if (null? outputs)		     
		 (let ([x (s)])
		   (if (eq? x stream-empty-token)
		       stream-empty-token
		       (begin 
			 (set! outputs 
			       (append! (unbox (f (stream-car s) (virtqueue))) outputs))
			 (produce!))))
		 (produce!))))))

     ;; This is the functional version of iterate.
     (define (integrate f zero s)
       (stream-map (let ([state zero])
		     (lambda (x)
		       (let ([vec (f x state)])
			 ;; Mutate state:
			 (set! state (vector-ref vec 1))
			 ;; Return value.
			 (vector-ref vec 0))))
		   s))

     ;; Very simple queue:
     ;; TODO: Could copy sigsegs on output here and see what the impact is.
     (define (emit q v) (set-box! q (cons v (unbox q))))

     ;;(emission (cons v (emission))))
     (define (virtqueue) (box '()))

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


     ;; We just call the continuation, the fluid-let worries about popping the stack.
     (define (break)
       ((car (for-loop-stack)) (void)))

     ;; Export these, they override the default scheme bindings.
     ;; ----------------------------------------
     (IFCHEZ (begin (define + ws+)
		    (define - ws-) 
		    (define * ws*) 
		    (define / ws/)
		    (define ^ ws^) 
		    ;(define-syntax letrec (identifier-syntax ws-letrec))
		    (define-id-syntax letrec  ws-letrec)
		    (define length ws-length)
		    ;(define-id-syntax print ws-print)
		    (define print ws-print)
		    )
	     (provide (rename ws+ +) (rename ws- -) (rename ws* *) (rename ws/ /) (rename ws^ ^)
		      (rename ws-letrec letrec)
		      (rename ws-length length)
		      (rename ws-print print)                      
                      for ;for-loop-stack
                      ))



) ; End module.


