;; [2006.07.22]

;; This language definition implements WaveScript, with its Sigsegs and all.
;; Uses the stream-processing prims from helpers.ss

;; TODO: Make the core language bindings into a module which is
;; imported when eval happens.  This way even petite with a limited
;; heap will still get performance.

;;======================================================================

;; Testing file IO on marmot audio traces:


;; This just checks some hard coded locations for the marmot file.
(define (marmotfile)
  (let ([file (cond
	       [(file-exists? "/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw")
		"/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw"]
	       [(file-exists? "~/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw")
		"~/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw"]
	       [(file-exists? "/tmp/100.raw")
		"/tmp/100.raw"]
	       [else (error 'marmotfile "couldn't find marmot data")])])
    (printf "Reading marmot datafile: ~s\n" file)
    file))
	
;; Takes 35 seconds using stupid approach (read-char).
(define (read-all)
  (wavescript-language)
  (time 
  (let loop ((s (audio 0 1024 0)))
    ;(if (= 0 (remainder n 100)) (display #\.))
;    (display #\.)  
;    (printf "~a\n" (stream-car s))
;    (ASSERT (= 1000 (vector-length (stream-car s))))
    (if (stream-empty? s) 'DONE
	(loop (stream-cdr s) ;(add1 n)
	      )))))
(define (read-n n)
  (wavescript-language)
  (time 
  (let loop ((s (audio 0 1024 0)) (n n))
    ;(if (= 0 (remainder n 100)) (display #\.))
;    (display #\.)  
;    (printf "~a\n" (stream-car s))
;    (ASSERT (= 1000 (vector-length (stream-car s))))
    (if (fxzero? n) 'DONE
	(loop (stream-cdr s) (fx- n 1)
	      )))))

;; Takes 3.3 seconds.
(define (baseline-read-all)
  (let ((p (marmotfile)))
     (time 
      (let loop ()
	(let ((c (#3%read-char p)))
	  (if (#3%eof-object? c)
	      'alldone
	      (loop)))))
     (close-input-port p)))

;; Takes 350-430 ms (depending on optimize-level) to load 315mb on faith.
;; (That must be using some disk caching, eh?)
;; Note: changing block size:
;;   10 - 17 sec
;;   100 - 2 sec
;;   1024 - 350 ms
;;   2048 - 260 ms
;;   16384 - 210-270 ms
;;   32768 - 180 ms
;;   65536 - 180-250 ms
(define (baseline-read-all-block)
  (define chunk 8000)
   (let ((p (open-input-file (marmotfile)))
	 (s (make-string chunk #\_)))
     (time 
      (let loop ()
	(let ((n (#3%block-read p s chunk)))
	  ;(printf "~a " n)
	  (if (#3%eof-object? n)
	      'alldone ;(printf "done: ~a\n" s)
	      (loop)))))
     (close-input-port p)))


(define-syntax define-inlined
  (syntax-rules ()
    [(_ (f x ...) e ...)
     (define-syntax f 
       (syntax-rules ()
	 [(_ x ...) e ...]))]))

(chez:module wavescript-language-module 
    (make-sigseg sigseg-start sigseg-end sigseg-vec sigseg-timebase
		 valid-sigseg?
		 app letrec (for for-loop-stack)

		 dump-binfile audioFile audio timer
		 ; read-file-stream
		 print show

		 to-uint16 to-int16 uint16->string
		 + - * / ^
		 +. -. *. /. ^.
		 +: -: *: /: ^:
		 sqrtf sqrtc sqrti
		 int_to_float float_to_int
		 realpart imagpart cnorm

		 nullseg nullarr nulltimebase
		 tuple tupref
		 makeArray arr-get arr-set! length
		 listLength makeList head tail
		 joinsegs subseg seg-get width start end timebase
		 to_array to_sigseg to-windowed 

		 assert-type
		 
		 wserror inspect
		 emit virtqueue
		 smap parmap sfilter
		 iterate break deep-iterate
		 unionList
		 fft 
		 
		 ;; Misc, ad-hoc, and Temporary
		 m_invert ;; A matrix inversion:

		 ;; We reexport these *module names* so that they can be imported subsequently.
		 mod_scheme  mod_helpers  mod_constants
		 ;; And import itself--this is so we can use import-only:
		 import
		 )

  ;; Import this for the module "scheme".
  ;(import (except scheme break length + - * / ^ inspect letrec))
  (import (only scheme scheme import))
  (import constants)
  (import helpers)

  (alias mod_helpers helpers)
  (alias mod_scheme scheme)
  (alias mod_constants constants)  

  ;; [2006.09.22] Ripped from slib:
  ;;@1 must be a square matrix.
  ;;If @1 is singlar, then @0 returns #f; otherwise @0 returns the
  ;;@code{matrix:product} inverse of @1.
  (define m_invert
    (let () 
      (define (matrix:cofactor mat i j)
	(define (butnth n lst)
	  (if (<= n 1) (cdr lst) (cons (car lst) (butnth (+ -1 n) (cdr lst)))))
	(define (minor matrix i j)
	  (map (lambda (x) (butnth j x)) (butnth i matrix)))
	(* (if (odd? (+ i j)) -1 1) (determinant (minor mat i j))))
      (define (determinant mat)
	(let ((n (#%length mat)))
	  (if (eqv? 1 n) (caar mat)
	      (do ((j n (+ -1 j))
		   (ans 0 (+ ans (* (list-ref (car mat) (+ -1 j))
				    (matrix:cofactor mat 1 j)))))
		  ((<= j 0) ans)))))
      (define (matrix:inverse mat)
	(let* ((det (determinant mat))
	       (rank (#%length mat)))
	  (and (not (zero? det))
	       (do ((i rank (+ -1 i))
		    (inv '() (cons
			      (do ((j rank (+ -1 j))
				   (row '()
					(cons (/ (matrix:cofactor mat j i) det) row)))
				  ((<= j 0) row))
			      inv)))
		   ((<= i 0) inv)))))
      ;; Here's the matrix-invert for wavescript:
      (lambda (mat)
	(list->vector 
	 (map list->vector
	   (matrix:inverse (map vector->list (vector->list mat))))))))
  
  ;; Contains a start and end SEQUENCE NUMBER as well as a vector.
  (reg:define-struct (sigseg start end vec timebase))
     ;(define-record timeseries (timebase))
     
  (define (valid-sigseg? w)
    (or (eq? w nullseg)
	(and (sigseg? w)
	     (<= (sigseg-start w) (sigseg-end  w))
	     (equal? (vector-length (sigseg-vec w))
		     (+ (sigseg-end w) (- (sigseg-start  w)) 1))
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
     (define-syntax letrec
       (syntax-rules ()
	 ;; We assume type info has already been stripped.
	 [(_ x ...) (letrec* x ...)]))

     (define for-loop-stack '())
     (define-syntax for
       (syntax-rules ()
	 [(_ (i st en) bod ...)
	  (call/1cc (lambda (escape)
		     (fluid-let ([for-loop-stack (cons escape for-loop-stack)])
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


     ;; [2006.11.18] TODO:
     ;; Michael, maybe you could fill this in?
     (define (doubleFile file len overlap)
       
       'DOUBLEFILE-UNFINISHED
       )


     ;; Read a stream of Uint16's.
     (define (audioFile file len overlap)
       (read-file-stream file 
			 2 ;; Read just 2 bytes at a time.
			 to-uint16
			 len overlap))

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
       (read-file-stream (marmotfile) 8 read-sample len overlap))

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
		 ())))))

     ;; This is meaningless in a pull model:
     (define (timer freq)
       (let loop () (stream-cons #() (loop))))

;      (define nullseg (gensym "nullseg"))
;      (define nullarr (gensym "nullarr"))
;      (define nulltimebase (gensym "nulltimebase"))
     (define nullseg 'nullseg)
     (define nullarr #())
     (define nulltimebase 'nulltimebase)

     (define + fx+)     (define - fx-)     (define * fx*)     (define / fx/)    
     (define +. fl+)    (define -. fl-)    (define *. fl*)    (define /. fl/)
     (define +: cfl+)   (define -: cfl-)   (define *: cfl*)   (define /: cfl/)

     (define ^ expt)
     (define ^. expt)
     (define ^: expt)

     (define (sqrti n) (flonum->fixnum (sqrt n)))
     (define sqrtf sqrt)
     (define sqrtc sqrt)

     (define int_to_float fixnum->flonum)
     (define float_to_int flonum->fixnum)
     
     (define (realpart x) (if (cflonum? x) (cfl-real-part x) x))
     (define (imagpart x) (if (cflonum? x) (cfl-imag-part x) 0))
     ;(define realpart cfl-real-part)
     ;(define imagpart cfl-imag-part)

     (define (cnorm c)
       (let ([real (realpart c)]
	     [imag (imagpart c)])
	 (import scheme) ;; Reset those numeric bindings to default!
	 (cond
	  [(zero? real) imag]
	  [(zero? imag) real]
	  [(>= (flabs real) (flabs imag))
	   (* (abs real) (sqrt (+ 1.0 (/ (* imag imag) (* real real)))))]
	  [else 
	   (* (abs imag) (sqrt (+ 1.0 (/ (* real real) (* imag imag)))))] )
	 ))

     ;; [2006.08.23] Lifting ffts over sigsegs: 
     ;; Would be nice to use copy-struct for a functional update.
     (define (fft ss)
       (import scheme) ;; Use normal arithmetic.
       (define (log2 n) (/ (log n) (log 2)))
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

     ;; [2006.09.01] Crap, how do we do this in a pull model, eh?
     (define (unionList ls)
       ;; There are all kinds of weird things we could do here.
       ;; We could pull all the streams in parallel (with engines or threads) 
       ;; and nondeterministically interleave.
	 
       ;; TEMP: this strategy just assumes they're all the same rate and round-robins them:
       (let loop ([streams (mapi vector ls)])
	 (if (null? streams) '()
	     (let ([batch (map (lambda (v) (vector (vector-ref v 0) (stream-car (vector-ref v 1))))
			    streams)])
	       (stream-append batch
			      (loop (filter (lambda (v) (not (stream-empty? (vector-ref v 1))))
				      (map (lambda (v) (vector (vector-ref v 0) (stream-cdr (vector-ref v 1))))
					streams)))))))
       )
     
     (define (wserror str) (error 'wserror str))
     (define inspect inspect/continue)

     (define tuple vector)
     (define (tupref ind _ v)
       (DEBUGMODE (unless (vector? v) (error 'tupref "this is not a tuple: ~s" v)))
       (vector-ref v ind))

     (define listLength #%length)
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
     (define length   vector-length)

     (define (print x)
       (if (string? x)
	   (display x)
	   (display-constrained (list x 300))))
#;
     (define (print x) 
       (if (string? x) (display x)	   
	   (parameterize ([print-length 50]
			  [print-level 5]
			  [print-graph #t])
	     (pretty-print x))))
     (define (show x) (format "~s" x))


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


     ;; Changing this to take an absolute sample number:
     (define (seg-get w ind) 
       (DEBUGASSERT (valid-sigseg? w))
       (if (eq? w nullseg) (error 'seg-get "cannot get element from nullseg!"))
       (DEBUGMODE (if (or (< ind (sigseg-start w)) (> ind (sigseg-end w)))
		      (error 'seg-get "index ~a is out of bounds for sigseg:\n~s" ind w)))
       (vector-ref (sigseg-vec w) (fx- ind (sigseg-start w))))
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
     (define (to_sigseg ar st en tb)
       (DEBUGASSERT (or (eq? ar nullarr) (vector? ar)))
       (DEBUGASSERT integer? st)
       (DEBUGASSERT integer? en)
       (DEBUGASSERT valid-timebase? tb)
       (if (not (= (vector-length ar) (+ en (- st) 1)))
	   (error 'to_sigseg "vector's size did not match start/end tags: ~s:~s ~s" st en ar))
       (DEBUGASSERT valid-sigseg?
		    (make-sigseg st en ar tb)))
     
     (define (parmap f s) (stream-parmap f s))
     (define smap stream-map)
     (define sfilter stream-filter)

     ;(define emission (make-parameter '()))
     (define (iterate f s)
       ;(delay
       (let loop ((s s))
	 (if (stream-empty? s) 
	     '()
	     ;; Note, vals are reversed:
	     (let ([vals (unbox (f (stream-car s)))])
	       (cond
		[(null? vals) (loop (stream-cdr s))]
		[(null? (cdr vals)) (stream-cons (car vals) (loop (stream-cdr s)))]
		[else 
		 (append! (reverse! vals) 
			  (delay (loop (stream-cdr s))))])))))

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
     (define (deep-iterate f s)
       (stream-map 
;	(let ([start 0] ;; Record the range covered thusfar.
;	      [end 0])
	  (lambda (w)
	    (make-sigseg (sigseg-start w)
			 (sigseg-end w)
			 ;; The function for deep-iterate had better be one-to-one.  I.e. it's really a map!
			 (vector-map (lambda (x) 
				       (let ([ls (unbox (f x))])
					 (unless (and (not (null? ls)) (null? (cdr ls)))
					   ;; Wish I could give source location:
					   (error 'deep-iterate 
						  "for now deep-iterate only allows one-to-one functions to be applied, got result: ~a"
						  ls))
					 (car ls)))
				     (sigseg-vec w))
			 (sigseg-timebase w)))))

     (define (to-windowed s len overlap)
       (let ([win (make-vector len)]
	     [count 0])
	 (let towinloop ([s s])
	   (let ([x (stream-car s)])
	     (vector-set! win count x)
	     (set! count (add1 count))
	     (if (= count len)
		 (let ([copy (vector-copy win)])
		   (stream-cons copy (towinloop (stream-cdr s))))
		 (towinloop (stream-cdr s)))))))

     ;; We just call the continuation, the fluid let worries about popping the stack.
     (define (break)
       ((car for-loop-stack) (void)))

     
  
  ) ;; End module.

;; ======================================================================

(define-language
  'wavescript-language
  `(begin
     ;; We only import these basic bindings to keep us honest.
     (import-only wavescript-language-module)
     ;; Then we import some "sub-modules" exported by the language-module.
     ;; This is everything but the overriden bindings from default scheme language:
     (import (except mod_scheme break length + - * / ^ inspect letrec import))
     (import mod_constants)
     (import mod_helpers)
 
     ;(eval-when (compile eval load) (printf "TRYING..\n"))
;     (printf "TRYING..\n")
;     (inspect list)

     ;; A safety mechanism:
#;
     ,@(IFDEBUG
       '((let ([rawconstructor make-sigseg])	   
	   (set! make-sigseg (lambda (start end vec timebase)
			       (ASSERT (integer? start))
			       (ASSERT (integer? end))
			       (ASSERT (vector? vec))
			       (ASSERT (eq? timebase nulltimebase))
			       (rawconstructor start end vec timebase)))
	   ))
       '())
     
     ))


;; This uses a convoluted evaluation order.  But it allows us to eval
;; the wavescope defs ONCE at load time, and have them visible to all
;; the unit tests
(define these-tests
  (eval `(let ()
	   ,(wavescript-language 'return)
	   ;(define nulltimebase 'nulltimebase)
	   (list 
	    `["Joinsegs" 
	      (,(lambda ()
		  (reg:struct->list
		   (joinsegs (make-sigseg 10 19 (list->vector (iota 10)) nulltimebase)
			     (make-sigseg 15 24 (list->vector (map (lambda (x) (+ x 5)) (iota 10))) nulltimebase)))))
	      ("sigseg" 10 24 #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14) ,nulltimebase)]

	    `["Subseg"
	      (,(lambda ()
		  (reg:struct->list
		   (subseg (make-sigseg 10 19 (list->vector (iota 10)) nulltimebase) 11 5))))
	      ("sigseg" 11 15 #(1 2 3 4 5) ,nulltimebase)]

	    `["seg-get"
	      (,(lambda ()	
		  (seg-get (make-sigseg 10 19 (list->vector (iota 10)) nulltimebase) 12)))
	      2]

	    `["width/start/end"
	      (,(lambda ()	
		  (let ([seg (make-sigseg 11 20 (list->vector (iota 10)) nulltimebase)])
		    (list (width seg) (start seg) (end seg)))))
	      (10 11 20)]

	    `["audioFile"
	      (,(lambda ()	
		  (let* ([stream (audioFile (string-append (REGIMENTD) "/demos/wavescope/countup.raw")
					    1024 0)]
			 [first (stream-car stream)]
			 [second (stream-car (stream-cdr stream))])
		    (list (width first) (start first) (end first)
			  (width second) (start second) (end second)))))
	      (1024 0 1023
	       1024 1024 2047)]

	    `["for loop"
	      (,(lambda ()
		  (let ([sum 0])
		    ;; This outer loop goes 10 times.
		    (for (i 1 20)
			(set! sum (+ sum (* 1000 i)))
		      ;; This inner loop goes 10 times for each outer iteration (100 total)
		      (for (i 21 40)
			  (set! sum (+ sum 1))
			(if (= i 30) (break))
			)
		      (if (= i 10) (break)))
		    sum)
		  ))
	      55100]

	    
	    ))))

#;
(define these-tests
  (eval `(let ()
	   ,(wavescript-language 'return)
	   ;(define nulltimebase 'nulltimebase)
	   (list 
	    `["Joinsegs" 
	      (,(lambda ()
		  (reg:struct->list
		   (joinsegs (make-sigseg 10 19 (list->vector (iota 10)) nulltimebase)
			     (make-sigseg 15 24 (list->vector (map (lambda (x) (+ x 5)) (iota 10))) nulltimebase)))))
	      ("sigseg" 10 24 #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14) ,nulltimebase)]
	    `["Subseg"
	      (,(lambda ()
		  (reg:struct->list
		   (subseg (make-sigseg 10 19 (list->vector (iota 10)) nulltimebase) 11 5))))
	      ("sigseg" 11 15 #(1 2 3 4 5) ,nulltimebase)]

	    `["seg-get"
	      (,(lambda ()	
		  (seg-get (make-sigseg 10 19 (list->vector (iota 10)) nulltimebase) 12)))
	      2]

	    `["width/start/end"
	      (,(lambda ()	
		  (let ([seg (make-sigseg 11 20 (list->vector (iota 10)) nulltimebase)])
		    (list (width seg) (start seg) (end seg)))))
	      (10 11 20)]

	    `["audioFile"
	      (,(lambda ()	
		  (let* ([stream (audioFile (string-append (REGIMENTD) "/demos/wavescope/countup.raw")
					    1024 0)]
			 [first (stream-car stream)]
			 [second (stream-car (stream-cdr stream))])
		    (list (width first) (start first) (end first)
			  (width second) (start second) (end second)))))
	      (1024 0 1023
	       1024 1024 2047)]

	    `["for loop"
	      (let ([sum 0])
		;; This outer loop goes 10 times.
		(for (i 1 20)
		    (set! sum (+ sum (* 1000 i)))
		  ;; This inner loop goes 10 times for each outer iteration (100 total)
		  (for (i 21 40)
		      (set! sum (+ sum 1))
		    (if (= i 30) (break))
		    )
		  (if (= i 10) (break)))
		sum)
	      55100]
	    
	    ))))



(define test-this (default-unit-tester "Wavescript emulation language bindings" these-tests))
(define test-ws test-this)

; ======================================================================
;; SCRATCH

;; UNFINISHED VERSION: Doing this with different sized grab-chunk and window-size is very annoying:
#|
       (define (read-window)
	 (let ([win (make-vector len)])	       
	   (let loop ()
	      (cond
	      [remainder 
	       (ASSERT (fxzero? ind1))
	       (if (fx>= (fx+ (string-length remainder) count1) winsize)
		   (begin
		     ;; TODO FINISH:
		     'notdone
		     )
		   (error 'read-window "dammit, block-read just didn't read enough"))
	       ]

	      ;; We're chugging through buffer1 and there's still enough left:
	      [(fx>= (fx- count1 ind1) winsize)
	       (for (i 0 (fx- len 1))
		   (vector-set! win i (read-sample buffer1 (fx+ ind1 (fx* i 8)))))
	       (set! ind1 (fx+ ind1 winsize))
	       win]

	      ;; There's some left, but not enough.
	      [(fx< ind1 count1)
	       ;; Push this leftover to the remainder:
	       (printf "Leftover! ~a\n" (- count1 ind1))
	       (set! remainder (substring buffer1 ind1 count1))
	       ;; Load up some new input:
	       (set! count1 (block-read infile buffer1 chunksize))
	       (set! ind1 0)
 	       (if (eof-object? count1)
		   #f (loop))]

	      ;; We precisely used up everything, reload and loop.
	      [(fx= ind1 count1)
	       (set! count1 (block-read infile buffer1 chunksize))
	       (set! ind1 0)
	       (if (eof-object? count1)
		   #f (loop))
	       ]
	      ))))
|#
