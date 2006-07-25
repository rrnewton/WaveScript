;; [2006.07.22]

;; This language definition implements WaveScript, with its Sigsegs and all.
;; Uses the stream-processing prims from helpers.ss

;; Discrete Fourier Transform:
(define (dft x)
  (define (w-powers n)
    (let ((pi (* (acos 0.0) 2)))
      (let ((delta (/ (* -2.0i pi) n)))
        (let f ((n n) (x 0.0))
          (if (= n 0)
              '()
              (cons (exp x) (f (- n 2) (+ x delta))))))))
  (define (evens w)
    (if (null? w)
        '()
        (cons (car w) (evens (cddr w)))))
  (define (interlace x y)
    (if (null? x)
        '()
        (cons (car x) (cons (car y) (interlace (cdr x) (cdr y))))))
  (define (split ls)
    (let split ((fast ls) (slow ls))
      (if (null? fast)
          (values '() slow)
          (call-with-values
            (lambda () (split (cddr fast) (cdr slow)))
            (lambda (front back)
              (values (cons (car slow) front) back))))))
  (define (butterfly x w)
    (call-with-values
      (lambda () (split x))
      (lambda (front back)
        (values
          (map + front back)
          (map * (map - front back) w)))))
  (define (rfft x w)
    (if (null? (cddr x))
      (let ((x0 (car x)) (x1 (cadr x)))
        (list (+ x0 x1) (- x0 x1)))
      (call-with-values
        (lambda () (butterfly x w))
        (lambda (front back)
          (let ((w (evens w)))
            (interlace (rfft front w) (rfft back w)))))))
  (rfft x (w-powers (length x))))

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
   (let ((p (open-input-file "/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw")))
     (time 
      (let loop ()
	(let ((c (#3%read-char p)))
	  (if (#3%eof-object? c)
	      'alldone
	      (loop)))))
     (close-input-port p)))

;; Takes 350-430 ms (depending on optimize-level) to load 315mb on faith.
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
   (let ((p (open-input-file "/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw"))
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

(define-language
  'wavescript-language
  '(begin

     ;; Contains a start and end SEQUENCE NUMBER as well as a vector.
     (reg:define-struct (sigseg start end vec timebase))
     ;(define-record timeseries (timebase))
     
     (define-syntax app
       (syntax-rules ()
	 [(_ f args ...) (f args ...)]))

     ;; For these programs, need letrec*.
     (define-syntax letrec
       (syntax-rules ()
	 ;; We assume type info has already been stripped.
	 [(_ x ...) (letrec* x ...)]))
     (define-syntax for
       (syntax-rules ()
	 [(for (i start end) bod ...)
	  ;; TODO: IMPLEMENT BREAK:
	  (let ((s start) (e end))
	    (do ([v s (fx+ v 1)])
		((fx> v e) #());; Retun unit.
	      (let () bod ...)))]))

     ;; This is a hack to load specific audio files:
     (define (audio chan len overlap)
       (define chunksize 32768)
       (define infile (open-input-file "/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw"))
;       (define infile (open-input-file "/archive/4/marmots/test.raw"))
       (define buffer1 (make-string chunksize #\_))
       (define count1 0)
       (define ind1 0)
;       (define buffer2 (make-string chunksize #\_))
;       (define count2 0)
;        (define (swap)
; 	 (let ([tb buffer1] [tc count1])
; 	   (set! buffer1 buffer2)
; 	   (set! count1 count2)
; 	   (set! buffer2 tb)
; 	   (set! count2 tc)))       
       (define winsize (* 8 len))
       (define remainder #f) ;; The unprocessed left-over from a batch.
       (define (to-int16 str ind)	 
#;
	 (let ([s str] [i1 ind])
	   (let ([unsigned (fx+ (fx* 256 (char->integer (string-ref s i1)))
				(char->integer (string-ref s (fx+ 1 i1))))])
	     (if (fx< unsigned 32768)
		 unsigned
		 (fx- unsigned 65536))))
	 ;; This seems to do about the same, maybe slightly better.
	 (let ([unsigned (fx+ (fxsll (char->integer (string-ref str ind)) 8)
			      (char->integer (string-ref str (fx+ 1 ind))))])
	   (if (fxzero? (fxlogand unsigned 32768))
	       unsigned
	       (fx- unsigned 65536))
	 ))
       (define (read-sample str index)
	 (let ([s str] [ind index])
	   ;; Just the requested channel:
	   (to-int16 s (fx+ ind (fx* chan 2)))

	   ;; All 4 channels:
	   ;; This allocation of little vectors is really painful performance wise:
	   #;
	   (vector (to-int16 s ind)
		   (to-int16 s (fx+ 2 ind))
		   (to-int16 s (fx+ 4 ind))
		   (to-int16 s (fx+ 6 ind)))
	   ))
	   
       ;; UNFINISHED: Doing this with different sized grab-chunk and window-size is very annoying:
#;
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

       ;; This version is simpler and just reads at the granularity of
       ;; the window-size.  However it has a CORRECTNESS problem.  It
       ;; depends on block-read getting all the data every read.
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
	     (warning 'read-window "discarding incomplete window of data, probably end of stream.  Got ~a, wanted ~a" count1 winsize)
	     ;(warning 'read-window "this version depends on block-read always getting all the chars, got ~a, wanted ~a"	count1 winsize)
	     ;(printf "Better get eof next... THE PROBLEM OF LEFTOVERS!\n")
	     (ASSERT (eof-object? (block-read infile buffer1 winsize)))
	     #f]
	  [else
	   (let ([win (make-vector len)])
	     (for (i 0 (fx- len 1))
		  (vector-set! win i (read-sample buffer1 (fx* i 8)))
		  ;(vector-set! win i 99)
		  (void)
		  )
	     win)]))

       ;; This returns the stream representing the audio channel (read in from disk):
       ;; TODO: HANDLE OVERLAP:
       (ASSERT (zero? overlap))
       (delay 
	 (let loop ([pos 0]) 
	   (let ([win (read-window)])
	     (if win
		 (let ((newpos (+ len pos)))
		   (stream-cons (make-sigseg pos newpos win 'nulltimebase)
				(loop newpos)))
		 ()))))
       )

     ;; TODO:
     ;(define timer )

     (define nullseg #())
     (define nullarr #())
    
     (define +. fl+)
     (define -. fl-)
     (define *. fl*)
     (define /. fl/)
     (define + fx+)
     (define - fx-)
     (define * fx*)
     (define / fx/)
     
     ;(define (realpart x) (if (cflonum? x) (cfl-real-part x) x))
     ;(define (imagpart x) (if (cflonum? x) (cfl-imag-part x) 0))
     (define realpart cfl-real-part)
     (define imagpart cfl-imag-part)

     (define fft (lambda (v) (list->vector (dft (vector->list v)))))
     
     ; break
     ; error

     (define tuple vector)

     (define newarr   make-vector)
     (define arr-get  vector-ref)
     (define arr-set! vector-set!)
     (define length   vector-length)

     ;; This is a bit silly.  Since we don't use an actual time-series
     ;; implementation, this just makes sure the overlap is EQUAL.
     (define (joinsegs w1 w2)
       (let ([a (sigseg-start w1)]
	     [b (sigseg-end w1)]
	     [x (sigseg-start w2)]
	     [y (sigseg-end w2)])
	 (cond
	  [(not (eq? (sigseg-timebase w1) (sigseg-timebase w2)))
	   `(DifferentTBs)]

	  ;; In this case the head of w2 is lodged in w1:
	  [(and (<= a x) (<= x b))
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
	  [(< b (sub1 x)) '(Gap)]
	  )))

     (define (subseg w start end)
       (if (or (< start (sigseg-start w))
	       (> end (sigseg-end w)))
	   (error 'subseg "cannot take subseg ~a:~a from sigseg ~s" start end w))
       (let ([vec (make-vector (add1 (- end start)))])
	 (for (i 0 (fx- (vector-length vec) 1))
	      (vector-set! vec i (vector-ref (sigseg-vec w) (+ i (- start (sigseg-start w))))))
	 (make-sigseg start end vec (sigseg-timebase w))))

     (define (seg-get w i) (vector-ref (sigseg-vec w) i))
     (define (width w) (vector-length (sigseg-vec w)))
     (define (start w) (vector-length (sigseg-start w)))
     (define (end w) (vector-length (sigseg-end w)))
     (define (seg-timebase w) (vector-length (sigseg-timebase w)))
     (define to_array sigseg-vec)

     ;(define emission (make-parameter '()))
     (define (iterate f s)
       ;(delay
       (let loop ((s s))
	 (if (stream-empty? s) 
	     '()
	     (let ([vals (reverse! (unbox (f (stream-car s))))])
	       (cond
		[(null? vals) (loop (stream-cdr s))]
		[(null? (cdr vals)) (stream-cons (car vals) (loop (stream-cdr s)))]
		[else 
		 (append! (reverse! vals) 
			  (delay (loop (stream-cdr s))))])))))
       
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
     

     ))


;; This uses a convoluted evaluation order.  But it allows us to eval
;; the wavescope defs ONCE at load time, and have them visible to all
;; the unit tests.
(define these-tests
  (eval `(let ()
	   ,(wavescript-language 'return)
	   (list 
	    `["Joinsegs" 
	      (,(lambda ()
		  (reg:struct->list
		   (joinsegs (make-sigseg 10 19 (list->vector (iota 10)) 'foo)
			     (make-sigseg 15 24 (list->vector (map (lambda (x) (+ x 5)) (iota 10))) 'foo)))))
	      ("sigseg" 10 24 #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14) foo)]
	    `["Subseg"
	      (,(lambda ()
		  (reg:struct->list
		   (subseg (make-sigseg 10 19 (list->vector (iota 10)) 'foo) 11 15))))
	      ("sigseg" 11 15 #(1 2 3 4 5) foo)]
	    
	    ))))



(define test-this (default-unit-tester "Wavescript emulation language bindings" these-tests))
(define test-ws test-this)
