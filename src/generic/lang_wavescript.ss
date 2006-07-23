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
  (let loop ((s (audio 0 1000 0)))
    ;(if (= 0 (remainder n 100)) (display #\.))
    ;(display #\.)
    (if (null? s) 'DONE
	(loop (stream-cdr s) ;(add1 n)
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
   (let ((p (open-input-file "/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw"))
	 (s (make-string 32768 #\_)))
     (time 
      (let loop ()
	(let ((n (#3%block-read p s 32768)))
	  ;(printf "~a " n)
	  (if (#3%eof-object? n)
	      'alldone ;(printf "done: ~a\n" s)
	      (loop)))))
     (close-input-port p)))


(define-language
  'wavescript-language
  '(begin

     ;; Contains a start and end SEQUENCE NUMBER as well as a vector.
     (reg:define-struct (sigseg start end vec timebase))
     ;(define-record timeseries (timebase))

     ;; This is a hack to load specific audio files:
     (define (audio chan len overlap)
       (letcc exit
       (define infile (open-input-file "/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw"))
       ;(define buffer (make-string 32768 #\_))
       (define (read-int16)
	 (let* ([a (read-char infile)]
		[b (read-char infile)])
	   (if (or (eof-object? a) (eof-object? a))
	       (exit '()))
	   (let ([unsigned (fx+ (fx* 256 (char->integer a))
				(char->integer b))])
	     (if (fx< unsigned 32768)
		 unsigned
		 (fx- unsigned 65536)))))
       (define (read-sample )
	 (let* ([c1 (read-int16)]
		[c2 (read-int16)]
		[c3 (read-int16)]
		[c4 (read-int16)])
	   (vector c1 c2 c3 c4)))
       (define (read-window)
	 (let ([win (make-vector len)])
	   (for i = 0 to (fx- len 1)
		(vector-set! win i (read-sample)))
	   win))

       ;; TODO: HANDLE OVERLAP:
       (ASSERT (zero? overlap))
       (delay 
	 (let loop () 
	   (let ([win (read-window)])
	     (if win
		 (stream-cons win (loop))
		 ()))))
       ))

     ;; TODO:
     ;(define timer )

     (define nullseg #())
     (define nullarr #())

     (define fft (lambda (v) (list->vector (dft (vector->list v)))))
     
     ; break
     ; emit
     ; error

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
	     (for i = a to (max b y)
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
	 (for i = 0 to (fx- (vector-length vec) 1)
	      (vector-set! vec i (vector-ref (sigseg-vec w) (+ i (- start (sigseg-start w))))))
	 (make-sigseg start end vec (sigseg-timebase w))))

     (define (seg-get w i) (vector-ref (sigseg-vec w) i))
     (define (width w) (vector-length (sigseg-vec w)))
     (define (start w) (vector-length (sigseg-start w)))
     (define (end w) (vector-length (sigseg-end w)))
     (define (seg-timebase w) (vector-length (sigseg-timebase w)))
     (define to_array sigseg-vec)

     (define iterate stream-map)
     (define (deep-iterate f s)
       (stream-map 
;	(let ([start 0] ;; Record the range covered thusfar.
;	      [end 0])
	  (lambda (w)
	    (make-sigseg (sigseg-start w)
			 (sigseg-end w)
			 (vector-map f (sigseg-vec w))
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
