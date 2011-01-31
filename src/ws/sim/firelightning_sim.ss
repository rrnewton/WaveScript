
;;;; TODO: SHOULD THE SENSOR BE DETERMINISTIC?  IN THAT IF ITS READ TWICE IN THE SAME TIME-STEP, SAME ANSWER??

;;;; .title A Simple Lightning/Forest-fire Sim.
;;;; .author Ryan Newton

;;;; [2006.02.01] <br> 
;;;; Matt and discussed a simple lightning/forest-fire simulation that
;;;; would stress Regiment and provide for some interesting programs.


;;; Constants:  TODO: FIX THESE UP, MAKE THEM MORE REAL:
	;; Need to make this much more sophisticated.
;(define lightning-rate .00001) ;; Probability per millisecond of lightning.
(define lightning-rate .00003) ;; Probability per millisecond of lightning.
	;[lightning-rate 0.5] ;; Probability per millisecond of lightning.
(define fire-spread-rate .001) ;; Again, per millisecond.
(define fire-width 3000) ;; The fire is a ring 500m thick.  It "burns out" in the center.

;(define fire-max-age 220000) ;; Total life in milleseconds.
;(define fire-max-age 180000) ;; Total life in milleseconds.
(define fire-max-age 320000) ;; Total life in milleseconds.
;(define fire-max-age 500000) ;; Total life in milleseconds. <-- Sensys paper.

(define fire-temp 200)  ;; Degrees in celcius.

(define heat-noise-magnitude 3)

;----------------------------------------------------------------------
;; This function maintains the global lightning state and also reads
;; off sensor values.  <br><br>
;; 
;; It's a curried function whose outmost layer is called only to
;; produce a new world.  The second layer is invoked every time the
;; time changes so as to maintain the simulation state.  (However,
;; this is optional, it could very well only update when a sensor
;; reading is requested.)  The time-step must be
;; greater-than-or-equal-to the one inputted last.  However they
;; needn't be consecutive. <br><br>
;; 
;; The inner lambda is only called when the simulator needs to read an
;; actual sensor value.  That is the type of this function is
;; something like the following (where effects are written in the arrows): <br>
;;
;; ()   --{Newsim}--> 
;; Time --{UpdateState}--> 
;; (type, id, x, y)  ----> 
;;  SensorReading
(define (firelightning-sensor)

  ;; This is the statically allocated state used by the lightning sim.
  ;; Could refactor this to encapsulate it later:
  (let ([last-time #f]

	;; A HACK:
	[constant_strikes CONSTANT_STRIKES]

	;;; Other state....
	;; Current set of fire objects.
	[fires '()]
	)
    (reg:define-struct (fire x y t rad gobj))
;    (define exp_const (exp (- lightning-rate)))
;    (define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))

    (printf "Constructing fire sim...\n")

    (lambda (t)
      (define worldx (simalpha-world-xbound))
      (define worldy (simalpha-world-ybound))
      (define quartx (/ (simalpha-world-xbound) 4))
      (define quarty (/ (simalpha-world-ybound) 4))

      (if (not last-time) (set! last-time t))
      (if (< t last-time)
	  (error 'firelightning-sensor
		 "can't go backwards in time from t=~a to t=~a." last-time t))
      (if (= t last-time)
	  (void)
	  (let ([delta (- t last-time)])
	    ;(printf " delt:~a\n" delta)
	    ;; Update State...
	    ;; Propogate existing fires, while filtering dead fires.
	    (set! fires
		  (filter (lambda (f)
			(if (> (- t (fire-t f)) fire-max-age)
			    ;; This fires dead!
			    (begin 
			      ;; Delete just the mark and keep the circle:
			      (IF_GRAPHICS (delete-gobj (cdr (fire-gobj f))))
			      ;(delete-gobj ((car (fire-gobj f)) 0)) ;; This would delete the circle also.
			      #f)
			    (begin 
			      (set-fire-rad! f (+ (fire-rad f) (* delta fire-spread-rate)))
			      (IF_GRAPHICS ((car (fire-gobj f)) (fire-rad f))) ;; Update the circle on the screen.
			      #t)))
		    fires))
	    (unless (null? fires)
	      (logger 0 t '_ 'GROUND-TRUTH 
		      `[fires ,(map (lambda (f) (list (fire-x f) (fire-y f) (fire-t f) (fire-rad f))) fires)]))

	    ;(if (not (null? fires)) (printf "    Number Fires: ~a\n" (length fires)))

	    ;; TEMP: FIXME:
	    ;; TEMPORARILY DOING MY CONSTANT SCHEDULE HACK
	    ;; Currently making it so there's one fire at a time for easy analysis.
	    (when (null? fires)
	    ;; See if there are new lightning strikes.
	      (for-each (lambda (strike-time)
			;; [2006.02.19] Restricting the fires to the inner portion of the square.  
			;; Can't have them falling outside of the network.			


			  ;; SUPER HACKISH:
			  ;; I just set the RNG here to get a fixed strike-x/strike-y.
			  (parameterize ((random-seed strike-time))

			    ;; [2006.04.05] HACK: FIXME
			    ;; PUTTING THEM IN THE CENTER OF THE NETWORK:
			(let (;[strike-x 2600];(/ worldx 2)]  ;(+ quartx (random (/ worldx 2)))]
			      ;[strike-y 2400];(/ worldy 2)]) ;(+ quarty (random (/ worldy 2)))]
			      [strike-x (if (= worldx 5000)
					    2420
					    (/ worldx 2)
					    )]
			      [strike-y (if (= worldy 5000)
					    2420
					    (/ worldy 2))]
			      )
			  ;(printf "  LIGHTNING!! ~a of ~a ~a\n" i numstrikes strike-time)
			  (let ([newfire (make-fire strike-x strike-y strike-time 0  ;; Initial radius zero
						    ;; The graphical object for this fire is a strange thing.  
						    ;; Consing the center mark and the adjustable circle together.
						    (IF_GRAPHICS (cons (draw-circle `(,strike-x ,strike-y) 0)
								       (draw-mark (list strike-x strike-y)))
								 '()))]
				[mark (IF_GRAPHICS  '())])
			    (printf " New fire!: ~a\n" (list (fire-x newfire) (fire-y newfire) (fire-t newfire)))
			    (set! fires (cons newfire fires))
			    ))))
	      (let ([strikes 
		     ;;;(poisson-span last-time t lightning-rate)
		     ;;;(fake-poisson last-time t lightning-rate)
		     ;(poisson-span-optimized1 last-time t lightning-rate)		     

		     ;; FIXME FIXME FIXME
		     ;; THIS HACK FORCES A CONSTANT SCHEDULE.
		     (HACK "Fix the fire schedule to be constant across runs."
		      (let loop ()
					;(printf "HEAD: ~a, last ~a cur ~a \n" (car constant_strikes) last-time t)
			(cond
			 [(null? constant_strikes)
			  (error 'firelightning-sensor "this hack wasn't done right")]
			 [(< (car constant_strikes) last-time)
			  ;; We've already missed this one, scroll forward.
			  (set! constant_strikes (cdr constant_strikes))
			  (loop)]
			 [(< (car constant_strikes) t)
			  (let ([x (car constant_strikes)])
			    (set! constant_strikes (cdr constant_strikes))
			    (cons x (loop)))]
			 [else ()])))
		     ])
		;; 
		;; It's annoying for me to run simulations and wait for a strike, so...
		;(HACK "If we haven't had any strikes by 10 seconds, just throw one in."
		;(if (and (>= t 10000) (null? fires) (null? strikes)) (list t) strikes))
		(if (null? strikes) ()
		    (HACK "Since we're only allowing one fire at a time, take only the first in this span"
			  (list (apply min strikes))))
		))
	    )
	    (set! last-time t)
	    ))
      ;; Done updating state, now create a function for reading sensor values:

      ; ========================================
      ;; ACTUAL SENSING KERNEL:  Supports 'temp and 'light sensors.
      (lambda (type id x y)
	(case type
	  ;; Default sensor is temp:
	  [(temp default)
	   (let ((temp 0))
	     (for-each (lambda (f)
			 ;; Measure distance from us to the center of the fire.
			 (let ((dist (sqrt (+ (^ (- x (fire-x f)) 2)
					      (^ (- y (fire-y f)) 2)))))
			   (set! temp
				 ;; [2006.02.12] Changing this so the fire-temp is a ceiling:
				 (min fire-temp
				      (+ temp (compute-fire-heat dist (fire-rad f)))))))
	       fires)
	     ;; Our sensor has constant gaussian noise attached to it, stddev 5 degrees:
	     (inexact->exact (floor (+ temp (* heat-noise-magnitude (gaussian)))))
	     ;; TEMP: using integer temperature:
	     ;(inexact->exact (floor temp))
	     )]
	  [(light) 9999] ;; TODO
	  [else (error 'firelightning-sensor
		       "unsupported sensor type: ~s" type)])))))


(define (compute-fire-heat dist radius)
  (let ([dist (exact->inexact dist)]
	[radius (exact->inexact radius)])
  ;; This function determines the rate that heat falls off
  ;; with distance from a fire of a given radius.          <br>
					;(sim-setlabel (format "~a" dist))
  ;; Are we within the circle?
  (if (< dist radius)
      ;; Are we within the "eye", however?
      (if (> (- radius fire-width) dist)
	  0 ;; It's perfectly cool once it's burned out.  A little silly.
	  fire-temp)
      ;; If we're outside the fire, compute our heat reading as follows.
      (if (zero? radius) 0
	  (* fire-temp
	     ;; Treat the fire as a spherical shell, thin the radiated energy
	     ;; over the larger spherical shell.      
	     ;  (fl/ (fl* radius radius)  (fl* dist dist))

	     ;; This one is just falls off as a line.
;	     (let ([dist-from-edge (- dist radius)])
;	       (if (fl> dist-from-edge 200.) 0.
;		   (fl- 1.0 (fl/ dist-from-edge 200.))))


	     ;; No, let's try an exponential decay instead:
	     ;  (^ .95 (- dist radius))

	     ;; Nah, let's actually make it a constant "halo" area.
	     ;(min 1.0 (fl/ 100.  (+ 100. (^ (- dist radius) 2))))

	     ;; Ok, also trying just a 1/n falloff:
	     (if (> (- dist radius) 350) 0.
		 (min 1.0 (fl/ 1.  (+ 1.0 (fl* 0.5 (- dist radius))))))

	     ;; This drops off, but with a different coefficient.
;	     (if (> (- dist radius) 350) 0.
;		 (min 1.0 (fl/ 1.  (+ 1.0 (fl* 0.25 (- dist radius))))))
	     
	     )
	  ))))

;; This only works if the base-rate is small.
(define (fake-poisson start end base-rate)
  (let ([acc '()])
    (for t = start to end
	 (when (< (random 1.0) base-rate)
	   (set! acc (cons t acc))))
    (reverse! acc)))

;----------------------------------------------------------------------
;; Poisson processes: but can be very inefficient for large spans.
;; And worse, this implementation can have failures of numeric
;; accuracy as well when the timespan gets large.
;; 
;; .param start - Start of spam for which events are generated.
;; .param end - End of time span.
;; .param base-rate - Average rate of events per time unit.
;; .returns A list of event times.
(define poisson-span 
  (letrec  ([fact (lambda (n) (if (zero? n) 1 (* n (fact (sub1 n)))))])
    (lambda (start end base-rate)
      (let* ([delta (exact->inexact (- end start))]
	     [mu (* delta base-rate)]
	     [exp_const (exp (- mu))])
	;; We iteratively roll the chances of their being 0, 1, 2... events over the elapsed interval.
	(let loop ([numstrikes 0] [prob exp_const] [probsum exp_const])
	  ;(printf "PROB: ~a sum:~a iter:~a const:~a delta:~a mu:~a\n" prob probsum numstrikes exp_const  delta mu)
	  (if (< (random 1.0) probsum) ;; If we fall under the accumulated probability mass...
	      ;; We've got the correct number of strikes, return a list of event times:
	      (let ((ls '()))
		(for i = 1 to numstrikes 
		     (let ([event-time (+ (random delta) start)])
		       (set! ls (cons event-time ls))))
		ls)
	      ;; Otherwise, we failed, move on to the next scenario:
	      (let* ([newnum (fx+ 1 numstrikes)]
		     [newprob 
		      ;;(* prob (fl+ 1.0 (/ mu newnum)))
		      ;;(* prob (/ (+ mu numstrikes 1) (add1 numstrikes)))
		      (/ (* (^ mu (add1 numstrikes)) exp_const) (fact (add1 numstrikes)))
		      ])
					;(loop newnum newprob (+ probsum newprob))
		(loop newnum 0.0 (* prob (fl+ 1.0 (/ mu newnum))))
		)
	      ))))))


;; Poisson processes: with reasonable efficiency
(define poisson-span-optimized1
  ;; This constant sets the maximum time-span we'll tolerate.  If the
  ;; span is bigger than this we do a divide-and-conquer.
  (letrec ([max_span_size 100]
	   [fact (lambda (n) (if (zero? n) 1 (* n (fact (sub1 n)))))])
    (lambda (start end base-rate)      
      (let divide-and-conquer ([start start] [end end])       
	(define delta (- end start))	
	(if (> delta max_span_size)
	    ;; Here we recursively split up the time-span into something bite-sized:
	    (let ([mid (+ (/ delta 2) start)])
	      (append  (divide-and-conquer start mid)
		       (divide-and-conquer mid end)))
	    ;; If it's small enough, we then roll our numbers and produce some events:
	    (poisson-span start end base-rate))))))

;----------------------------------------------------------------------
;; Install the lightning sim as the default sim.  <br>
;; As well as plugging in the correct sensing stub, this will set the
;; dimensions of the world and the radio parameters correctly.
(define (install-firelightning)
  (simalpha-sense-function-constructor firelightning-sensor)

#;
  (begin 
    ;; Set the world size, square:
    ;; Set both of these binds for now, lame:
    (set! world-xbound 5000)
    (set! world-ybound 5000)
    (simalpha-world-xbound 5000)
    (simalpha-world-ybound 5000)
    (sim-num-nodes 250))

  (begin 
    (set! world-xbound 1700)
    (set! world-ybound 1700)
    (simalpha-world-xbound 1700)
    (simalpha-world-ybound 1700)
    (sim-num-nodes 30))

  ;; Inner/Outer Radius for radios is 300/500 meters:
;  (simalpha-inner-radius 300)
;  (simalpha-outer-radius 500)

#;
  (simalpha-inner-radius 
   (begin 
     (fprintf (console-output-port) "SETTING RADIUS FROM COMM_DIAMETER\n")
     (string->number (getenv "COMM_DIAMETER"))))
;  (simalpha-outer-radius (flonum->fixnum (* (/ 5 3.) (simalpha-inner-radius))))

  (simalpha-channel-model 'linear-disc)
  (simalpha-placement-type 'gridlike)
  (simalpha-failure-model 'none)

;; This should have been parameterized here before:
  (etx-retry-delay 50)
  ;; And this is the maximum number of times a retry will be made.
  (etx-max-retries 3)

  )


;================================================================================

;;; Test fire drop-off.  Graphs a histogram.

;; This is just a visual test.  Check by eye that it looks like what you want.
(define (test-fire-heat)
  ;; Currying:
  (define (: f . args) (lambda (x) (apply f (append args (list x)))))
  
  ; First the drop off from a small fire, rad 10.
  (gnuplot (map (lambda (i) (list i (compute-fire-heat i 10))) (map (: * 3) (iota 100))) 'boxes)
  ; Next, rad 100
  (gnuplot (map (lambda (i) (list i (compute-fire-heat i 100))) (map (: * 2) (iota 500))) 'boxes)
  ; Then rad 1000
  (gnuplot (map (lambda (i) (list i (compute-fire-heat i 1000))) (map (: * 5) (iota 1000))) 'boxes)
  )


;; [2006.03.02] Hack, just allocating this fixed schedule for now.
;; [2006.04.05] Hacking 

(define CONSTANT_STRIKES
  '(
    1000000 
    2000000
    3000000
    4000000
    5000000
    6000000
    7000000
    8000000
    9000000
    10000000
    11000000
    12000000
    ))
#;
(define CONSTANT_STRIKES
'(10243 21589 43736 58078 114326 149726 181964 215430 256309
 281683 424553 468013 481352 500276 500533 514676 545114
 546127 560210 607527 611534 624541 641814 669163 719490
 788765 808463 845110 854322 867473 897220 915947 934427
 981299 998257 1002443 1039920 1042044 1066391 1081490
 1084986 1097524 1099763 1130223 1170031 1236174 1242993
 1387041 1408903 1427235 1437664 1444487 1472415 1472948
 1473145 1501231 1525413 1587115 1633010 1648112 1762126
 1769584 1781246 1786414 1790808 1834045 1845595 1856508
 1922452 1961071 1981169 2016990 2040264 2046295 2077565
 2093759 2096759 2111112 2196341 2204167 2234650 2311744
 2355096 2388894 2444415 2506783 2585182 2597611 2617856
 2798343 2827389 2895639 2981823 2983808 3026997 3056677
 3074133 3103145 3228117 3380144 3402079 3406309 3458117
 3491445 3555025 3607994 3637669 3701183 3735376 3764814
 3804907 3862103 4075528 4100766 4119216 4166479 4169274
 4184393 4203783 4244897 4302173 4540391 4572117 4582902
 4610467 4723500 4725215 4757673 4814242 4829018 4838314
 4856519 4869420 4899623 4992730 4993645 5160179 5263726
 5298925 5329169 5354155 5380597 5474680 5527809 5536476
 5572147 5604983 5638711 5690055 5701255 5744091 5749880
 5791720 5791896 5851115 5953696 5958460 6015047 6068127
 6122466 6128577 6167959 6234022 6285562 6325316 6391410
 6480227 6483890 6490352 6539687 6561783 6711597 6719889
 6752277 6764998 6769343 6784232 6815698 6883901 6917939
 6965389 6969720 6994131 7007440 7034607 7061422 7068254
 7091475 7134628 7134772 7180088 7184776 7188788 7207563
 7244056 7282890 7286774 7292248 7344835 7348482 7352932
 7363218 7407166 7412423 7460350 7477789 7510935 7527466
 7545997 7619558 7650908 7766420 7779128 7812482 7823410
 7833786 7847563 7919837 7940170 7984288 8029478 8036005
 8065880 8104842 8119428 8198857 8278670 8334472 8340563
 8354401 8373002 8380948 8384248 8468284 8518906 8604727
 8638920 8648586 8660070 8753234 8773450 8953852 8985285
 8999054 9007501 9013109 9047381 9074803 9104999 9109841
 9139759 9206180 9282990 9312919 9329344 9356430 9399601
 9410195 9495910 9515515 9515692 9530222 9557604 9597575
 9630666 9689146 9724251 9762327 9826946 9910727 9931115
 10031363 10062127 10128052 10162303 10174992 10199396
 10224249 10246856 10250339 10252283 10281763 10332332
 10345783 10387537 10399596 10402789 10464320 10479136
 10524152 10579311 10584780 10594129 10667064 10722029
 10729916 10761997 10765568 10833305 10913976 10993205
 11007014 11013681 11035686 11062806 11111250 11115867
 11259001 11260090 11324199 11372345 11382682 11438677
 11440048 11487758 11490475 11493471 11502260 11567750
 11582051 11617496 11621166 11679512 11762305 11763980
 11777981 11811275 11811625 11852161 11860656 11944197
 11969018 12041117 12065588 12083082 12163340 12165501
 12216155 12231584 12261044 12302816 12321327 12325258
 12384323 12431951 12467779 12474091 12494543 12605875
 12624193 12671262 12671462 12680093 12699098 12704777
 12737720 12778455 12796185 12800337 12832292 12884444
 12924388 12944496 13004856 13009570 13041080 13058097
 13058246 13079742 13097350 13121070 13173803 13180670
 13210875 13216595 13230997 13238076 13280879 13298377
 13306454 13379759 13399083 13426447 13443660 13605720
 13608467 13665932 13665963 13668704 13727422 13730093
 13763011 13771637 13818279 13825022 13829705 13885141
 13893464 13967028 13969436 14019427 14029583 14090150
 14127580 14151496 14200503 14201066 14386126 14479905
 14489060 14575356 14594080 14606259 14620071 14627724
 14673556 14733326 14734024 14778023 14823973 14838736
 14949565 14960853 15058144 15072502 15090627 15111900
 15141007 15185667 15187689 15247300 15252868 15362695
 15368485 15371824 15428294 15434951 15470578 15479244
 15533089 15556096 15558154 15595587 15613892 15627579
 15676715 15682689 15689790 15705090 15707108 15734006
 15770199 15772432 15776249 15795177 15801277 15810616
 15851874 15865470 15938681 16047252 16049869 16072165
 16086872 16172902 16181576 16312398 16320969 16324549
 16367409 16446099 16466394 16472040 16486714 16521665
 16539973 16586241 16681260 16801332 16806295 16877099
 16910103 16916013 16947650 16948702 16972163 17057926
 17080844 17106870 17113821 17174971 17215455 17218435
 17230591 17233370 17242200 17281388 17308567 17433065
 17461716 17471562 17484729 17490486 17512792 17532131
 17561001 17574902 17639627 17651589 17676832 17679787
 17704347 17719705 17727701 17728114 17756852 17788544
 17797881 17803061 17803289 17844066 17856550 17879592
 17899335 17902220 17902725 17934289 18014332 18078058
 18083535 18107345 18263695 18276743 18308815 18349524
 18360671 18443177 18483885 18588799 18635726 18636417
 18643771 18665503 18690857 18714105 18798284 18847558
 18886482 18992928 19025509 19027221 19079340 19085706
 19140123 19156772 19178856 19195991 19199172 19207413
 19290820 19308139 19316620 19321218 19365066 19409791
 19417984 19425637 19485407 19576570 19609228 19632722
 19696641 19710885 19768170 19815128 19883520 19931960
 19963875 20013093 20026359 20073327 20114695 20118787
 20126773 20192136 20215691 20303392 20311520 20350665
 20357311 20357482 20434232 20451556 20458577 20487348
 20498608 20509947 20544308 20586497 20641133 20643551
 20717431 20750987 20752470 20799240 20800561 20802843
 20810130 20821492 20832041 20833691 20854363 20939461
 20977495 21018420 21033626 21044176 21060027 21077742
 21084654 21088991 21191588 21229283 21234919 21383647
 21390676 21391626 21427763 21436688 21471047 21471851
 21480153 21498218 21508243 21529907 21721264 21785077
 21816745 21824260 21855476 21884297 21897995 21926568
 21947820 21965847 21984886 22003195 22009414 22027552
 22042438 22093829 22100842 22106335 22120771 22122356
 22142442 22201917 22204667 22266089 22269980 22370471
 22391943 22430985 22435889 22448980 22451287 22617824
 22668770 22674415 22679189 22706384 22759241 22801606
 22810740 22853250 22866523 22903865 22951804 23021005
 23042510 23043780 23104565 23123575 23155607 23166049
 23166782 23211598 23215866 23236772 23281364 23288165
 23308779 23314160 23349833 23367830 23396607 23453788
 23562541 23564675 23577672 23577760 23579723 23596761
 23597053 23605678 23612086 23623648 23673894 23743835
 23798958 23817323 23859294 23873811 23906748 23910011
 23937577 23950107 24043520 24071973 24255268 24269068
 24281273 24340247 24376822 24400347 24400448 24403950
 24427825 24455890 24502418 24527022 24544950 24575795
 24589099 24663175 24708282 24730063 24731687 24753546
 24753574 24761368 24764474 24775628 24828133 24840388
 24857321 24861987 24908973 24947493 24969154 24983798
 25024584 25033593 25042588 25048855 25057204 25091001
 25101137 25135147 25148442 25155867 25179098 25188152
 25191064 25198345 25258258 25277664 25286628 25297818
 25332707 25372885 25374306 25429242 25503817 25570388
 25605854 25625258 25658631 25698918 25711978 25734198
 25751916 25831035 25832169 25849554 25861520 25958018
 26102012 26132058 26193131 26241921 26278827 26300275
 26334679 26337264 26411011 26442735 26461213 26475757
 26497934 26504713 26570532 26585431 26656586 26664466
 26684181 26684553 26701348 26794698 26798334 26810543
 26860790 26964623 26999664 27016878 27106936 27108972
 27146065 27150413 27177826 27185967 27196963 27216211
 27230843 27237554 27267911 27276879 27303322 27348258
 27349525 27351505 27373448 27396470 27461066 27474714
 27479425 27489853 27534170 27554067 27568049 27635913
 27676579 27708491 27722094 27750948 27824388 27838723
 27851561 27873507 27896801 27928217 27932830 27936400
 28048477 28051403 28076931 28106214 28113934 28121111
 28175023 28201296 28206358 28269018 28285742 28330230
 28332632 28358755 28366759 28368196 28425494 28461298
 28510041 28558481 28668276 28694973 28754253 28772645
 28785374 28831018 28833623 28842815 28886853 28992289
 29016973 29018734 29180932 29207535 29227309 29238589
 29305102 29313066 29315915 29368675 29382412 29407329
 29431606 29472567 29596499 29599142 29659942 29671568
 29697688 29704889 29726955 29760466 29823006 29839843
 29897493 29936847 29947519 30001687 30017008 30174081
 30236640 30242660 30277583 30302871 30351630 30358708
 30421327 30475797 30515077 30584987 30760915 30765458
 30791324 30805040 30825465 30858451 30899603 31054291
 31066448 31085617 31099795 31103619 31123345 31154686
 31156515 31210701 31261703 31282695 31285877 31290058
 31440133 31448252 31464697 31533370 31622045 31640446
 31642258 31650301 31664505 31687325 31767139 31777410
 31834846 31930815 32019479 32026306 32032782 32042765
 32069666 32095702 32135039 32194018 32207815 32225348
 32235757 32244426 32282700 32301814 32321800 32380569
 32388910 32410292 32460447 32506158 32553080 32575959
 32579909 32599278 32661597 32696548 32708756 32716640
 32745757 32928534 32961321 32966777 33045929 33094538
 33171073 33214174 33267644 33272587 33356743 33382172
 33408840 33453579 33464892 33496274 33524139 33568344
 33588069 33640616 33697047 33711396 33789856 33809116
 33874511 33885840 33889840 33899188 33933477 33945847
 33998739 34028321 34044648 34053302 34056558 34209035
 34220841 34231460 34339540 34364694 34369017 34398861
 34402662 34445983 34529539 34599651 34602915 34640328
 34783359 34789774 35008513 35014856 35023431 35033491
 35052986 35053890 35093022 35107967 35125934 35154543
 35158179 35172207 35183580 35191923 35228165 35278072
 35292113 35327661 35418014 35440992 35469118 35545083
 35556730 35599763 35620830 35627454 35645153 35674332
 35761005 35780427 35803450 35838162 35918397 35924654
 35935706 35949147 35950763 35953278 35968426 35999675
 36013816 36036340 36044234 36052391 36140574 36216150
 36219287 36225178 36302814 36310915 36315460 36339254
 36392453 36428230 36434489 36458911 36471874 36532456
 36534382 36584765 36673741 36697901 36765738 36783648
 36841327 37047134 37054583 37069955 37070170 37073264
 37147065 37195213 37229375 37276106 37280991 37343732
 37412757 37461906 37539289 37595407 37612583 37725276
 37766034 37823051 37862341 37876968 37905795 38026197
 38071959 38100755 38160894 38184569 38195431 38337837
 38355128 38435029 38495646 38509996 38528146 38528448
 38597499 38621387 38647769 38697895 38719755 38742215
 38788148 38807109 38828885 38929107 38935018 38949194
 38951496 38972768 39002024 39064730 39124636 39131808
 39174664 39183566 39206253 39216219 39223624 39242328
 39286649 39299629 39317671 39323548 39369136 39405248
 39460490 39554889 39614893 39615795 39652585 39705578
 39768410 39769152 39777698 39806561 39815231 39854408
 39864149 39871046 39878858 39879307 39932749 39941850
 39956851 39992752 40047997 40048318 40100061 40101240
 40168912 40181351 40194487 40268267 40348066 40384673
 40394414 40414087 40468966 40481852 40506876 40511098
 40512172 40553464 40573570 40612945 40629123 40709329
 40716244 40774245 40796955 40846525 40868184 40954555
 40966025 40998507 41055462 41076934 41114391 41117385
 41125689 41129591 41131733 41156563 41178189 41201059
 41261943 41315268 41318668 41334471 41445427 41452496
 41455152 41497803 41617159 41624112 41668854 41704001
 41736243 41761721 41805334 41817234 41916531 41946618
 41960698 42028276 42042452 42078759 42091011 42094844
 42111394 42128177 42177385 42195641 42221093 42240489
 42249849 42293793 42339832 42380181 42400687 42511867
 42555783 42559945 42619132 42685604 42751213 42756792
 42835687 42883938 42912341 42937008 42973304 43028037
 43042166 43086210 43162497 43165800 43198905 43279166
 43293587 43334908 43341266 43356290 43356970 43428438
 43559391 43600194 43697284 43702602 43766758 43776524
 43786773 43827414 43858043 43866086 43873434 43895204
 43920270 44060088 44116501 44165391 44171622 44173376
 44207797 44242001 44255266 44297543 44313352 44316499
 44366486 44539285 44552073 44603637 44616153 44637810
 44697518 44748036 44802818 44822777 44832169 44837076
 44865456 44866646 44875074 44958786 44960890 44968104
 44968504 44990984 45013157 45019887 45037718 45094502
 45159731 45168228 45210841 45214643 45226349 45226752
 45294599 45363620 45393842 45406825 45415050 45424447
 45435525 45451908 45492454 45505338 45544264 45571996
 45609729 45636283 45654054 45666359 45754360 45764047
 45768818 45776833 45816368 45842264 45873342 45983468
 46000802 46050363 46062336 46084228 46114599 46123911
 46214852 46246168 46260232 46274173 46330708 46362417
 46449781 46471064 46476068 46535879 46560837 46626356
 46692497 46694385 46703566 46749749 46756948 46776863
 46804340 46814355 46818536 46865032 46914403 46932495
 46938593 46947549 46953113 46958906 46982212 47044842
 47082744 47168209 47218673 47254600 47339603 47352570
 47353146 47427113 47435357 47481163 47505128 47512302
 47549144 47600382 47618123 47622268 47655702 47700908
 47723135 47838493 47880393 47902101 47902391 47920606
 47930912 47932221 47938311 47957395 47966196 47967865
 48011272 48077199 48079654 48146564 48149746 48151292
 48240175 48290435 48338209 48346096 48371514 48408673
 48462824 48501167 48562341 48576535 48586554 48631238
 48673928 48686461 48701254 48760870 48783144 48827257
 48897527 48945166 48975570 48976868 48983689 48997746
 49060846 49061900 49070373 49083869 49101965 49110030
 49116579 49182785 49195401 49264522 49264961 49326025
 49378470 49391679 49394687 49396781 49402444 49431108
 49433534 49451191 49469531 49480089 49492611 49514629
 49527029 49576890 49596248 49646384 49691770 49695052
 49711902 49781631 49804362 49889825 49911229 49915390
 50000673 50005810 50059073 50086242 50159622 50208237
 50232286 50244606 50247816 50247861 50253792 50280727
 50281095 50309974 50313179 50334807 50349577 50350010
 50356718 50436833 50466105 50566884 50576323 50608915
 50611715 50693802 50768048 50876109 50877046 50888148
 50940484 50998567 50998689 51019920 51154100 51186965
 51194900 51198363 51206110 51225462 51264312 51271650
 51285363 51299578 51308809 51322199 51384404 51409216
 51442671 51474068 51514229 51619919 51719343 51726737
 51738928 51765475 51781302 51798671 51807496 51810975
 51817295 51974910 51980841 51994965 51997658 52006579
 52032259 52041483 52049228 52057584 52098485 52112423
 52162071 52205965 52224023 52239377 52241905 52257157
 52266851 52318151 52336609 52352874 52388285 52402021
 52450672 52507963 52523420 52529165 52572269 52659388
 52670751 52716393 52769009 52803386 52841701 52898762
 52924757 52946915 52970163 53040086 53060744 53081025
 53140809 53248287 53260507 53284292 53359267 53369199
 53400906 53537583 53539130 53559479 53588924 53592482
 53667447 53686626 53700305 53718499 53784034 53890368
 53912047 53993699 54002568 54017874 54084341 54138047
 54170361 54266897 54284697 54324990 54332494 54356369
 54391835 54398955 54452535 54454826 54528181 54529964
 54530766 54643410 54663835 54665829 54678707 54687424
 54727106 54805657 54927642 54941375 55008220 55047788
 55112682 55169993 55280736 55334367 55355565 55444871
 55448285 55512392 55520534 55551421 55557092 55742850
 55828680 55843946 55848278 55873640 55930516 56043509
 56079541 56082297 56117607 56188774 56204870 56210989
 56267184 56274653 56404653 56416915 56459362 56500285
 56556111 56586335 56696281 56702599 56744188 56790965
 56893360 56913733 56921836 56934677 56942350 56970597
 57006669 57033910 57068400 57070012 57082840 57116328
 57151817 57237054 57248616 57273901 57277406 57327537
 57368238 57410188 57439350 57451039 57453698 57494959
 57496887 57522548 57541080 57553698 57554363 57570433
 57680081 57701856 57828516 57913704 57922997 57942754
 57947540 57968737 57980225 58000604 58007194 58076662
 58135547 58168545 58173993 58195956 58226399 58235524
 58289543 58317717 58326109 58355378 58388542 58388964
 58438393 58441627 58457069 58460774 58477082 58483025
 58523188 58597265 58738723 58780542 58803317 58815277
 58823326 58834346 58835212 58935065 59099584 59120259
 59132006 59158926 59204199 59267881 59316486 59331629
 59345505 59413155 59435005 59485625 59486851 59604413
 59622542 59680255 59709537 59734987 59746688 59756207
 59763267 59774934 59810786 59826134 59861957 59896658
 59951246 59981939 59996790 60032821 60106542 60138791
 60227422 60245018 60262017 60264582 60350708 60367755
 60370648 60389955 60486928 60503547 60504602 60518367
 60534926 60603684 60713814 60745347 60801813 60824563
 60832918 60942140 61060716 61074065 61127376 61149704
 61171007 61202431 61219059 61236418 61261737 61303377
 61305226 61311368 61314053 61347755 61351812 61363891
 61408469 61437605 61461026 61464714 61508804 61519546
 61539774 61559136 61569761 61627926 61659905 61671466
 61678490 61684270 61688477 61722575 61784574 61862058
 61890652 61894817 61940394 61953027 62008642 62141323
 62155471 62190628 62283349 62284744 62287214 62324279
 62327353 62350916 62375695 62425960 62429904 62461532
 62466890 62504524 62511589 62545395 62574575 62673557
 62680707 62691932 62731356 62820568 62896638 62899367
 62904085 63018981 63030895 63064072 63150443 63157780
 63177637 63249488 63254199 63311885 63319509 63387905
 63418991 63424354 63437621 63455687 63461561 63471178
 63485491 63518961 63558671 63584486 63587564 63668842
 63747145 63752320 63779729 63792044 63918105 64065253
 64107789 64237826 64270797 64287045 64301606 64402936
 64474534 64482235 64496228 64574815 64575628 64584967
 64601730 64696731 64716217 64721592 64738975 64786221
 64849850 64895274 64904550 64911710 64962842 65006237
 65015954 65102485 65199699 65204331 65286560 65297140
 65324228 65351458 65367492 65376154 65376485 65386548
 65422326 65437421 65442015 65451892 65528181 65614614
 65690501 65720814 65732994 65787985 65812232 65842844
 65876705 65905569 65931126 65934270 65964284 66040171
 66133718 66137120 66186545 66206774 66244400 66249997
 66296002 66297909 66300421 66345121 66365258 66373423
 66402147 66407104 66421164 66445415 66448583 66482766
 66512808 66558848 66561729 66579006 66587406 66653253
 66719982 66738058 66750399 66855209 66868345 66893432
 66897449 66937310 67020135 67037923 67054207 67072983
 67130111 67160652 67164473 67168637 67210432 67299692
 67309992 67349466 67427037 67451395 67457144 67466239
 67556285 67645895 67648084 67652884 67681016 67688155
 67779857 67823050 67839865 67929294 67945609 67979946
 68008264 68027067 68041437 68068262 68105043 68117031
 68140724 68227263 68358456 68358960 68392190 68398235
 68399453 68437706 68443374 68529711 68536177 68551557
 68557783 68602872 68633069 68699844 68708518 68764736
 68779141 68808738 68823278 68842847 68895143 68983417
 69038040 69043244 69060416 69103938 69149793 69153298
 69186994 69201058 69254904 69263833 69264459 69278987
 69297082 69384782 69388568 69389306 69418044 69428877
 69473014 69481127 69489523 69567674 69581895 69650933
 69655842 69700743 69710573 69755965 69804559 69831587
 69836062 69843145 69854079 69875763 69895958 69909307
 69923534 69990114 69993394 70007942 70013799 70029939
 70035904 70102900 70132196 70168772 70194518 70198055
 70204466 70277485 70284942 70311398 70331580 70365375
 70446038 70448074 70472494 70514894 70537137 70585530
 70637751 70648293 70670452 70679232 70711930 70722145
 70748547 70765041 70785267 70786184 70908874 70935195
 70947327 70969698 71008979 71012558 71014188 71064580
 71082573 71177406 71212095 71214723 71246620 71258051
 71299360 71312943 71322032 71384906 71581906 71606087
 71636390 71637121 71682857 71721068 71723393 71770942
 71782862 71837655 71879659 71908896 71964933 71990233
 71998573 71998659 72013022 72086675 72102029 72103913
 72132576 72164621 72182374 72195465 72218753 72251432
 72272079 72302879 72412917 72441938 72514374 72551486
 72596261 72615028 72652314 72668314 72669602 72693712
 72813193 72842263 72880046 72883429 72898588 72911753
 72913104 72932224 72949041 72996745 73030208 73079192
 73164203 73177557 73299846 73305483 73320854 73334460
 73361570 73404998 73461019 73576149 73580212 73581778
 73617696 73633236 73666721 73690399 73737057 73753349
 73756637 73786499 73800524 73829029 73953611 74022609
 74119141 74182704 74215892 74261100 74272316 74348935
 74435065 74482782 74494464 74504205 74517687 74545714
 74546187 74551965 74587747 74614973 74625238 74810195
 74826223 74827539 74837090 74845609 74867043 74977910
 74988402 75032697 75053492 75086940 75095120 75117519
 75192661 75297190 75365509 75461900 75473747 75478447
 75494827 75501282 75504806 75515684 75589450 75598396
 75666979 75673522 75698953 75733252 75851573 75880269
 75884237 75904988 75936995 75981145 75998089 76008011
 76075559 76239191 76260973 76282674 76393598 76459642
 76682784 76687698 76732855 76778864 76835042 76845307
 76878273 76949941 76967477 77012808 77035917 77163049
 77186961 77317104 77362133 77378585 77390580 77438609
 77440645 77466666 77514905 77530442 77538002 77543871
 77564849 77571107 77588937 77618702 77626621 77714088
 77753264 77765111 77775164 77866380 77914686 77915990
 77939980 77969271 77998376 78053617 78068739 78093476
 78183993 78195752 78235128 78242979 78364553 78368237
 78397759 78456643 78490075 78513237 78538673 78554937
 78568724 78575609 78583025 78587287 78614010 78617212
 78641916 78674212 78680507 78686784 78711398 78799496
 78943059 78947549 78960252 78987076 78997608 79002202
 79007631 79041726 79051465 79090803 79117162 79231812
 79233601 79352061 79413172 79442835 79460535 79549468
 79558086 79590835 79649988 79724004 79792426 79812658
 79877616 79918553 79979832 80026817 80037582 80043301
 80061979 80073848 80080872 80116760 80147529 80160763
 80168865 80234981 80255578 80278090 80372514 80396717
 80413433 80421688 80480566 80564093 80564792 80584377
 80625052 80632892 80667269 80752417 80754466 80758048
 80776066 80798939 80822781 80851401 80912863 80934403
 80974553 80983281 80999288 81008779 81016271 81054243
 81077467 81125399 81151029 81160395 81177275 81203010
 81308692 81419886 81453065 81540501 81622789 81654313
 81677340 81732377 81733925 81737500 81739313 81762936
 81764239 81908300 81908717 81938574 81943231 82132746
 82156871 82217836 82318284 82327792 82366704 82400066
 82406578 82454891 82484945 82489922 82515656 82543694
 82579538 82582726 82697898 82721149 82752056 82764006
 82771343 82774689 82800119 82815716 82871719 82890398
 82922984 82928080 82933857 83070652 83088485 83120604
 83143341 83159290 83266311 83268046 83307353 83330217
 83404763 83425999 83470241 83486195 83509457 83569777
 83570002 83623781 83651404 83692690 83698734 83700659
 83743610 83768070 83768577 83887902 83914908 83934514
 83948071 83962202 84028667 84060574 84073551 84104335
 84143326 84164206 84195710 84244331 84250060 84289762
 84303726 84316240 84364711 84366192 84388723 84478606
 84547363 84624052 84701824 84716628 84741439 84758714
 84821325 84840379 84848829 84908983 84932332 84963470
 84977787 84993972 85024614 85058929 85090821 85159828
 85210328 85281829 85287632 85470428 85520658 85550474
 85567434 85638203 85642670 85652329 85691591 85700522
 85701159 85737482 85742548 85767230 85848927 85851182
 85960184 85974624 85998284 86002651 86010374 86046198
 86055547 86062127 86090564 86198936 86207863 86257213
 86285807 86288843 86294988 86361095 86441613 86470296
 86504213 86505446 86508346 86597689 86639746 86675417
 86711772 86721621 86742351 86773343 86870480 86877243
 86914489 86976423 86982477 86987175 86993742 86994184
 87010859 87087657 87098383 87118572 87119727 87123923
 87159845 87165930 87185508 87241886 87272720 87297389
 87340329 87366128 87439874 87452944 87489702 87504858
 87528042 87619535 87621370 87646038 87711152 87747260
 87781563 87827930 87856064 87908958 87945329 87947046
 87963855 87995467 88029961 88052919 88245264 88261941
 88339298 88428210 88458190 88472810 88499399 88504022
 88553660 88580314 88581670 88586013 88602782 88621850
 88631793 88634354 88643557 88695638 88730908 88738938
 88753309 88826996 88889165 88929339 88946371 88951818
 88995446 89010113 89022093 89047073 89066555 89240110
 89309169 89368447 89444116 89444829 89466302 89489371
 89509501 89565453 89591409 89640830 89645540 89733557
 89739946 89805305 89811774 89821268 89829663 89874946
 89936409 89966459 89972711 89987029
 ))