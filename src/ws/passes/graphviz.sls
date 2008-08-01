#!r6rs

;; Visualize a dataflow graph in graphviz.

(library (ws passes graphviz)
  (export output-graphviz)
  (import (rnrs) (ws common) 
	  (ws compiler_components c_generator))

(define (cutpoint? op)
  (match op [(cutpoint . ,_) #t]  [,else #f]))

(define-pass output-graphviz
  [Program
   (lambda (prog Expr)
     (match prog 
       [(,lang '(graph (const ,_ ...)
		       (init  ,__ ...)
		       (sources ((name ,srcv*) (output-type ,st*) (code ,se*) (outgoing ,sdownstrm** ...)) ...)
		       (operators ,oper* ...)
		       (sink ,base ,basetype)
		       ,meta* ...))
	(match (filter (compose not cutpoint?) oper*)
	  [((,op* (name ,opv*) (output-type ,ot*) (code ,oe*) 
		  (incoming ,oin* ...) (outgoing ,odownstrm** ...))
	    ...)

	(define annot-table
	  (let ([tab (make-default-hash-table (length op*))])
	    (for-each (lambda (name op)
			(ASSERT symbol? (car op))
			(let ([entry (assq 'annotations (cdr op))])
			  (hashtab-set! tab name (if entry (cdr entry) '()))))
	      (append srcv* opv*)
	      (append se* oe*))
	    ;(inspect (hashtab->list tab))
	    tab))
	
        (define input-frequency 
	  (let ([tmp (project-metadata 'profiled-input-frequencies prog)])
	    ;(unless tmp (error 'graphviz "right now you MUST have scheme profile data so we can get the input-frequency"))
	    ;(ASSERT (curry = 2) (length tmp))
	    ;; HACK: FIXME!   FIXME!  FIXME!   FIXME!   FIXME!  
	    ;; For now we're just taking the MAX!
	    (if tmp (apply max (cdr tmp)) 1)))
	(define (profiling-duration)
	  (match (ws-profile-limit)
	    [(virttime ,vt) vt]
	    [,oth (error 'graphviz "could not determine the Scheme profile duration from this setting of ws-profile-limit: ~s" 
			 (ws-profile-limit))]))

	(define cutnodes
	  (filter id
	    (map (lambda (name code)
		   (match code
		     [(,_ (annotations ,annot ...) ,rest ...)
		      (and (assq 'partition-point annot) name)]
		     ;[#f #f]
		     ))
	      opv* oe*)))
	(define (denumber x) (if (symbol? x) x (begin (ASSERT (list? x)) (denumber (cadr x)))))
	(define partition-edge-style  "[style=\"setlinewidth(6)\",arrowhead=\"diamond\"]")
	;; Generate edge entries for each subscriber of a stream.
	(define (blowup src dest*)	  
	  ;; Could use deunique-name for the "label" of the node.
	  (map (lambda (dest) (format "  ~a -> ~a ~a ~a;\n" src (denumber dest)			      
				      (if (memq src cutnodes) partition-edge-style "")
				      ;; Annotate data-rates on the edges.
				      (match (assq 'data-rates  (ASSERT (hashtab-get annot-table src)))
					[(data-rates ,name ,stats) 
					 #;
					 (format "[label=\" ~a bytes in ~a tuples\"]" 
						 (bench-stats-bytes stats)
						 (bench-stats-tuples stats))
					;(inspect (vector (profiling-duration) input-frequency))
					 ;(inspect stats)
					 (format ;"[label=\" ~:d bytes/sec\\n \"]" 
					         "[label=\" ~a bytes/sec\\n \"]"
						 (comma-number
						  (if (zero? (bench-stats-bytes stats))
						     "?"
						     ;; Compute bytes/sec
						     (exact
						      (round
						       (* (/ (bench-stats-bytes stats) 
							     (bench-stats-tuples stats))
							  input-frequency)))
						     #;
						     (round-to 
						      1 (/ (bench-stats-bytes stats)
							   (/ (profiling-duration) input-frequency)))))
						 )]
					[#f ""])
				      ))
	    dest*))
	;; Don't bother generating nodes for the inline code.
	(define edges1 (map (lambda (src down* code)
			      (match code
				[(inline_TOS . ,_) ""]
				[(inline_C . ,_) ""]
				[,else (blowup src down*)]))
			 srcv* sdownstrm** se*))
	(define edges2 (map blowup opv* odownstrm**))
	
	(define srclabels
	  (map (lambda (name code)
		 (match code
		   [(inline_TOS . ,_) ""]
		   [(inline_C . ,_) ""]
		   [,else (format "  ~a [shape=plaintext, label=\"~a\"]\n" name 
				  (deunique-name name))]))
	    srcv* se*))

	(define (get-annotation sym)
	  (lambda (exp)
	    (match exp
	      [(,op (annotations . ,annot) . ,_)
	       (assq sym annot)]
	      [,else #f])))
	
	;; Changing this to log instead of linear:
	;(define (preprocess-ticks x) x)
	(define (preprocess-ticks x) (log (add1 (abs x))))
	(define ticks->color
	  (lambda (all_ticks)
	    (define min_ticks (apply min +inf.0 all_ticks))
	    (define max_ticks (apply max -inf.0 all_ticks))
	    (define span (- max_ticks min_ticks))
	    (lambda (_ticks)	      
	      (define ticks (preprocess-ticks _ticks))
	      (define fraction (if (zero? span) 0 (/ (- ticks min_ticks) span)))
	      (define (pad str) (if (= (string-length str) 1) (string-append "0" str) str))
	      ;; RGB:
	      (string-append "#"
	       (pad (number->string (exact (floor (* 255. fraction))) 16))
	       (pad (number->string (exact (floor (* 255. (- 1 fraction)))) 16))
	       "00")
	      #;
	      ;; HSV:
	      (string-append (number->string (inexact (+ .65 (* .35 fraction))))
			     " 0.7 0.8")
	      )))
	
	(define measured-ticks->color
	  (ticks->color (map preprocess-ticks
			  (map cadr
			    (filter id 
			      (map (get-annotation 'measured-cycles)
				(append se* oe*)))))))
	(define scheme-ticks->color 
	  (ticks->color (map preprocess-ticks
			  (map bench-stats-cpu-time
			    (map caddr
			      (filter id 
				(map (get-annotation 'data-rates)
				  (append se* oe*))))))))

	(define nodelabels 
	  (map (lambda (name opcode)
		      (match opcode
			[(,streamop (annotations . ,annot) . ,rest)
			 (let* ([embedded-node? #f]
				[entry (assq 'name annot)]
				[sym (deunique-name (if entry (cadr entry) name))]
				[str (symbol->string sym)]
				[k (string-length "Node:")]
				[namelabel 
				 (if (and (> (string-length str) k)
					  (equal? "Node_" (substring str 0 k)))
				     (begin (set! embedded-node? #t)
					    (substring str k (string-length str)))
				     str)])			   
			   (list (format "  ~a [label=\"~a~a\" ~a];\n"
				       name
				       ;; [2008.04.06] Temporarily removing the separate label:
				       name ;namelabel
				       (let ([cpu (assq 'cpu-pin annot)]
					     [datarates (assq 'data-rates annot)]
					     [measured-cycles (assq 'measured-cycles annot)]
					     )
					 (string-append
					  (if cpu       (format "\\n[cpu ~a]" (cdr cpu)) "")
					  ;; This should be improved, and should probably affect the color
					  (if datarates (format "\\n~a ms Server" 
								(max 0
								 (round-to 4
								 (/ (bench-stats-cpu-time (caddr datarates))
								    ;; How many epochs did we simulate?
								    (/ (profiling-duration) input-frequency)
								    ))))
					      "")
					  (if measured-cycles 
					      (let ([num (cadr measured-cycles)]
						    [denom (caddr measured-cycles)])
						;;(format "\\n~a ticks/~akhz")
						(format "\\n~a ms ~a"
							(round-to 2 (* (/ num denom) 1000))
							;; HACK: FIXME: need a better way to tell if we're in java mode:
							(case (compiler-invocation-mode)
							  ;[(wavescript-compiler-nesc) "TinyOS"]
							  ;[(wavescript-compiler-javaME) "javaME"]
							  [(wavescript-compiler-nesc) "Tmote"]
							  [(wavescript-compiler-javaME) "N80"]
							  [else "Mote"]))
						)

					      "")
					  ))

				       ;; Extra node configuration fields:
				       (string-append
					;; First, set the shape:
					#;
					(begin 
					  (unless (eq? streamop 'iterate)
					    (printf "streamop: ~s\n" streamop))
					  "")
					(match streamop
					 [_merge ", shape=point"]
					 [unionN ", shape=point"]					 
					 [,_ (guard embedded-node?) 
					     (if (assq 'floating annot)
						 ;", shape=octagon"
						 ", shape=box, style=\"filled,rounded\""
						 ", shape=box")]
					 [,else ""])
					;; Next, set the color:
					(cond
					 ;; In this first case we just use color to visualize the partitioning:
					 [(assq 'node/server-assignment annot) => 
					  (lambda (entry)
					    (format ", style=filled, fillcolor=\"~a\""
						    (if (zero? (cadr entry))
							"yellow" "cyan")))]
					 ;; In this case we use color to represent profiling results on the real hardware
					 [(assq 'measured-cycles annot)
					    (if embedded-node?						
						(format ", style=filled, fillcolor=\"~a\""
							(measured-ticks->color (cadr (assq 'measured-cycles annot))))
						;; If it was measured it was on the node but is floating:
						(format ", shape=box, style=\"filled,rounded\", fillcolor=\"~a\""
							(measured-ticks->color (cadr (assq 'measured-cycles annot)))))]
					 ;; If we don't have data from the embedded node, we use the Scheme-generated profiling data:
					 [(assq 'data-rates annot) => 
					  (lambda (datarates)
					    (let ([cpu (bench-stats-cpu-time (caddr datarates))])
					      (format ", style=filled, fillcolor=\"~a\"" (scheme-ticks->color cpu))))]

					 [else ""]))
				       )))]
			[(__foreign_source ',name ,ls ,ty)
			 "";(format "  ~a [shape=invtriangle, label=\"~a\"]\n" name name)
			 ]
			
			[(inline_C . ,_) ""]
			[(inline_TOS . ,_) ""]

;			[#f ""]

			;[,_ (void)]
			)
		      )
	    (append srcv* opv*) 
	    (append se* oe*)))
	
(text->string
`("
digraph Foo {
  BASE [shape=point];
"  ,(append edges1 (reverse edges2))"
"  ,srclabels"
"  ,nodelabels"
}"))	   

	   ])]))])

;;   rankdir=LR;

;; Take the output of explicit-stream-wiring and produce 







	;(define edges3 (list (format " ~a -> BASE\n" base)))	
;	(inspect (append edges1 edges2 edges3))


) ;; End module
