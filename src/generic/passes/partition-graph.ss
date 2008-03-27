
(module partition-graph mzscheme
    (require "../../plt/common.ss"
	     "../../plt/hashtab.ss"
	     )
    (provide 
     	   partition-graph-by-namespace
	   refine-node-partition
	   refine-server-partition
	   merge-partitions
	   partition->opnames
	   map-partition-ops
	   discard-spurious-cutpoints
	   remove-unused-streams
	   exhaustive-partition-search
	   min-bandwidth-hueristic
	   max-nodepart-hueristic
	   reinsert-cutpoints
	   process-read/until-garbage-or-pred
	   extract-time-intervals
	   inject-times
	   tag-op
	   )
    (chezimports)


;; ================================================================================
;;; Helpers and hooks:


;; Keeping stats for data sizes, frequencies, etc.  Perhaps we should
;; keep the whole distribution.  Short of that, for now we will keep
;; max/min/mean/stdev.
(reg:define-struct (stat sum count max min))
(define (stat-mean st) (/ (stat-sum st) (stat-count st)))
(define (stat-variance st) 0)
(define (stat-stddev st) 0)

(define (cutpoint? op)
  (match op [(cutpoint . ,_) #t]  [,else #f]))
(define (make-cutpoint ty src dest) 
  (ASSERT src)
  (ASSERT dest)
  `(cutpoint (name #f) (output-type ,ty) (code #f) (incoming ,src) (outgoing ,dest))
  )

;; We should probably have shared code for handling operators:
;; These simple accessors are just used locally within this module.
(define (opname op) 
  (define stripped (if (symbol? (car op)) (cdr op) op))
  (let ([entry (assq 'name stripped)])
    (if entry (cadr entry) 
	(error 'opname "could not get op name: ~s" op))))
(define (optype op)
  (cadr (ASSERT (assq 'output-type (cdr op)))))

(define (lookup name ops)
  (let loop ([ops ops])
    (if (null? ops) #f
	(let ([entry (assq 'name (cdr (car ops)))])
	  (if (and entry (eq? (cadr entry) name))
	      (car ops)
	      (loop (cdr ops)))))))
;; Returns the first of the downstream operators.
(define (op->downstream op ops) 
  (define ls (ASSERT (assq 'outgoing (cdr op))))
  (ASSERT op) (ASSERT (= (length ls) 2))
  (lookup (cadr ls) ops))
(define (op->upstream op ops) 
  (define ls (ASSERT (assq 'incoming (cdr op))))
  (ASSERT op) (ASSERT (= (length ls) 2))
  (lookup (cadr ls) ops))

(define (op->inputtype op)   
  (match op
    [(cutpoint . ,rest) (cadr (ASSERT (assq 'output-type rest)))]
    [(_merge . ,rest)   (cadr (ASSERT (assq 'output-type rest)))]
    [(iterate . ,rest) 
     (match (assq 'code rest)
       [(code (iterate ,ann (let ,binds (lambda ,args (,inty ,vqty) ,bod)) ,instrm))
	`(Stream ,inty)])]))

(define (op-outgoing op)
  (ASSERT (symbol? (car op)))
  (cdr (ASSERT (assq 'outgoing (cdr op)))))

(define not-inline_TOS?
  (lambda (src) 
    (match (assq 'code src) [(code (inline_TOS . ,_)) #f] [,_ #t])))

(define (replace-assoc assoc ls)
  (define tag (car assoc))
  (let loop ([ls ls])
    (ASSERT (not (null? ls)))
    (if (eq? tag (caar ls))
	(cons assoc (cdr ls))
	(cons (car ls) (loop (cdr ls))))))

;; This tags it with a new entry in its metadata:
(define (tag-op tag op)
  (match (assq 'code (cdr op))
    [(code (,opname (annotations ,annot* ...) ,rest ...))
     (cons (car op)
	   (replace-assoc `(code (,opname (annotations ,tag ,@annot*) ,@rest))
			  (cdr op)))]
    [,_ op]))

;; ================================================================================

;; A hack to do program partitioning based on the names of top-level streams.
;;
;; NOTE: There's no real need to add in cutpoints as part of this
;; pass, we can simply do that afterwards.
(define-pass partition-graph-by-namespace
    (define (node-name? nm)
      (define str (symbol->string nm))
      (define len (string-length "Node:"))
      (and (> (string-length str) len)
	   (or ;(equal? "Node:" (substring str 0 len))
	       (equal? "Node_" (substring str 0 len)))))
    ;; TODO: Need to do this recursively (scan for downstream node-side ops)
    (define (Source node-ops)
      (define all-incoming (apply append (map (lambda (op) (cdr (assq 'incoming (cdr op)))) node-ops)))
      ;(printf "ALL NODE INCOMING: ~s\n" all-incoming)
      (lambda (xp)	
	(match xp
	  [((name ,nm) (output-type ,ty) (code ,code) (outgoing ,down* ...))
	   (if (or (node-name? nm) (memq nm all-incoming)
		   (eq? (car code) 'inline_TOS))
	       (values (list xp) '())
	       (values '() (list xp)))])))
    ;; For a given operator 
    (define (Operator op)
       (let ([name (cadr (assq 'name (cdr op)))]
	     [type (cadr (assq 'output-type (cdr op)))]
	     [incoming (cdr (assq 'incoming (cdr op)))]
	     [outgoing (cdr (assq 'outgoing (cdr op)))])
	 (define me? (node-name? name))
	 ;(define up?* (map node-name? incoming))
	 ;(define all-up? (andmap id up?*))
	 ;(define some-up? (ormap id up?*))
	 (define down?* (map node-name? outgoing))
	 (define all-down? (andmap id down?*))
	 (define some-down? (ormap id down?*))
	 #;
	 (when (and (not all-up?) some-up?)
	   (error 'partition-graph-by-namespace 
		  "operator cannot currently have some input streams node-side and some server: ~a ~s" 
		  name incoming))
	 (cond
	  ;[(and all-up? me?)              (values (list op) '())] ;; Purely node.

	  ;; A cut point, node-side:
	  [(and me? (not all-down?))
	   (let ([serv-downstrm (filter id (map (lambda (x y) (and (not x) y)) down?* outgoing))])
	     ;(printf "GENERATING CUT POINTS: ~s ~s ~s ~s\n" name serv-downstrm down?* outgoing)
	     (values (cons op (map (lambda (down) (make-cutpoint type  name down)) ;; node cuts
				serv-downstrm))
		     (map (lambda (down) (make-cutpoint type name down)) ;; server cuts
		       serv-downstrm)))]

	  ;; We're either node-side or feed to somebody that's node side:
	  [(or me? (ormap id down?*))      (values (list op) '())] ;; Node.

	  ;[(and (not all-up?) some-up?)   (values '() (list op))] ;; Server, e.g. merge.
	  [else                           (values '() (list op))]) ;; Server.
	))
    [Program 
     (lambda (prog Expr)
       (match prog
	 [(,input-language 
	   '(graph (const ,cnst* ...)
		   (init  ,init* ...)
		   (sources ,src* ...)
		   (operators ,[Operator -> node-oper** server-oper**] ...)
		   (sink ,base ,basetype)	,meta* ...))
	  (define nodeops (apply append node-oper**))
	  (define serverops (apply append server-oper**))
	  (define allops (append nodeops serverops))

	  ;; Here we do a post-processing step.
	  ;; For now, any supposedly "server" ops that feed into known node-ops get sucked onto the node also:
	  (let ([nametable (set->hashtab (map-filter opname nodeops))])
	    (let loop ([ops serverops] [acc '()])
	      (define rejects
		(filter (lambda (op)
			  (ormap (trace-lambda INTABLE (name) (hashtab-get nametable name))
				 (op-outgoing op)))
		  ops))
	      (if (null? rejects) 
		  (begin 		    
		    ;(inspect (vector 'REJECTED acc))
		    ;; Finished, modify partitions:
		    (set! nodeops (append acc nodeops))
		    (set! serverops (difference serverops acc)))
		  (begin 
		    ;; Add the new rejects to the table:
		    (for-each (lambda (reject) (hashtab-set! nametable reject #t)) acc)
		    (loop (difference ops rejects) (append rejects))))))

	  (let-match ([(,[(Source nodeops) -> node-src** server-src**] ...) src*])
	  
	    ;; TODO: Should filter the constants appropriately:
	    (vector
	     `(,input-language 
	       '(graph (const ,cnst* ...) (init ,@init*)
		       (sources ,@(apply append node-src**))
		       (operators ,@nodeops)
		       (sink #f #f)
		       ,@meta*))
	     `(,input-language 
	       '(graph (const ,cnst* ...) (init ,@init*)
		       (sources ,@(apply append server-src**))
		       (operators ,@serverops)
		       (sink ,base ,basetype)
		       ,@meta*)))
	    )]))]

    )


;; The initial cut (based on namespace) provides some information --
;; in particular, foreign calls in the node partition must happen on
;; the node.  Next, we separate out those operators which are flexible
;; -- *mobile* -- that can live on either side of the partition.  We
;; insert new cutpoints along the new fringe that we push to.
(define-pass refine-node-partition     
  (define (Operator Expr)
    (lambda (op)
      (let ([code (assq 'code (cdr op))])
	(if code 
	    (Expr (cadr code))
	    (match op
	      ;; Prune out cutpoints for now:
	      [(cutpoint . ,_) 
	       #t
	       ])
	    ))))
  ;; This returns true if the operator is "clean" -- if it doesn't
  ;; contain anything that would force it to be on the embedded node.
  [Expr (lambda (xp fallth)
	  (match xp
	    [(,frgn . ,_)
	     (guard (eq-any? frgn 'foreign '__foreign 'foreign-app
			     ;; For now printing is disabled too:
			     'print
			     ))
	     #f]
	    [,oth (fallth oth)]))]
  [Fuser (lambda (ls k) (and-list ls))]
  [Program 
     (lambda (prog Expr)
       (match prog
	 [(,input-language 
	   '(graph (const ,cnst* ...)
		   (init  ,init* ...)
		   (sources ,src* ...)
		   (operators ,oper* ...)
		   (sink ,base ,basetype)	,meta* ...))
	  (define cutpoints
	    (filter (lambda (op) (eq? (car op) 'cutpoint)) oper*))
	  (define (get-downstream x) (op->downstream x oper*))
	  (define (get-upstream x)   (op->upstream   x oper*))
	  (define next-down (filter id (map get-downstream cutpoints)))
	  (define next-up   (filter id (map get-upstream   cutpoints)))
	  ;; From each cutpoint, walk until we hit something that's not mobile:
	  (define (trace get-next startpoints cutpoint)
	    (define (insert-cp a b acc)
	      (if (or (cutpoint? a) (cutpoint? b))
		  acc
		  (cons (cutpoint a b) acc)))
	    (if (null? startpoints) '()
		(let loop ([tracepoints (cons (get-next (car startpoints)) (cdr startpoints))]
			   [acc '()]
			   [last (car startpoints)])
		  (match tracepoints
		    [() acc]
		    [(,head . ,tail)
					;(printf "  Considering: ~s\n" (assq 'name (cdr head)))
		     (cond
		      [((Operator Expr) head)
		       (loop (let ([x (get-next head)]) (if x (cons x tail) tail))
			     (cons head acc) head)]
		      [(null? tail) 
		       ;; Insert a cutpoint:
		       (insert-cp last head acc)]
		      [else		       
		       ;; When we reach a stopping point, put in a cutpoint:
		       (loop (cons (get-next (car tail)) (cdr tail))
			     (insert-cp last head acc)
			     (ASSERT cutpoint? (car tail)))])]))))

;	  (define _ (print-level 3))

	  ;; Hack, making this work in both directions:
	  ;; (Thus we can use it for the node and server.)
	  (define mobile 
	    (append (trace get-downstream (filter get-downstream cutpoints);next-down 
			   (lambda (in out) (make-cutpoint (optype in) (opname in) (opname out))))
		    (trace get-upstream   (filter get-upstream cutpoints) ;next-up
			   (lambda (out in) (make-cutpoint (optype in) (opname in) (opname out))))))
	  (define nodeops (difference oper* mobile))	  
	  ;; Returns #(floating stationary):
	  ;(define tagged (map (lambda (x) (tag-op '(floating) x)) mobile))
	  (vector
	   `(,input-language 
	     '(graph (const ,@cnst*) (init ,@init*) (sources) ;; All sources stay on the node for now.
		     (operators ,@mobile) ;(operators ,@tagged)
		     (sink #f #f) ,@meta*))
	   `(,input-language 
	     '(graph (const ,@cnst*) (init ,@init*) (sources ,@src*)
		     (operators ,@nodeops)
		     (sink #f #f) ,@meta*))
	   )
	  ]))])


;; ==================================================
;;; Heuristics:

(define (min-bandwidth-hueristic name epochsize time datasize datafreq) 0)
(define (max-nodepart-hueristic name epochsize time datasize datafreq)
  (if (>= time epochsize) 0 time)) ;; Try to fill up the epoch

;; ==================================================

(define (exhaustive-partition-search heuristic part)
  (match part
    [(,input-language 
      '(graph (const ,cnst* ...) (init  ,init* ...)
	      (sources ,src* ...) (operators ,oper* ...)
	      (sink ,base ,basetype)	,meta* ...))
     (define cutpoints (filter cutpoint? oper*))

     (define (op->time op) 
       (let ([entry (deep-assq 'measured-cycles op)])  ;; Hackish
	 (if entry
	     (cadr entry)
	     0)))

     (define realsources  (filter not-inline_TOS? src*))

     (define epochsize 
       (match realsources
	 [((,_ ,__ (code (__foreign_source ',fn '(,rate . ,rest) ',ty)) . ,___))
	  (let ([_rate (ASSERT (string->number rate))])
	    ;; By a 32khz clock.
	    (/ 32000 _rate))]))
     
     (define ___ 
       ;; TEMP: just handling a linear pipe right now:
       (unless (and (= 1 (length cutpoints))
		    (= 1 (length realsources)))
	 (error 'exhaustive-partition-search "only works for a linear pipeline right now...")))

     (define datasize #f) ;; TODO: Hook up to scheme profiling!
     (define datafreq #f)
     (define initscore
       (heuristic (opname (car realsources))
		  epochsize (op->time (car realsources)) datasize datafreq))

     (define ____ (begin (printf "Begining search")(flush-output-port)))

     ;; Do a linear walk from source to cutpoint, scoring each potential cut.
     (define new-cutpoint
       (progress-dots
	(lambda ()
	  (let trace ([ptr (car realsources)] 
		      [epochtime 0]
		      [curbest initscore]
		      [cur (car realsources)])
	    (define datasize #f) ;; TODO: Hook up to scheme profiling!
	    (define datafreq #f)
	    ;; Evaluate the heuristic for this configuration:
	    (define score (heuristic (opname ptr) epochsize epochtime datasize datafreq))
	    (define-values (newscore newcur)
	      (if (> score curbest)	       
		  (values score   ptr)
		  (values curbest cur)))
	    (define downstream (op->downstream ptr oper*))
	    (if downstream
		(trace downstream (+ epochtime (op->time downstream)) newscore newcur)
		;; Otherwise we're all done, return the new cutpoint:
		newcur)))))
     (printf " done!\n")
          

     ;; Now partition according to that new cutpoint: Perhaps we
     ;; should have built up these new partitions while we were
     ;; searching... but this keeps things separate:
     (partition-at (list new-cutpoint) part)]))


;; For the time being, the mobility criteria for the server side is
;; the *same* as the node-side (are there foreign calls?).  In the
;; future, this may include other distinctions, such as using floating
;; arithmetic and so on.  Also, we ultimately need to enforce some
;; limit on very expensive operators.  It could be a disaster to even
;; try to benchmark really expensive operators on the motes.  We
;; should use the benchmark results from the server side to estimate
;; whether we should at all attempt benchmarking on the node.
(define refine-server-partition refine-node-partition)

;; Takes the "base" portion, the metadata, the init, and the constants from p1...  
(define (merge-partitions p1 p2)
  (discard-spurious-cutpoints
   (match (vector p1 p2)
     [#((,input-language 
	 '(graph (const ,cnst* ...)  (init  ,init* ...)  (sources ,src* ...)
		 (operators ,oper* ...)  (sink ,base ,basetype) ,meta* ...))
	(,___
	 '(graph (const ,cnst2* ...) (init  ,init2* ...) (sources ,src2* ...)
		 (operators ,oper2* ...) (sink ,base2 ,2basetype) ,2meta* ...)))
      `(,input-language 
	'(graph (const ,@cnst*)  ; ,@cnst2*
		(init ,@init*) ; ,@init2*
		(sources ,@src* ,@src2*)
		(operators ,@oper* ,@oper2*)
		(sink ,base ,basetype) ,@meta*))])))

;; This takes a set of edges to cut at.  However, currently, "edges"
;; are represented by the boxes that output to those edges.  This is
;; not quite right, but will do for now.
;;
;; Right now this also caries the responsibility of tagging the
;; operator in question (using its annotations) to mark it as 'partition-point'.
;; ALSO, it inserts new cutpoints.
;; 
;; .returns A vector containaining node-partition (upstream) and
;; server-partition (downstream)
(define (partition-at newcuts part)
    (match part
    [(,input-language 
      '(graph (const ,cnst* ...) (init  ,init* ...)
	      (sources ,src* ...) (operators ,oper* ...)
	      (sink ,base ,basetype)	,meta* ...))

     (define (set-cons x set) (if (memq x set) set (cons x set)))

     (define pre-tagged '()) ;; HACK: mutated below

     ;; First, do a walk from the sources, accumulating everything
     ;; until we hit a "cutpoint" operator.
     ;; TODO: it's probably worth making a hash for this walk.
     (define nodeops_and_sources
       (let trace ([ptrs (filter not-inline_TOS? src*)] [acc '()])
	 ;(printf "TRACING ~a\n" (map opname ptrs))
	 (if (null? ptrs) (reverse! acc)
	     (let ([head (car ptrs)])
	       (cond 
		[(not head) (trace (cdr ptrs) acc)]
		[(memq head newcuts) ;; We've reached a cut.
		 ;; TODO: Should check that there aren't any more "cuts"
		 ;; downstream.  That would be invalid. 
		 (set! pre-tagged (cons (car ptrs) pre-tagged))
		 (trace (cdr ptrs) (set-cons (tag-op '(partition-point) (car ptrs)) acc))]
		[else 
		 (let ([down (op->downstream head oper*)])
		   (trace (cons down (cdr ptrs)) 
			  (set-cons (car ptrs) acc)))]
		)))))
     (define nodeops (difference nodeops_and_sources src*))
     (define serverops (difference (difference oper* nodeops) pre-tagged))

     (vector
      (reinsert-cutpoints
       `(,input-language 
	'(graph (const ,cnst* ...) (init ,@init*)
		(sources ,@src*)
		(operators ,@nodeops)
		(sink #f #f)
		,@meta*)))
      (reinsert-cutpoints
       `(,input-language 
	 '(graph (const ,cnst* ...) (init ,@init*) (sources)
		 (operators ,@serverops)
		 (sink ,base ,basetype) ,@meta*))))
     ]))

;; Get all the operator names in a subgraph.
;; This includes the source names!
(define (partition->opnames part)
  (match part
    [(,input-language 
      '(graph (const ,cnst* ...)
	      (init  ,init* ...)
	      (sources ,src* ...)
	      (operators ,oper* ...)
	      (sink ,base ,basetype)	,meta* ...))
     (filter id 
       (append (map opname src*)
	       (map opname oper*)))
     ]))

;; Map a function across all the operators (not including sources):
(define (map-partition-ops fn part)
  (match part
    [(,input-language 
      '(graph (const ,cnst* ...)  (init  ,init* ...)
	      (sources ,src* ...) (operators ,oper* ...)
	      (sink ,base ,basetype)	,meta* ...))
     `(,input-language 
      '(graph (const ,@cnst*)  (init  ,@init*)
	      (sources ,@src*) (operators ,@(map fn oper*))
	      (sink ,base ,basetype) ,@meta*))]))


;; Remove any cutpoints that don't make sense (because the supposedly
;; 'cut' operator actually is in the partition).  We use this to clean
;; up after the "refine" steps above.
(define discard-spurious-cutpoints
  (lambda (part)
    (define nametable (set->hashtab (cons 'BASE (partition->opnames part))))
    (define (valid? op)
      (match op
	[(cutpoint . ,rest)
	 (or (not (hashtab-get nametable (cadr (assq 'outgoing rest))))
	     (not (hashtab-get nametable (cadr (assq 'incoming rest)))))]
	[,_ #t]))
    (match part
      [(,input-language 
	'(graph (const ,cnst* ...)
		(init  ,init* ...)
		(sources ,src* ...)
		(operators ,oper* ...)
		(sink ,base ,basetype)	,meta* ...))
      `(,input-language 
	'(graph (const ,@cnst*)  
		(init ,@init*) 
		(sources ,@src*)
		(operators ,@(filter valid? oper*))
		(sink ,base ,basetype) ,@meta*))])))

;; Recursively removes stream operators from the graph that are not consumed:
(define remove-unused-streams
  (lambda (part)
    (define nametable (set->hashtab (cons 'BASE (partition->opnames part))))
    (define (valid? op) ;; Works for sources and ops:
      (let* ([op (if (symbol? (car op)) (cdr op) op)]
	     [name (cadr (assq 'name op))]
	     [outgoing (cdr (ASSERT (assq 'outgoing op)))])
	;(printf "CONSIDERING ~s ~s ~s\n" name outgoing (map car (hashtab->list nametable)))
	(if (ormap (lambda (out) (hashtab-get nametable out)) outgoing)
	    #t
	    (begin (hashtab-remove! nametable name)
		   ;(printf "  REMOVED ~s\n" name)
		   #f))))
    (match part
      [(,input-language 
	'(graph (const ,cnst* ...)(init  ,init* ...)
		(sources ,src* ...)(operators ,oper* ...)
		(sink ,base ,basetype)	,meta* ...))
       ;; iterate until fixed point:
       (define (iterate ops)
	 (let loop ([ops ops])
	   (let ([new (filter valid? ops)])
	     (if (equal? new ops) new
		 (loop new)))))
       (define newops (iterate oper*))
       (define newsrc (iterate src*))       
      `(,input-language 
	'(graph (const ,@cnst*)  (init ,@init*) 
		(sources ,@newsrc)
		(operators ,@newops)
		(sink ,base ,basetype) ,@meta*))])))


;; This should obsolete some of the other operations above.  It should
;; be safe to just work with cutpoint-free partitions and THEN insert
;; the cutpoints at the end.  I should remove any code above that goes to pains to deal with cutpoints.
(define reinsert-cutpoints
  (lambda (part)
    (define names (cons 'BASE (partition->opnames part)))
    (define nametable (set->hashtab names))
    (define (make-cutpoints ops)
      (apply append
	     (map (lambda (op)
		    (match op
		      [(,op* (name ,opv) (output-type ,ot) (code ,oe)
			     (incoming ,in* ...) (outgoing ,downstrm* ...))
		       (append 
			(map-filter (lambda (down) 
				      (if (hashtab-get nametable down) #f
					  (make-cutpoint ot opv down)))
				    downstrm*)
			(let ([intype (op->inputtype op)])
			  (map-filter (lambda (in) 
					(if (hashtab-get nametable in)  #f
					    (make-cutpoint intype in opv)))
				      in*)))
		       ]))
	       ops)))
    (match part
      [(,input-language 
	'(graph (const ,cnst* ...)
		(init  ,init* ...)
		(sources ,src* ...)
		(operators ,oper* ...)
		(sink ,base ,basetype)	,meta* ...))
       (define filtered (filter (compose not cutpoint?) oper*))
       `(,input-language 
	 '(graph (const ,@cnst*)  
		 (init ,@init*) 
		 (sources ,@src*)
		 (operators ,@(append (make-cutpoints filtered) filtered))
		 (sink ,base ,basetype) ,@meta*))])))





;; ================================================================================


;;; Also using this file for utilities pertaining to partitioning,
;;; mainly having to do with timing.

(define (process-read/until-garbage-or-pred str pred)
  (IFCHEZ 
   (let-match ([(,inp ,outp ,pid) (process str)])
     (define (shutdown) 
       (close-output-port outp)
       (close-input-port  inp)
       ;; For some reason that's not enough to kill it:
       (printf "Killing off stray java process...\n")
       (system (format "kill ~a" pid)))
     (printf "Reading serial interface through java process with ID ~s\n" pid)
     (flush-output-port)
     (let loop ([n 0] [acc '()])
       (let* (;[line (read-line inp)]
					;[_ (printf "Read: ~a\n" line)]
					;[x (read (open-input-string line))]
	      [x (read inp)]
	      )
	 (printf "  ReadFromMote: ~a\n" x)(flush-output-port)
	 (cond 
	  [(eof-object? x) (shutdown) (reverse! acc)]
	  [(pred x) ; (> n 15)
	   (shutdown) (reverse! (cons x acc))]
	  [else (loop (add1 n) (cons x acc))])))
     ;; TODO: handle read errors:
     )
   (error 'process-read/until-garbage-or-pred "not implemented except under Chez Scheme")))

(define (extract-time-intervals log)
  ;; ASSUMPTION: uint16_t counter:
  (define FUDGE-FACTOR 82) ;; The number of ticks assumed for the printf statements themselves.
  (define (diff st en)
    (let ([elapsed (- en st FUDGE-FACTOR)])
					;(ASSERT (> elapsed 0))
      (max elapsed 0)
      #;
      (if (< elapsed 0)
	  (inspect elapsed) ;(+ (^ 2 16) elapsed)
	  elapsed)))
  (define (combine overflow n) (+ (* (^ 2 16) overflow) n))
  (let loop ([alist '()] [open #f] [strt #f] [log log])
    (match log 
      [() alist]
      [((Start ,name ,overflow ,t) . ,_) (ASSERT (not (assq name alist)))
					;(set! alist (cons name t))
       (loop alist name (combine overflow t) _)]
      [((End ,name ,overflow ,tme) . ,_)
       (ASSERT (eq? name open))
       (loop (cons (cons name (diff strt (combine overflow tme))) alist) #f #f _)]
      ;; HACK: End message ALSO serves to close off the open measurement.
      [((EndTraverse ,overflow ,tme) . ,_)
       (ASSERT null? _)
       (loop (if open 
		 (cons (cons open (diff strt (combine overflow tme))) alist)
		 alist)
	     #f #f _)]
      ;; Ignoring garbage for now:
      [(,oth . ,[rest]) rest])))


;; Inserts a (measured-cycles <elapsed> <timer-frequency>) annotation:
(define (inject-times prog times)
  (define (Operator op)
    (match op
      [(iterate (name ,nm) ,ot
		(code (iterate (annotations ,annot* ...) ,itercode ,_))
		,rest ...)
					;(printf "LOOKING UP ~a in ~a\n" nm times)
       (let ([entry (assq nm times)])		     
	 (if entry
	     `(iterate (name ,nm) ,ot 
		       (code (iterate (annotations (measured-cycles ,(cdr entry) 32000) ,@annot*)
				      ,itercode ,_))
		       ,@rest)
	     op))]
      [,oth oth]))
  (match prog
    [(,input-language 
      '(graph ,cnst ,init ,src
	      (operators ,[Operator -> oper*] ...) ,rest ...))
     `(,input-language 
       '(graph ,cnst ,init ,src
	       (operators ,oper* ...) ,rest ...))]))




) ;; End module