
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
	   discard-spurious-cutpoints
	   exhaustive-partition-search
	   min-bandwidth-hueristic
	   max-nodepart-hueristic
process-read/until-garbage-or-pred
extract-time-intervals
inject-times
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
  `(cutpoint (incoming ,src) (outgoing ,dest) (output-type ,ty)))

;; We should probably have shared code for handling operators:
;; These simple accessors are just used locally within this module.
(define (opname op) 
  (let ([entry (assq 'name (cdr op))])
    (if entry (cadr entry) #f)))
(define (optype op)
  (cadr (ASSERT (assq 'output-type (cdr op)))))

(define (lookup name ops)
  (let loop ([ops ops])
    (if (null? ops) #f
	(let ([entry (assq 'name (cdr (car ops)))])
	  (if (and entry (eq? (cadr entry) name))
	      (car ops)
	      (loop (cdr ops)))))))
(define (op->downstream op ops) (ASSERT op)  
  (lookup (cadr (ASSERT (assq 'outgoing (cdr op)))) ops))
(define (op->upstream op ops) (ASSERT op)  
  (lookup (cadr (ASSERT (assq 'incoming (cdr op)))) ops))


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
;; This tags it with a '(floating) entry in its metadata:
(define (tagit tag op)
  (match (assq 'code (cdr op))
    [(code (,opname (annotations ,annot* ...) ,rest ...))
     (cons (car op)
	   (replace-assoc `(code (,opname (annotations ,tag ,@annot*) ,@rest))
			  (cdr op)))]
    [,_ op]))

;; ================================================================================

;; A hack to do program partitioning based on the names of top-level streams.
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
	   (if (or (node-name? nm) (memq nm all-incoming))
	       (values (list xp) '())
	       (values '() (list xp)))])))
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
		       (operators ,@(apply append server-oper**))
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
		       (cons (cutpoint last head) acc)]
		      [else		       
		       ;; When we reach a stopping point, put in a cutpoint:
		       (loop (cons (get-next (car tail)) (cdr tail))
			     (cons (cutpoint last head) acc)
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
	  (vector
	   `(,input-language 
	     '(graph (const ,@cnst*) (init ,@init*) (sources) ;; All sources stay on the node for now.
		     (operators ,@(map (lambda (x) (tagit '(floating) x)) mobile))
		     (sink #f #f) ,@meta*))
	   `(,input-language 
	     '(graph (const ,@cnst*) (init ,@init*) (sources ,@src*)
		     (operators ,@nodeops)
		     (sink #f #f) ,@meta*))
	   )
	  ]))])


;; ==================================================
;;; Heuristics:

(define (min-bandwidth-hueristic epochsize time datasize datafreq) 0)
(define (max-nodepart-hueristic  epochsize time datasize datafreq)
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
       (heuristic epochsize (op->time (car realsources)) datasize datafreq))

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
	    (define score (heuristic epochsize epochtime datasize datafreq))
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
		 (trace (cdr ptrs) (set-cons (tagit '(partition-point) (car ptrs)) acc))]
		[else 
		 (let ([down (op->downstream head oper*)])
		   (trace (cons down (cdr ptrs)) 
			  (set-cons (car ptrs) acc)))]
		)))))
     (define nodeops (difference nodeops_and_sources src*))
     (define serverops (difference (difference oper* nodeops) pre-tagged))

     (vector
      `(,input-language 
	'(graph (const ,cnst* ...) (init ,@init*)
		(sources ,@src*)
		(operators ,@nodeops)
		(sink #f #f)
		,@meta*))
      `(,input-language 
	'(graph (const ,cnst* ...) (init ,@init*) (sources)
		(operators ,@serverops)
		(sink ,base ,basetype) ,@meta*)))

     ]))

;; Get all the operator names in a subgraph.
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

;; Remove any cutpoints that don't make sense (because the supposedly
;; 'cut' operator actually is in the partition).  We use this to clean
;; up after the "refine" steps above.
(define discard-spurious-cutpoints
  (lambda (part)
    (define nametable (set->hashtab (partition->opnames part)))
    (define (valid? op)
      (match op
	[(cutpoint . ,rest)	
	 (not (hashtab-get nametable (cadr (assq 'outgoing rest))))]
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
  (define FUDGE-FACTOR 83) ;; The number of ticks assumed for the printf statements themselves.
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