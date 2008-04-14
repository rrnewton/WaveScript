
(module partition-graph mzscheme
    (require "../../plt/common.ss"
	     "../../plt/hashtab.ss"
	     "wavescope_bkend/nominalize-types.ss"
	     )
    (provide 
     	   partition-graph-by-namespace
	   refine-node-partition
	   refine-server-partition
	   merge-partitions
	   partition->opnames
	   print-partition
	   map-partition-ops
	   discard-spurious-cutpoints
	   remove-unused-streams
	   exhaustive-partition-search

	   min-bandwidth-heuristic
	   max-nodepart-heuristic
	   min-nodepart-heuristic

	   reinsert-cutpoints
	   process-read/until-garbage-or-pred
	   extract-time-intervals
	   extract-java-time-intervals
	   inject-times
	   inject-assignments
	   tag-op

	   multiplex-migrated

	   partition->simple-graph
	   partition->frequency
	   partition-sourcesonly
	   partition-baseonly
	   partition-getmiddle
	   emit-lp read-back-lp-results

	   test-partition-graph
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
  `(cutpoint (name #f) (output-type ,ty) (code #f) (incoming ,src) (outgoing ,dest)))

;; Due to our lack of static type checking, things can get messed up,
;; this is for dynamic asserts:
(define (operator? op)
  (and (pair? op) (stream-primitive? (car op))))

(define (source? x)
  (match x
    [((name ,n) (output-type ,ty) (code (,prim . ,args)) (outgoing ,out* ...))
     (regiment-primitive? prim)]
    [,_ #f]))

(define (op-or-source? x)
  (or (operator? x)  (source? x)))

;; We should probably have shared code for handling operators:
;; These simple accessors are just used locally within this module.
(define (opname op)
  (define stripped (if (symbol? (car op)) (cdr op) op))
  (let ([entry (assq 'name stripped)])
    (if entry (cadr entry) 
	(error 'opname "could not get op name: ~s" op))))
(define (optype op)
  (cadr (ASSERT (assq 'output-type (cdr op)))))

;; Returns #f if lookup fails:
(define (lookup name origops)
  (let loop ([ops origops])
    (if (null? ops) 
	#f
	;(error 'lookup "did not find ~s in ~s" name origops)
	(let ([entry (assq 'name (cdr (car ops)))])
	  (if (and entry (eq? (cadr entry) name))
	      (car ops)
	      (loop (cdr ops)))))))

;; This is annoying
(define (strip-index outgoing)
  (match outgoing
    [,s (guard (symbol? s)) s]
    [(,ind ,s) (ASSERT (symbol? s)) (ASSERT (fixnum? ind)) s]))

;; NOTE: These two helpers will return #f if the dowstream/upstream
;; operator does not exist in ops.
(define (op->downstream op ops) 
  (ASSERT op)
  (let ((ls (ASSERT (assq 'outgoing (cdr op)))))
    (map (lambda (x) (lookup x ops)) 
      (map strip-index (cdr ls)))))
(define (op->upstream op ops)
  (ASSERT op)
  (let ([ls (ASSERT (assq 'incoming (cdr op)))])
    (map (lambda (x) (lookup x ops)) (cdr ls))
    ))

;; These just retrieve the *names*, not the actual operators
(define (op-outgoing op)
  (ASSERT (symbol? (car op)))
  (map strip-index (cdr (ASSERT (assq 'outgoing (cdr op))))))
(define (op-incoming op)
  (ASSERT (symbol? (car op)))
  (cdr (ASSERT (assq 'incoming (cdr op)))))

(define (op-code op)
  (let ([alist (if (symbol? (car op)) (cdr op) op)])
    (cadr (ASSERT (assq 'code (cdr op))))))

;; Takes only ops proper, not sources.
(define (op->annotations op)
  (match (op-code op)
    [#f '()] ;; cutpoints
    [(,_ (annotations ,annot ...) . ,__)     
     ;;(inspect annot)
     annot]
    [(inline_TOS . ,_) '()]
    [(inline_C . ,_) '()]
    [(__foreign_source . ,_) '()]
    ))

(define (op->inputtype op part)
  (match op
    [(iterate . ,rest) 
     (match (assq 'code rest)
       [(code (iterate ,ann (let ,binds (lambda ,args (,inty ,vqty) ,bod)) ,instrm))
	`(Stream ,inty)])]
    [(cutpoint . ,rest) (cadr (ASSERT (assq 'output-type rest)))]
    [(_merge . ,rest)   (cadr (ASSERT (assq 'output-type rest)))]
    [(unionN . ,rest) ;; Have to do some digging for this one.
     (match (cadr (ASSERT (assq 'output-type rest)))
       [(Stream (Struct ,name))
	(let* ([defs (cdr (ASSERT (project-metadata 'struct-defs part)))]
	       [entry (ASSERT (assq name defs))])
	  (match entry
	    ;; Finally!  Here it is:
	    [(,nm (,fld1 Int) (,fld2 ,intype)) intype])
	  )])]
    [(__readFile . ,rest) #()]))

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
	     [outgoing (map strip-index (cdr (assq 'outgoing (cdr op))))])
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
			  (ormap (lambda (name) (hashtab-get nametable name))
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
  ;; Return true if the operator is clean of anything that would force
  ;; it to be embedded:
  (define thisname "")
  (define (Operator Expr)
    (lambda (op)
      (ASSERT symbol? (car op))
      (let ([code (assq 'code (cdr op))])
	(ASSERT code)
	(fluid-let ([thisname (opname op)])
	  (Expr (cadr code))))))
  ;; This returns true if the code is "clean" -- if it doesn't
  ;; contain anything that would force it to be on the embedded node.
  [Expr (lambda (xp fallth)
	  (match xp ;; No match recursion!
	    [(,frgn . ,_)
	     (guard (eq-any? frgn 'foreign '__foreign 'foreign-app
			     ;; For now printing is disabled too:
			     'print
			     ))
	     (printf " ~a Disqualified based on: ~s\n" thisname frgn)
	     #f]

	    ;; TEMP FIXME: For now we dissallow state in mobile nodes!!
	    ;; Eventually, we want to allow state to migrate from
	    ;; node->server but not vice versa.

	    ;; [2008.04.08] Hacking this to allow it so we can at least look at the partitionings.
#|
	    [(iterate (annotations . ,annot) (let ,binds (lambda . ,_)) ,strm)
	     (if (null? binds) 
		 (fallth xp)		 
		 (begin
		   (printf "Disqualifying iterate based on state!! ~s \n" (assq 'name annot))
		   #f))]
	    [(iterate . ,_) (error 'refine-node-partition "missed iterate: ~s" xp)]
|#

	    [,oth (fallth oth)]))]
  [Fuser (lambda (ls k) (and-list ls))]
  [Program 
     (lambda (prog Expr)
       (match prog
	 [(,input-language 
	   '(graph (const ,cnst* ...)  (init  ,init* ...)
		   (sources ,src* ...) (operators ,_oper* ...)
		   (sink ,base ,basetype)	,meta* ...))
	  ;; Separate out the cutpoints right at the outset.
	  (define-values (cutpoints oper*) (partition cutpoint? _oper*))

	  ;; A lot of these op->downstream/upstream calls will return
	  ;; #f's due to those operators not being part of the partition we're looking at.
	  ;; That's ok for this particular pass.
	  (define (get-downstream x) (filter id (op->downstream x oper*)))
	  (define (get-upstream x)   (filter id (op->upstream   x oper*)))

	  ;; From each cutpoint, walk until we hit something that's not mobile:
	  (define (trace get-next startpoints cutpoint)
	    (ASSERT (curry andmap id) startpoints) ;; No #f's should have snuck through.
	    (if (null? startpoints) '()
		(begin 
		  (when (>= (regiment-verbosity) 3) (printf " Tracing from starts ~s\n " (map opname startpoints)))
		 (let loop ([tracepoints startpoints] ;; A work-list of trace-points.
			    [acc '()]) ;; Accumulates mobile operators.
		   (match tracepoints
		     [() (when (>= (regiment-verbosity) 3) (printf " finished.\n")) acc]
		     [(,head . ,tail)
		      (cond
		       [((Operator Expr) head)
			(when (>= (regiment-verbosity) 3) (printf " ~s" (opname head)))
			(loop (append (filter id (get-next head)) tail) (cons head acc))]
		       ;; Otherwise we stop the trace here, and continue processing the work-list.
		       [else		       
			(when (>= (regiment-verbosity) 3) (printf " | ~s  endpoint.\n Tracing: " (opname head)))
			;; When we reach a stopping point, put in a cutpoint:
			(loop tail  acc)])])))))

	  ;; Hack, making this work in both directions:
	  ;; (Thus we can use it for the node and server.)	  
	  (define mobile 
	    (append (trace get-downstream (apply append (map get-downstream cutpoints))
			   (lambda (in out) (make-cutpoint (optype in) (opname in) (opname out))))
		    (trace get-upstream   (apply append (map get-upstream cutpoints)) 
			   (lambda (out in) (make-cutpoint (optype in) (opname in) (opname out))))))
	  (define nodeops (difference oper* mobile))
	  ;; Returns #(floating stationary):
	  ;(define tagged (map (lambda (x) (tag-op '(floating) x)) mobile))
	  (vector
	   (reinsert-cutpoints	    
	    `(,input-language 
	      '(graph (const ,@cnst*) (init ,@init*) (sources) ;; All sources stay on the node for now.
		      (operators ,@mobile) ;(operators ,@tagged)
		      (sink #f #f) ,@meta*)))
	   (reinsert-cutpoints 	    
	    `(,input-language 
	      '(graph (const ,@cnst*) (init ,@init*) (sources ,@src*)
		      (operators ,@nodeops)
		      (sink #f #f) ,@meta*))))
	  ]))])

;; ==================================================
;;; Heuristics:

 ;; Unfinished
(define (min-bandwidth-heuristic name epochsize time datasize datafreq) 0)

(define (max-nodepart-heuristic name epochsize time datasize datafreq)
  (if (>= time epochsize) 0 time)) ;; Try to fill up the epoch
(define (min-nodepart-heuristic name epochsize time datasize datafreq)
  (- epochsize time)) ;; Minimize time, put everything possible on server.

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
	 (error 'exhaustive-partition-search "only works for a linear pipeline right now: cutpoints ~s sources ~s"
		(length cutpoints) (length realsources))))
     
     ;; HACK: IF we merge the real source in with an inline_TOS, then
     ;; let's scroll forward to the merge point:
     (define startpoint 
       (if (> (length src*) 1)
	   (begin 
	     (printf "(Hack) Scrolling forward start of search past inline_TOS.\n")
	     (let loop ([start (car realsources)])
	       (let ([down* (op->downstream start oper*)])
	       (ASSERT (andmap id down*)) 
	       (ASSERT (= 1 (length down*)))
	       ;; HACK: as soon as we find *A* merge, we take it... this isn't truly sufficient.
	       (if (eq? '_merge (caar down*))
		   (car down*)
		   (loop (car down*))))))
	   (car realsources)))

     (define datasize #f) ;; TODO: Hook up to scheme profiling!
     (define datafreq #f)
     (define initscore
       (heuristic (opname startpoint)
		  epochsize (op->time startpoint) datasize datafreq))

     (define ____ (begin (printf "Beginning search")(flush-output-port)))

     ;; Do a linear walk from source to cutpoint, scoring each potential cut.
     (define new-cutpoint
       (progress-dots
	(lambda ()
	  (let trace ([ptr startpoint]
		      [epochtime 0]
		      [curbest initscore]
		      [cur startpoint])
	    (define __ (ASSERT op-or-source? ptr))
	    (define datasize #f) ;; TODO: Hook up to scheme profiling!
	    (define datafreq #f)
	    ;; Evaluate the heuristic for this configuration:
	    (define score (heuristic (opname ptr) epochsize epochtime datasize datafreq))
	    (define-values (newscore newcur)
	      (if (> score curbest)	       
		  (values score   ptr)
		  (values curbest cur)))
	    (define downstream (filter id (op->downstream ptr oper*)))
	    (if (null? downstream)
		;; Otherwise we're all done, return the new cutpoint:
		newcur
		(begin
		  (ASSERT null? (cdr downstream)) ;; TEMP TEMP
		  (trace (car downstream) (+ epochtime (op->time downstream)) newscore newcur))
		)))))
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

;; Takes the "base" portion, the metadata, the init, and the constants
;; from p1...  Eventually we should probably break apart the constants
;; when splitting, in which case we would then need to also put them
;; back together here.
(define (merge-partitions p1 p2)
  ;; Must discard cutpoints obsoleted by the merge.
  (discard-spurious-cutpoints
   (match (vector p1 p2)
     [#((,input-language 
	 '(graph (const ,cnst* ...)  (init  ,init* ...)  (sources ,src* ...)
		 (operators ,oper* ...)  (sink ,base ,basetype) ,meta* ...))
	(,___
	 '(graph (const ,cnst2* ...) (init  ,init2* ...) (sources ,src2* ...)
		 (operators ,oper2* ...) (sink ,base2 ,basetype2) ,2meta* ...)))
      `(,input-language 
	'(graph (const ,@cnst*)  ; ,@cnst2*
		(init ,@init*) ; ,@init2*
		(sources ,@src* ,@src2*)
		(operators ,@oper* ,@oper2*)
		(sink ,(if base base base2) ,(if base basetype basetype2)) ,@meta*))])))

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
		   (trace (append down (cdr ptrs)) 
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

;; Print out the connectivity of a partition:
(define (print-partition part)
  (define table (make-default-hash-table))
  (define (maybe-deunique sym)
    (define result (deunique-name sym))
    ;; Have we already used this deuniqued name?
    (let ([entry (hashtab-get table result)])
      (cond 
       ;; If so, was it used for the same unique name?
       [(eq? entry sym) result]
       ;; Here we stick with the unique name:
       [entry sym]
       ;; Otherwise this is the first binding, set it:
       [else (hashtab-set! table result sym)
	     result])))
  (define (Name sym-or-false)
    (if sym-or-false
	(let ([str (symbol->string (maybe-deunique sym-or-false))])
	  ;; Hack for prettiness:
	  (if (and (> (string-length str) 5)
		   (equal? (substring str 0 5) "Node_"))
	      (substring str 5 (string-length str))
	      str))
	"<>"))
  (match part
    [(,input-language 
      '(graph (const ,cnst* ...)
	      (init  ,init* ...)
	      (sources ,src* ...)
	      (operators ,oper* ...)
	      (sink ,base ,basetype)	,meta* ...))     
     (for-each (lambda (src)
		 (printf " ~a (src) -> " (pad-width 10 (Name (opname src))))
		 (for-each (curry printf "~a ") (map Name (map strip-index (cdr (assq 'outgoing src)))))
		 (newline))
       src*)
     (for-each (lambda (op)
		 ;(unless (opname op) (inspect op))
		 (printf " ~a: " (pad-width 10 (Name (opname op))))		 
		 (for-each (curry printf "~a ") (map Name (op-incoming op)))		
		 (printf "  ->  " )
		 (for-each (curry printf "~a ") (map Name (op-outgoing op)))
		 (newline))
       oper*)]))


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
	 (define in (hashtab-get nametable (cadr (assq 'incoming rest))))
	 (define out (hashtab-get nametable (strip-index (cadr (assq 'outgoing rest)))))
	 ;; XOR: one must be in and the other out
	 (or (and in (not out))  (and (not in) out))]
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
	     [outgoing (map strip-index (cdr (ASSERT (assq 'outgoing op))))])
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
			(let ([intype (op->inputtype op part)])
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
  (define FUDGE-FACTOR  82) ;; The number of ticks assumed for the printf statements themselves.
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
      ;[(,oth . ,[rest]) rest]
      [(,oth . ,rest) (loop alist open strt rest)])))


(define (extract-java-time-intervals log)
  (define _ (when (string? log) (set! log (file->slist log))))

  (define FUDGE-FACTOR 0) ;; The number of ticks assumed for the printf statements themselves.
  (define (diff st en)
    (let ([elapsed (- en st FUDGE-FACTOR)])
      (max elapsed 0)))
  (reg:define-struct (rec name start total))

  (define (split sym ls)
    (split-before (lambda (x) (and (pair? x) (eq? (car x) sym))) ls))
   
  (define (extract-segment segment)
    (let loop ([stack '()] [acc '()] [log segment])
    (match log 
      [() (map (lambda (x) (list (rec-name x) (rec-total x))) acc)]
      ;; Open a new call:
      [((Start ,name ,tme) . ,rest)
       (define x (make-rec name tme 0))
       
       ;; End a segment for the enclosing operator
       (unless (null? stack) ;; null means the outer-emit. 
	 (let ([x (car stack)])
	   ;(printf "Adding segment to ~a : ~a ms start ~s total ~s ms\n" (rec-name x) (- tme (rec-start x)) (rec-start x) (rec-total x))
	   (set-rec-total! x (+ (rec-total x) (- tme (rec-start x))))
	   (set-rec-start! x #f)))
       
       (loop (cons x stack) (cons x acc) rest)]

      #;
      ;; End a segment, add to elapsed:
      ;; TODO: Need a FUDGE factor in here.
      [((Emit ,tme) ,rest ...)
       (unless (null? stack) ;; null means the outer-emit. 
	 (let ([x (car stack)])
	   ;(printf "Adding segment to ~a : ~a ms total ~s ms\n" (rec-name x) (- tme (rec-start x)) (rec-total x))
	   (set-rec-total! x (+ (rec-total x) (- tme (rec-start x))))
	   (set-rec-start! x #f)))
       (loop stack acc rest)]

      ;; Start the clock again on the top operator:
      #;
      [((Ret ,tme) ,rest ...)
       (unless (null? stack) (set-rec-start! (car stack) tme))
       (loop stack acc rest)]

      ;; Again, close the frame.
      ;; Pop the stack frame:
      [((End ,name ,tme) ,rest ...) 
       (define x (car stack))
       ;(printf " Adding segment to ~a : ~a ms start ~s total ~s ms\n" (rec-name x) (- tme (rec-start x)) (rec-start x) (rec-total x))
       (ASSERT (eq? name (rec-name (car stack))))
       (set-rec-total! x (+ (rec-total x) (- tme (rec-start x))))
       (set-rec-start! x #f)
       ;; As we pop the stack, we reset the timer:
       (unless (null? (cdr stack)) (set-rec-start! (cadr stack) tme))
       (loop (cdr stack) acc rest)]
            
      ;; Ignoring garbage for now:
      [(,oth . ,rest) (loop stack acc rest)]
      )))

    ;; Split out the first start/end traverse pair:
#;
  (define first-seg 
    (let-values ([(_ tail) (split 'StartTraverse log)])
      (let-values ([(mid end) (split 'EndTraverse tail)])
	;(append mid (list (car end)))
	(cdr mid) ;; Throw out the start and end markers.
	)))

  (define segments
    (let loop ([ls log])
      ;(printf "looping... ~s\n" (length ls))
      (if (null? ls) '()
	  (let-values ([(_ tail) (split 'StartTraverse ls)])
	    ;(printf "  remprefix ~a ~a ~a\n" (car tail) (length _) (length tail))
	    (let-values ([(mid end) (split 'EndTraverse tail)])
	      ;(printf "  mid ~a ~a ~a\n" (car end) (length mid) (length end))
	      (cons (cdr mid) (loop end)))
	    ))))
  
  ;; Collect all results:
  (define table (make-default-hash-table))
  
  (for-each 
      (lambda (seg)
	(for-each (lambda (entry)
		    (let ([rec (hashtab-get table (car entry))])
		      (if rec
			  ;; Extend the entry:
			  (set-cdr! rec (cons (cadr entry) (cdr rec)))
			  (let ([rec (list-copy (cdr entry))])			    
			    (hashtab-set! table (car entry) rec)))))
	  (extract-segment seg)))
    segments)
  
  ;; Full report:
  #;
  (inspect   
   (sort 
   (lambda (sexp1 sexp2)
     (define selected 'median)
     ;(define selected 'mean)
     (< (cadr (assq selected (cdr sexp1)))
	(cadr (assq selected (cdr sexp2)))))
   (map (lambda (entry)
	  (define nums (cdr entry))
	 `(,(car entry) 
	   (mean ,(exact->inexact (average nums)))
	   (median ,(median nums))
	   (stddev ,(stddev nums))
	   (raw ,(sort < nums))))
     (hashtab->list table))))
  
  ;; Just medians:
  (sort (lambda (e1 e2) (> (cdr e1) (cdr e2)))
	(map (lambda (entry)
	       (cons (car entry) 
		     ;(median (cdr entry))
		     (exact->inexact (average (cdr entry)))
		     ))
	  (hashtab->list table)))
  )


;; Inserts a (measured-cycles <elapsed> <timer-frequency>) annotation:
;; [2008.04.12] Takes either a program or a graph.
(define (inject-times prog times timerfreq)
  (define (Operator op)
    (match op
      [(iterate (name ,nm) ,ot
		(code (iterate (annotations ,annot* ...) ,itercode ,_))
		,rest ...)
       ;(printf "LOOKING UP ~a in ~a result ~a\n" nm times (assq nm times))
       (let ([entry (assq nm times)])		     
	 (if entry
	     `(iterate (name ,nm) ,ot 
		       (code (iterate (annotations (measured-cycles ,(cdr entry) ,timerfreq) ,@annot*)
				      ,itercode ,_))
		       ,@rest)
	     op))]
      [,oth oth]))
  (define-pass regularprog
      [Expr (lambda (xp fallthru)
	      (match xp
		[(iterate (annotations ,annot* ...) ,[fun] ,[strm])
		 `(iterate (annotations 
			    ,@(let* ([name (cadr (ASSERT (assq 'name annot*)))]
				     [entry (assq name times)])
				(if entry
				    `((measured-cycles ,(cdr entry) ,timerfreq))
				    '()
				    ))
			    ,@annot*)
			   ,fun ,strm)]
		[,oth (fallthru oth)]))])
  (match prog
    [(,input-language 
      '(graph ,cnst ,init ,src
	      (operators ,[Operator -> oper*] ...) ,rest ...))
     `(,input-language 
       '(graph ,cnst ,init ,src
	       (operators ,oper* ...) ,rest ...))]
    [,else (regularprog prog)]))

;; HACK: duplicating... should do something nicer.
(define (inject-assignments prog assignments)
  (define (Operator op)
    (match op
      [(iterate (name ,nm) ,ot
		(code (iterate (annotations ,annot* ...) ,itercode ,_))
		,rest ...)
       ;;(printf "LOOKING UP ~a in ~a\n" nm assignments)
       (let ([entry (assq nm assignments)])
	 (if entry
	     `(iterate (name ,nm) ,ot 
		       (code (iterate (annotations (node/server-assignment ,(cadr entry)) ,@annot*)
				      ,itercode ,_))
		       ,@rest)
	     (begin
	       ;(inspect (vector op assignments))
	       op
	       )))]
      [,oth oth]))
  (match prog
    [(,input-language 
      '(graph ,cnst ,init ,src
	      (operators ,[Operator -> oper*] ...) ,rest ...))
     `(,input-language 
       '(graph ,cnst ,init ,src
	       (operators ,oper* ...) ,rest ...))]))




;; This is ugly: 
(define-regiment-parameter max-tinyos-nodes 10)

;; This goes over the whole graph and multiplexes only those operators
;; that have migrated from node to server.
#;(define-pass multiplex-migrated
  (define (Operator op)
    (if (assq 'NODEOP? (op->annotations op))	
	(multiplex-operator op)
	op))
  [Program
   (lambda (prog Expr)
     (match prog
       [(,input-language 
	 '(graph (const ,cnst* ...) (init  ,init* ...) (sources ,src* ...)
		 (operators ,oper* ...)
		 (sink ,base ,basetype)	,meta* ...))
	`(,input-language 
	  '(graph (const ,cnst* ...) (init ,@init*) (sources ,@src*)
		  (operators ,@(map Operator oper*))
		  (sink ,base ,basetype)
		  ,@meta*))
	]))])

  ;; The current system for distributed programming replicates the
  ;; "Node" partition across all nodes in the network.  These streams
  ;; are implicitly multiplexed at the point where they cross into the
  ;; non-Node part of the program.  However, when we adjust the
  ;; partitioning, moving "Node" operators onto the server, they then
  ;; must be multiplexed to simulate N such operators running on
  ;; different nodes.
  ;;
  ;; In other words, each such operator must have its state duplicated
  ;; in a table.  Input and output streams are augmented with a node ID.
  ;; The nodeID is thrown away when we make it to the Server partition.
;;
;; This is parameterized by the tag that distinguishes node-originated operators.
(define (multiplex-migrated tag prog)
  ;(define struct-defs (project-metadata 'struct-defs prog))

  (define type-table '()) ;; Mutated below

  ;; NOTE: this doesn't currently try to reuse struct-defs that are already present.
  (define (get-struct-type! . fldtypes)
    (define entry (assoc fldtypes type-table))
    (or entry
	(let* ([name (unique-name "mplextupty")]
	       [new (cons fldtypes name)])
	  (set! type-table (cons new type-table))
	  new)))
  
  ;; Adding another argument that indicates whether we should pass the
  ;; node-id on through to the next operator down the line.
  (trace-define (multiplex-operator op output-multiplexed?)
      (define NodeIDTy 'Int)  
    (match op
      [(iterate (name ,name) 
		(output-type ,ty)
		(code 
		 (iterate (annotations ,an* ...) 
			  (let ([,lhs* ,ty* ,rhs*] ...)
			    (lambda (,x ,vq) (,xty (VQueue ,vqty)) ,bod))
			  ,strm))
		(incoming ,up)
		(outgoing ,down* ...))
       ;; We want to remain agnostic to the number of nodes in the
       ;; network, so each state variable becomes a (growable) hash
       ;; table mapping node-id to values.
       #;
       (define newstate
	 (map (lambda (ty rhs) `(HashTable:make '100))
	   ty* rhs*))

       ;; The code for initializing state must run anew each time we hit a new node:
					;(define genstate)
       
       ;; [2008.03.29] For now we don't have hash tables in the wsc2
       ;; backend, so instead we just keep an array and we assume the
       ;; node IDs are restricted to the range 0 to lengthOfArray-1.
       (define newrhs*
	 (map (lambda (ty rhs) 
		`(vector ,@(make-list (max-tinyos-nodes) rhs)))
	   ty* rhs*))
       (define newty* (map (lambda (ty) `(Array ,ty)) ty*))

       (define nid (unique-name "nodeID"))
       (define inp (unique-name "newinput"))
       
       ;; Sadly, this transform currently happens AFTER
       ;; nominalize-types.  So we have to bend over backwards here to
       ;; pass *structs* rather than tuples.
       (define INSTRUCT  (get-struct-type! NodeIDTy xty))
       (define OUTSTRUCT (get-struct-type! NodeIDTy vqty))
       (define fld0 (list-ref standard-struct-field-names 0))
       (define fld1 (list-ref standard-struct-field-names 1))

       ;; Also need to replace all emits so they tag the node-ID on!!!
       (trace-define fix-emits 
	 (core-generic-traverse
	  (lambda (xp fallthru)
	    (match xp
	      [(emit ,vq ,x) 
	       (define tmp (unique-name "emtmp"))
	       `(let ([,tmp (Struct ,OUTSTRUCT) (make-struct ,OUTSTRUCT ,nid ,x)]) (emit ,vq ,tmp))]
	      [,oth (fallthru oth)]))))

       `(iterate (name ,name) (output-type ,ty)
		 (code (iterate (annotations ,@an*) 
				(let ,(map list lhs* newty* newrhs*)
				  (lambda (,inp ,vq) (,xty (VQueue ,vqty))
					  (let ([,x ,xty (struct-ref ,INSTRUCT ,fld1 ,inp)])
					    (let ([,nid ,NodeIDTy (struct-ref ,INSTRUCT ,fld0 ,inp)])
					      ,(if output-multiplexed? (fix-emits bod) bod)))))
				,strm))
		 (incoming ,up)  (outgoing ,down* ...))]
      [(_merge . ,_)  op]
      [(cutpoint . ,_) op]    
      [(__readFile . ,_) op]))
  (define (Operator op)
    (if (assq tag (op->annotations op))	
	(multiplex-operator op #t)
	op))


  ;(map-partition-ops Operator prog)
  (match prog
    [(,input-language 
      '(graph (const ,cnst* ...) (init  ,init* ...) (sources ,src* ...)
	      (operators ,oper* ...)
	      (sink ,base ,basetype)	,meta* ...))
     `(,input-language 
       '(graph (const ,cnst* ...) (init ,@init*) (sources ,@src*)
	       (operators ,@(map Operator oper*))
	       (sink ,base ,basetype)

	       ;;;;;;; FINISH ME !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	       ,@(assq-remove-all 'struct-defs meta*)))])

)


;; It's annoying to deal with big unweildy sexpressions.  This spits
;; out a simple graph as a list-of-lists.  Each inner list contains
;; the edges for a single vertex: (src dest ...)
(define (partition->simple-graph part)
  (define table (make-default-hash-table))
  (match part
    [(,input-language 
      '(graph (const ,cnst* ...)
	      (init  ,init* ...)
	      (sources ,src* ...)
	      (operators ,oper* ...)
	      (sink ,base ,basetype)	,meta* ...))     
     (append 
      (map (lambda (src) (cons (opname src) (map strip-index (cdr (assq 'outgoing src)))))
	src*)
      (apply append 
	     (map (lambda (op) 
		    (define name (opname op))
		    (if name ;; cutpoints don't have names:			
			(list (cons name (op-outgoing op)))
			'()))
	       oper*)))]))


(define (partition->src&ops part)
  (match part
    [(,input-language 
      '(graph (const ,cnst* ...)
	      (init  ,init* ...)
	      (sources ,src* ...)
	      (operators ,oper* ...)
	      (sink ,base ,basetype)	,meta* ...))
     (append src* oper*)]))


(define (partition->frequency part)
  (let* ([opsrcs (partition->src&ops part)]
	 [src* (filter (lambda (x) (not (symbol? (car x)))) opsrcs)]
	 [rlsrc* (filter not-inline_TOS? src*)])
    
    (match rlsrc*
      [((,_ ,__ (code (__foreign_source ',fn '(,rate . ,rest) ',ty)) . ,___))  
       (string->number rate)]

      [((,_ ,__ (code (timer ,ann  ',rate)) . ,___))  rate]
      )))

;; Spit out a .lp file describing the pure integer linear program that will solve the partitioning problem.
(define (emit-lp nodepart floating serverpart)
  ;; Build a table of annotations
  (define annot-table
    (let ([tab (make-default-hash-table 1000)])
      (for-each (lambda (op/src)
		  (and ;(if (not (symbol? (car op/src))) (inspect op/src) #t) ;; Verify that op/src is not source.
		       (hashtab-set! tab (opname op/src) (op->annotations op/src))))
	(append (partition->src&ops nodepart)
		(partition->src&ops floating)
		(partition->src&ops serverpart)))
      tab))
  
  ;; Retrieve the input frequencies so we can translate execution *times* into percent cpu.
  ;(define input-frequency (ASSERT number? (partition->frequency nodepart)))
  ;; HACK FIXME!  We assume that the frequency that the PROFILED version was driven at is the correct one.
  (define input-frequency 
	  (let ([tmp (project-metadata 'profiled-input-frequencies nodepart)])
	    (ASSERT (= 2 (length tmp)))
	    (ASSERT number? (cadr tmp))))
  ;; DUPLICATED CODE: DIG OUT THE RELEVANT EPOCH INFO:
  (define (profiling-duration)
    (match (ws-profile-limit)
      [(virttime ,vt) vt]
      [,oth (error 'graphviz "could not determine the Scheme profile duration from this setting of ws-profile-limit: ~s" 
		   (ws-profile-limit:))]))

  (define g1 (partition->simple-graph nodepart))
  (define g2 (partition->simple-graph floating))
  (define g3 (partition->simple-graph serverpart))
  (define leftnames  (map car g1))
  (define rightnames (map car g3))
  ;; We introduce variables with the same names as the nodes.
  (define vars (map car g2))
  ;; Also add in edges from the node that go *into* the floating part:
  (define incoming-edges (filter (lambda (row) (not (null? (intersection (cdr row) vars)))) g1))

  ;; Sometimes we don't have edge-rate data!  What should we do??
  (define default-edge-weight (* 10 1000 1000)) ;; 9999
  (define default-vert-weight 1)
  (define cpu-granularity 1000)  ;; Doing a centi-percent.

  (define (Var v) 
    (cond 
     [(memq v leftnames)  1]
     [(memq v rightnames) 0]
     [else  v]))

  ;;------------------------------
  (define cpu-total (number->string cpu-granularity))
  (define (cpu-coeff vr)
    (+ 0     ;; HACK FIXME!!! ADDING to it temporarily to artificially stress the LP solver:
       (max 0		 
	 ;; If the TinyOS numbers are there, use them.
	 (let  ([entry (assq 'measured-cycles (hashtab-get annot-table vr))])
	   (if entry
	       ;; TinyOS/Java Case:
	       ;; FIXME INCORRECT: CURRENTLY ASSUMING 1 SECOND EPOCH:
	       
	       ;; Percent cpu is measured / alloted, where alloted is clock rate / freq
	       (let ([frac (/ (cadr entry) (/ (caddr entry) 
					      ;input-frequency
					      ;; HACKING THIS, we don't profile at the real frequency!!
					      ;(/ 8000 200)
					      ))])
		 (printf "  FRAC: ~a ~a ~a  -> ~a\n" (cadr entry) (caddr entry) input-frequency frac)
		 (inexact->exact (floor (* frac cpu-granularity))))
	       
	       ;; Scheme cpu weights mode:
	       (let ([entry (assq 'data-rates (hashtab-get annot-table vr))])
		 ;(printf "Using Scheme execution times to formulate LP.\n")
		 (if entry
		     (let ([elapsed-vtime 
			    (match (ws-profile-limit)
			      ;; How many ms did we run scheme for:
			      ;; This is real time:
			      ;[(time ,ms)  ms]
			      ;;[(elements ,n) ]
			      [(virttime ,vt) 
			       vt
			       ;; HACK FIXME:
			       ;;3000
			       ;1000
			       ] ;; (* 1000 vt)
			      )])
		       (let ([frac (/ (bench-stats-cpu-time (caddr entry)) elapsed-vtime)])
			 (printf "  TIME ~s ELAPSED ~s RATIO ~s\n"
				 (bench-stats-cpu-time (caddr entry)) elapsed-vtime (exact->inexact frac))
			 (inexact->exact (floor (* frac cpu-granularity)))))
		     default-vert-weight))
	       )))))
		      
  ;; TinyOS cpu weights:
  ;;------------------------------
  #;
  (define (cpu-coeff vr) 
    (max 0 
	))

  (printf "~s ops in floating partition.\n" (length vars))
  (printf "Left names: ~s\n" leftnames)
  (printf "Floating names: ~s\n" vars)
  (printf "Right names: ~s\n" rightnames)
  (if (null? vars) ;; We can't make an LP for this.
      (error 'emit-lp "Cannot generate LP formulation with ZERO mobile operators!")
  (apply string-append
   (append
    (list
     (format "// ~s Vars Total\n" (length vars))
     "\n// Minimize bandwidth of cut:\n"      
     "min: "
     (apply string-append
       (insert-between "\n    + "
	    (apply append 
		   (map (lambda (row) 
			  (define src (car row))
			  (define _src (Var src))			  
			  (define edgecoeff 
			    (max 0 
			      (let ([entry (assq 'data-rates (hashtab-get annot-table (car row)))])
				(if entry
				    (inexact->exact
				     (round
				      (* (/ (bench-stats-bytes (caddr entry)) 
					    (bench-stats-tuples (caddr entry)))
					 input-frequency)))
				    default-edge-weight))))
			  (define left (memq src leftnames))
			  
			  ;; HACK: scheme profiling misses the first edge:
			  ;; Set it to something big:
			  (if (zero? edgecoeff) (set! edgecoeff default-edge-weight))
			  
			  (map (lambda (dst)
				 (define right (memq dst rightnames))
				 ;; Lamely, the file format can't handle const * const:
				 (cond
				  [(and left right) (format "~a " edgecoeff)]
				  [right  (format "~a ~a " edgecoeff _src)] ;; zero coefficient
				  [left   (format "~a - ~a ~a" edgecoeff edgecoeff (Var dst))] ;; one coefficient
				  [else   (format "~a ~a - ~a ~a " edgecoeff _src edgecoeff (Var dst))]))
			    (cdr row)))
		     (append incoming-edges g2)))))
     ";\n")

    '("\n// Make them binary (should be able to do this with an bin x; assert):\n")
    (map (lambda (name) (format "0 <= ~a <= 1;\n" name)) vars)
    '("\n// Require that we only cut each path once:\n")
    (apply append
     (map (lambda (row) 
	   (define src (Var (car row)))
	   (map (lambda (dst)
		  (if (memq dst rightnames) 
		      ;; Optimization, don't bother with this constraint:
		      "" ;; var - 0 >= 0   is redundant
		      (format "~a - ~a >= 0;\n" src (Var dst))))
	     (cdr row)))
       (append incoming-edges g2)))

    (list 
     "\n// Require that CPU sums to less than 100%:\n"
     (apply string-append
	    cpu-total " >= "
	    (insert-between "\n    +  "
	      (map (lambda (vr)		 		     
		     ;; Need coefficients:		 
		     (format " ~a ~a " (cpu-coeff vr) (Var vr)))
		vars)))
     ";\n")

    (list
     "\n// Make all vars integer (should be binary)\n"
     (apply string-append 
	     "int " (insert-between ", " 
				   (map (curry format "~a")
				     (filter (lambda (x) (not (memq x '(0 1))))
				       (map Var vars)))))
     ";\n")

    ))))

#|
min: x1 + x2;
x1 >= 1;
x2 >= 1;
myrow: x1 + x2 >= 2;

int x1;
// bin x1;
|#

			  
;; Returns two values: the value of the objective function, and a
;; [<var> <value>] association list.
(define (read-back-lp-results file)
  (define results (file->string "partition_assignments.txt"))
  (match (string->slist results)
    [(Value of objective function: ,objective
	    Actual values of the variables: 
	    ,rest ...)
     (define (everyother ls)
       (cond
	[(null? ls) '()]
	[(null? (cdr ls)) ls]
	[else (cons (car ls) (everyother (cddr ls)))]))
     (define names (everyother rest))
     (define vals (everyother (cdr rest)))
     (values objective (map list names vals))]))


;; These are helpers to emit-lp.  They prune out artifical
;; partitions consisting of just the source or sink operators.
(define (partition-sourcesonly part)
  (match part
    [(,input-language 
      '(graph (const ,cnst* ...)
	      (init  ,init* ...)
	      (sources ,src* ...)
	      (operators ,oper* ...)
	      (sink ,base ,basetype)	,meta* ...))
     `(,input-language 
       '(graph (const ,cnst* ...)
	       (init  ,init* ...)
	       (sources ,src* ...)
	       (operators)
	       (sink ,#f ,#f)	,meta* ...))]))
(define (partition-baseonly part)
  (match part
    [(,input-language 
      '(graph (const ,cnst* ...)
	      (init  ,init* ...)
	      (sources ,src* ...)
	      (operators ,oper* ...)
	      (sink ,base ,basetype)	,meta* ...))
     (define filtered (filter (lambda (op) (eq? (opname op) base)) oper*))
     (ASSERT (not (null? filtered)))
     `(,input-language 
       '(graph (const ,cnst* ...)
	       (init  ,init* ...)
	       (sources ,src* ...)
	       (operators ,@filtered)
	       (sink ,#f ,#f)	,meta* ...))]))
(define (partition-getmiddle part)
  (match part
    [(,input-language 
      '(graph (const ,cnst* ...)
	      (init  ,init* ...)
	      (sources ,src* ...)
	      (operators ,oper* ...)
	      (sink ,base ,basetype)	,meta* ...))
     (define filtered (filter (lambda (op) (not (eq? (opname op) base))) oper*))
     (ASSERT (< (length filtered) (length oper*)))
     `(,input-language 
       '(graph (const ,cnst* ...)
	       (init  ,init* ...)
	       (sources)
	       (operators ,@filtered)
	       (sink ,#f ,#f)	,meta* ...))]))


;; ================================================================================
;; Unit tests

;; These use quite a bit of stuff from type_environments.ss as well:
(define-testing these-tests
  `(

  ))

;; Unit tester.
(define test-partition-graph 
  (default-unit-tester "Partitioning Node/Server programs" these-tests))


) ;; End module
