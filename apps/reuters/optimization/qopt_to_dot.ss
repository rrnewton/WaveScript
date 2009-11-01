#! /bin/bash
#|
exec regiment.chez i --script $0 $*
exec regiment.plt i --script $0 $*
|#

(define port
  (if (= (length (command-line)) 6)
      (open-input-file (rac (command-line)))
      (current-input-port)))


;;              CAREFUL!
;; [2009.11.01] This is dangerous because we might cause COLLISIONS!
;;
;; Some characters are not acceptable:
;; Type: Symbol -> String
(define (clean-name s)
  ;; A table of replacements
  (define bad-chars  (list->hashtab '((#\- #\_))))
  (list->string 
   (map (lambda (c) 
	  (or (hashtab-get bad-chars c) c))
     (string->list (symbol->string s))))
  )

; digraph Foo {
;   COUNTUP_8 -> COUNTUP_7;
;   interleaveSS_1 -> BASE;
;   interleaveSS_2 -> interleaveSS_1;
;   fn_3 -> interleaveSS_2;
;   fn_4 -> interleaveSS_2;
;   fn_5 -> interleaveSS_2;
;   window_6 -> fn_5;
;   window_6 -> fn_4;
;   window_6 -> fn_3;
;   COUNTUP_7 -> window_6;

;   COUNTUP_8 [label="COUNTUP_8"];
;   COUNTUP_7 [label="COUNTUP_7"];
;   window_6 [label="window_6"];
;   fn_5 [label="fn_5"];
;   fn_4 [label="fn_4"];
;   fn_3 [label="fn_3"];
;   interleaveSS_2 [label="interleaveSS_2"];
;   interleaveSS_1 [label="interleaveSS_1"];
; }
(printf "digraph QueryAndOrNetwork {\n")

(for-each
    (lambda (entry)
      (match entry
	[() (void)]
	[(edge ,src bw ,bw  -> ,dst* ...)
	 (define csrc (clean-name src))
	 (for-each (lambda (dst) 
		     (printf "    ~a -> ~a [label=\" ~a\"];\n" csrc (clean-name dst) bw))
	   dst*)]
	
	[(pin ,op ,node)           (printf "   ~a [shape=box, style=rounded];\n" (clean-name op))]
	[(op ,name   cpu ,cpucost) (printf "   ~a [shape=box];\n" (clean-name name))]

	[(node ,name cpu ,cpucap)  
	 (define clean (clean-name name))
	 (printf "  ~a [label=\"~a\"];\n" clean clean)]

	[(link ,src bw ,band lat ,latency <-> ,dst)
	 (printf "  ~a -> ~a [label=\"bw ~a\\nlat ~a\"];\n" (clean-name src) (clean-name dst) band latency)
	 (void)]

	[(query ,name ,opnames ...)
	 (void)]
	
	[(network ,name* ...) 
	 (void)]
	))
  (port->linelists port)
  )

(printf "}\n")
