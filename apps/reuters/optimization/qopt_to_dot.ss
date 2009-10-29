#! /bin/bash
#|
exec regiment.chez i --script $0 $*
exec regiment.plt i --script $0 $*
|#

(define port
  (if (= (length (command-line)) 6)
      (open-input-file (rac (command-line)))
      (current-input-port)))

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
	 (for-each (lambda (dst) 
		     (printf "    ~a -> ~a [label=\" ~a\"];\n" src dst bw))
	   dst*)]

	[(pin ,op ,node)           (printf "   ~a [shape=box, style=rounded];\n" op)]
	[(op ,name   cpu ,cpucost) (printf "   ~a [shape=box];\n" name)]

	[(node ,name cpu ,cpucap)  
	 (printf "  ~a [label=\"~a\"];\n" name name)
	 ]

	[(link ,src bw ,band lat ,latency <-> ,dst)
	 (printf "  ~a -> ~a [label=\"bw ~a\\nlat ~a\"];\n" src dst band latency)
	 (void)]

	[(query ,name ,opnames ...)
	 (void)]
	
	[(network ,name* ...) 
	 (void)]
	))
  (port->linelists port)
  )

(printf "}\n")
