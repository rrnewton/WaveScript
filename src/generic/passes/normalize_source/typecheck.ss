
;;;; This is a pass that invokes the typechecker.


;; This is simply used between subsequent passes to verify the intermediate programs.
(define-pass retypecheck
    [Program (lambda (prog _)
	       ;; Only actually do it in debugmode:	      
	       (IFDEBUG (annotate-program prog)
			prog))])


#;
(retypecheck '(lift-letrec-language
  '(program
     (lazy-letrec
       ((resultoftoplevel_2 (Signal Int) (rdump tmprmap_5))
         (tmprmap_5 (Area Int) (rmap tmpnonprim_4 tmpworld_3))
         (tmpnonprim_4
           (Node -> Int)
           (lambda (a_1)
             (Node)
             (lazy-letrec
               ((resultofanonlambda_1 Int (nodeid a_1)))
               resultofanonlambda_1)))
         (tmpworld_3 Region world))
       resultoftoplevel_2)
     (Signal Int))))


; (lift-letrec-language
;   '(program
;      (lazy-letrec
;        ((resultoftoplevel_2
;           '(cwp quote
;                 (cwr Signal #0='(cwq . #1='(cww quote (cwx . Int)))))
;           (rdump tmprmap_5))
;          (tmprmap_5 '(cwo Area #0#) (rmap tmpnonprim_4 tmpworld_3))
;          (tmpnonprim_4
;            '(cwn #2='(cwt . #3='(cwv . Node)) -> '(cws . #0#))
;            (lambda (a_1)
;              (#3#)
;              (lazy-letrec
;                ((resultofanonlambda_1 #1# (nodeid a_1)))
;                resultofanonlambda_1)))
;          (tmpworld_3 '(cwm Area #2#) world))
;        resultoftoplevel_2)
;      (Signal Int)))

#;
(define test-this   
  (default-unit-tester "retypecheck: redo typechecking to verify compilation"
`(
  
  (retypecheck 
   '(lang (lambda (reg_5)
      ((Area Node))
      (lazy-letrec
       ((thevals_6
	 (Area (List Int))
	 (rmap tmpnonprim_13 reg_5))
	(resultofsumhood_3
	 (Signal (List Int))
	 (rfold tmpnonprim_14 '() thevals_6))
	(tmpnonprim_13
	 (Node -> (List Int))
	 (lambda (n_7)
	   (Node)
	   (lazy-letrec
	    ((resultofthevals_1
	      (List Int)
	      (cons tmpbasic_12 '()))
	     (tmpbasic_12 Int (nodeid n_7)))
	    resultofthevals_1)))
	(tmpnonprim_14
	 ((List Int) (List Int) -> (List Int))
	 (lambda (a_9 b_8)
	   ((List Int) (List Int))
	   (lazy-letrec
	    ((resultofanonlambda_2
	      (List Int)
	      (append a_9 b_8)))
	    resultofanonlambda_2))))
       resultofsumhood_3)) Int))

#;
  ["Caputured bug: types generalize incorrectly:"
   (cadr (deep-assq 'sumhood_1
	      (retypecheck '(lift-letrec-language
	       '(program
		    (lazy-letrec
		     ((nodes_4 (Area Node) (rfilter tmpnonprim_10 tmpworld_8))
         (onehop_3
           (Node -> Region)
           (lambda (n_10)
             (Node)
             (lazy-letrec
               ((resultofonehop_4 Region (khood tmpnodeanchor_11 '1))
                 (tmpnodeanchor_11 Anchor (node->anchor n_10)))
               resultofonehop_4)))
         (nbrhoods_2 (Area Region) (rmap onehop_3 nodes_4))
         (sumhood_1
           ((Area Node) -> (Signal (List Int)))
           (lambda (reg_5)
             ((Area Node))
             (lazy-letrec
               ((thevals_6
                  (Area (List Int))
                  (rmap tmpnonprim_13 reg_5))
                 (resultofsumhood_3
                   (Signal (List Int))
                   (rfold tmpnonprim_14 '() thevals_6))
                 (tmpnonprim_13
                   (Node -> (List Int))
                   (lambda (n_7)
                     (Node)
                     (lazy-letrec
                       ((resultofthevals_1
                          (List Int)
                          (cons tmpbasic_12 '()))
                         (tmpbasic_12 Int (nodeid n_7)))
                       resultofthevals_1)))
                 (tmpnonprim_14
                   (#0=(List Int) #1=(List Int) -> (List Int))
                   (lambda (a_9 b_8)
                     (#0# #1#)
                     (lazy-letrec
                       ((resultofanonlambda_2
                          (List Int)
                          (append a_9 b_8)))
                       resultofanonlambda_2))))
               resultofsumhood_3)))
         (resultoftoplevel_7
           (Area Int)
           (rmap tmpnonprim_17 tmpliftsig_16))
         (tmpnonprim_10
           (Node -> Bool)
           (lambda (n_11)
             (Node)
             (lazy-letrec
               ((resultofnodes_5 Bool (= tmpbasic_9 '1))
                 (tmpbasic_9 Int (nodeid n_11)))
               resultofnodes_5)))
         (tmpworld_8 Region world)
         (tmpnonprim_17
           (#2=(List Int) -> Int)
           (lambda (x_12)
             (#2#)
             (lazy-letrec
               ((resultofanonlambda_6 Int '333))
               resultofanonlambda_6)))
         (tmpliftsig_16 (Area (List Int)) (liftsig tmprmap_15))
         (tmprmap_15
           (Area (Signal (List Int)))
           (rmap sumhood_1 nbrhoods_2)))
       resultoftoplevel_7)
     (Area Int))))))
   ;((Area 'baw) -> (Signal (List 'bce)))
   ((Area Node) -> (Signal (List Int)))
   ]

  )))



