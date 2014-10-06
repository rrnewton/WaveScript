
;; Thinking about what it takes to implement arbitrarily deep Region types.

(define lift (lambda (n) (khood (node->anchor n) 2)))
(define lift2 (lambda (r) (rmap lift r)))

(define reg1 world)
(define reg2 (rmap lift world))
(define reg3 (rmap lift2 reg2))

;; Reg3 is an Area (Area (Area Node)).

;; At runtime regions are represented by tokens in the form <Tok, Id>.
;; Therefore, Id has to be chosen in such a way as to guarantee that
;; there are no "collisions" between gradients which are really
;; supposed to be separate gradients.  

;; The obvious thing to do for these chains of khoods, is use *all*
;; relevent node-ids (yours, your parents, etc) as the token index.
;; This also corresponds to the semantic type, which is essentially
;; Time -> Space -> Space ... -> Val.  Each of the "Space"s means
;; keeping track of a node-id from a node in the network (a "place").

;; However, this state of affairs is cumbersome because it makes it
;; impossible to bound the bitwidth of the subtoken indices, which I
;; had wanted to be small, ideally.

;; Can we do better?

(define rrmap 
  (lambda (f rr)
    (rmap (lambda (r) (rmap f r)) rr)))
(define rrrmap 
  (lambda (f rrr)
    (rrmap (lambda (r) (rmap f r)) rrr)))

(define crfold (lambda (f v) (lambda (r) (rfold f v r)))) ; Curried

(define vals (rrrmap sense reg3))
(define sigs (rrmap (crfold + 0) vals))

sigs

;; Well, one idea is that if we're sure we need storage proportional
;; to N, where N is the number of times "Area" occurs in our type,
;; then we just need to ensure that there are N tokens involved, each
;; with a fixed-bitwidth index.

;; By a counting argument, we should be able to show that a very large
;; number of trees can be created that are all rooted at the same node
;; in the network, and associated with the same code location.
;; ("Minted" in the same place.)

;; LOC1: (khood (node->anchor n) 2)
;; LOC2: (rmap lift r)
;; LOC3: (rmap lift2 reg2)

;; To achieve the goal of distributing storage over multiple tokens
;; rather than loading it on the index, we should consider
;; optimizations that reuse trees rooted at node, while storing
;; "parent" information in separate tokens so as to be able to 




; (annotate-program (mvlet (((a b) (read-wavescript-source-file "demos/drawing_board/double_nested.rs"))) a))
; (export-type (type-expression (mvlet (((a b) (read-wavescript-source-file "demos/drawing_board/double_nested.rs"))) a) ()))
