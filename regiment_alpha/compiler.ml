
type var = string

type prim = Rmap | Smap 
	    | Circle | CircleAt
	    | Cons | Plus | Minus | Times | Divide

type expr = 
    	  | Imm of int
	  | Var of var
	  | Lambda of ((var list) * expr)
	  | Let of ((var * expr) * expr)
	  | Primapp of (prim * (expr list))
	  | If of (expr * expr * expr)

type core = Pgm of expr


let x = Pgm (Let (("r", Primapp 
		     (CircleAt,
		      [Imm 50; 
		       Primapp (Cons, [Imm 30; Imm 40])])),
		  Var "r"));;       	



(*
(let* ((R (circle-at 50 '(30 40)))
       (f (lambda (tot next)
	    (cons (+ (car tot) (sense next))
		  (+ (cdr tot) 1))))
       (g (lambda (tot) (/ (car tot) (cdr tot))))
       (avg (smap g (rfold f (cons 0 0) R))))
  (until (pred (lambda (x) (> x 15.3)) avg)
	 R
	 (circle-at 100 '(0 0))))


*)
