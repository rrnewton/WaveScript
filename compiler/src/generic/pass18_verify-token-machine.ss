;;; [2004.06.28]
;;; NOTE: for the moment 

;;; Input and Output language:

;;;  <Pgm> ::= (program (bindings <Decl>*) <SOCPgm> <NodePgm>)
;;;  <SOCPgm> ::= <Statement*>
;;;  <NodePgm> ::= (nodepgm <Entry> (bindings <Decl>*) (tokens <TokBinding>*))
;;;  <Entry>  ::= <Token>
;;;  <Decl> ::= (<var> <Exp>)
;;;  <TokBinding> ::= (<Token>  <Code>*)
;;; <TODO> DECIDE ON LOCAL BINDINGS:
;;;  <TokBinding> ::= (<Token> (bindings <Decl>*) <Code>*)

;;;  <Code> ::= <Statement>*
;;;  <Statement>  ::= <BasicStuff?>
;;;                | (emit <Token> <Simple>*)
;;;                | (relay <Token>)
;;;                | (dist <Token>)
;;;                | (return <Token> <Simple>)
;;;                | <Macro> 
;;;  <Macro> ::= (flood <Token>)
;;;            | (elect-leader <Token> [<Token>])  ;; <TODO> optional second argument.. decider
;;;  <Simple> ::= (quote <Lit>) | <Var>

;;;  <Token> ::= <Symbol> | ...???
;;;  <Exp>  ::= ???


