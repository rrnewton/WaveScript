;;; wavescope.el --- Caml mode for (X)Emacs.   -*- coding: latin-1 -*-
   
;;        Copyright © 1997-2005 Albert Cohen, all rights reserved.
;;        Licensed under the GNU General Public License.

;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.

;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;    GNU General Public License for more details.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'easymenu)

(defconst wavescope-mode-version "Wavescope Version 1.44.2"
  "        Copyright © 1997-2005 Albert Cohen, all rights reserved.
         Copying is covered by the GNU General Public License.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Emacs versions support

(defconst wavescope-with-xemacs (featurep 'xemacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Compatibility functions

(defalias 'wavescope-match-string
  (if (fboundp 'match-string-no-properties)
      'match-string-no-properties
    'match-string))

(if (not (fboundp 'read-shell-command))
    (defun read-shell-command  (prompt &optional initial-input history)
      "Read a string from the minibuffer, using `shell-command-history'."
      (read-from-minibuffer prompt initial-input nil nil
			    (or history 'shell-command-history))))

(if (not (fboundp 'string-as-multibyte))
    (defun string-as-multibyte (str)
      "Return same string for not multibyte emacs'en"
      str))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Import types and help features

(defvar wavescope-with-caml-mode-p
  (condition-case nil
      (and (require 'caml-types) (require 'caml-help))
    (error nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       User customizable variables

;; Use the standard `customize' interface or `wavescope-mode-hook' to
;; Configure these variables

(require 'custom)

(defgroup wavescope nil
  "Support for the Objective Caml language."
  :group 'languages)

;; Comments

(defcustom wavescope-indent-leading-comments t
  "*If true, indent leading comment lines (starting with `(*') like others."
  :group 'wavescope :type 'boolean)

(defcustom wavescope-indent-comments t
  "*If true, automatically align multi-line comments."
  :group 'wavescope :type 'boolean)

(defcustom wavescope-comment-end-extra-indent 0
  "*How many spaces to indent a leading comment end `*)'.
If you expect comments to be indented like
	(*
          ...
	 *)
even without leading `*', use `wavescope-comment-end-extra-indent' = 1."
  :group 'wavescope
  :type '(radio :extra-offset 8
		:format "%{Comment End Extra Indent%}:
   Comment alignment:\n%v"
		(const :tag "align with `/' in comment opening" 0)
		(const :tag "align with `*' in comment opening" 1)
		(integer :tag "custom alignment" 0)))

(defcustom wavescope-support-leading-star-comments t
  "*Enable automatic intentation of comments of the form
        (*
         * ...
         *)
Documentation comments (** *) are not concerned by this variable
unless `wavescope-leading-star-in-doc' is also set.

If you do not set this variable and still expect comments to be
indented like
	(*
          ...
	 *)
(without leading `*'), set `wavescope-comment-end-extra-indent' to 1."
  :group 'wavescope :type 'boolean)

(defcustom wavescope-leading-star-in-doc nil
  "*Enable automatic intentation of documentation comments of the form
        (**
         * ...
         *)"
  :group 'wavescope :type 'boolean)

;; Indentation defaults

(defcustom wavescope-default-indent 2
  "*Default indentation.

Global indentation variable (large values may lead to indentation overflows).
When no governing keyword is found, this value is used to indent the line
if it has to."
  :group 'wavescope :type 'integer)

(defcustom wavescope-lazy-paren nil
  "*If true, indent parentheses like a standard keyword."
  :group 'wavescope :type 'boolean)

(defcustom wavescope-support-camllight nil
  "*If true, handle Caml Light character syntax (incompatible with labels)."
  :group 'wavescope :type 'boolean
  :set '(lambda (var val)
	  (setq wavescope-support-camllight val)
	  (if (boundp 'wavescope-mode-syntax-table)
	      (modify-syntax-entry ?` (if val "\"" ".")
				   wavescope-mode-syntax-table))))

(defcustom wavescope-let-always-indent t
  "*If true, enforce indentation is at least `wavescope-let-indent' after a `let'.

As an example, set it to false when you have `wavescope-with-indent' set to 0,
and you want `let x = match ... with' and `match ... with' indent the
same way."
  :group 'wavescope :type 'boolean)

(defcustom wavescope-|-extra-unindent wavescope-default-indent
  "*Extra backward indent for Caml lines starting with the `|' operator.

It is NOT the variable controlling the indentation of the `|' itself:
this value is automatically added to `function', `with', `parse' and
some cases of `type' keywords to leave enough space for `|' backward
indentation.

For exemple, setting this variable to 0 leads to the following indentation:
  match ... with
    X -> ...
    | Y -> ...
    | Z -> ...

To modify the indentation of lines lead by `|' you need to modify the
indentation variables for `with', `function' and `parse', and possibly
for `type' as well. For example, setting them to 0 (and leaving
`wavescope-|-extra-unindent' to its default value) yields:
  match ... with
    X -> ...
  | Y -> ...
  | Z -> ..."
  :group 'wavescope :type 'integer)

(defcustom wavescope-class-indent wavescope-default-indent
  "*How many spaces to indent from a `class' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-sig-struct-align t
  "*Align `sig' and `struct' keywords with `module'."
  :group 'wavescope :type 'boolean)

(defcustom wavescope-sig-struct-indent wavescope-default-indent
  "*How many spaces to indent from a `sig' or `struct' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-method-indent wavescope-default-indent
  "*How many spaces to indent from a `method' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-begin-indent wavescope-default-indent
  "*How many spaces to indent from a `begin' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-for-while-indent wavescope-default-indent
  "*How many spaces to indent from a `for' or `while' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-do-indent wavescope-default-indent
  "*How many spaces to indent from a `do' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-fun-indent wavescope-default-indent
  "*How many spaces to indent from a `fun' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-function-indent wavescope-default-indent
  "*How many spaces to indent from a `function' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-if-then-else-indent wavescope-default-indent
  "*How many spaces to indent from an `if', `then' or `else' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-let-indent wavescope-default-indent
  "*How many spaces to indent from a `let' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-in-indent wavescope-default-indent
  "*How many spaces to indent from a `in' keyword.
A lot of people like formatting `let' ... `in' expressions whithout
indentation:
        let x = 0 in
        blah x
Set this variable to 0 to get this behaviour.
However, nested declarations are always correctly handled:
        let x = 0 in                             let x = 0
        let y = 0 in              or             in let y = 0
        let z = 0 ...                            in let z = 0 ..."
  :group 'wavescope :type 'integer)

(defcustom wavescope-match-indent wavescope-default-indent
  "*How many spaces to indent from a `match' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-try-indent wavescope-default-indent
  "*How many spaces to indent from a `try' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-with-indent wavescope-default-indent
  "*How many spaces to indent from a `with' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-rule-indent wavescope-default-indent
  "*How many spaces to indent from a `rule' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-parse-indent wavescope-default-indent
  "*How many spaces to indent from a `parse' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-parser-indent wavescope-default-indent
  "*How many spaces to indent from a `parser' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-type-indent wavescope-default-indent
  "*How many spaces to indent from a `type' keyword."
  :group 'wavescope :type 'integer)

(defcustom wavescope-val-indent wavescope-default-indent
  "*How many spaces to indent from a `val' keyword."
  :group 'wavescope :type 'integer)

;; Automatic indentation
;; Using abbrev-mode and electric keys

(defcustom wavescope-use-abbrev-mode t
  "*Non-nil means electrically indent lines starting with leading keywords.
Leading keywords are such as `end', `done', `else' etc.
It makes use of `abbrev-mode'.

Many people find eletric keywords irritating, so you can disable them by
setting this variable to nil."
  :group 'wavescope :type 'boolean
  :set '(lambda (var val)
	  (setq wavescope-use-abbrev-mode val)
	  (abbrev-mode val)))

(defcustom wavescope-electric-indent t
  "*Non-nil means electrically indent lines starting with `|', `)', `]' or `}'.

Many people find eletric keys irritating, so you can disable them in
setting this variable to nil."
  :group 'wavescope :type 'boolean)

(defcustom wavescope-electric-close-vector t
  "*Non-nil means electrically insert `|' before a vector-closing `]' or
`>' before an object-closing `}'.

Many people find eletric keys irritating, so you can disable them in
setting this variable to nil. You should probably have this on,
though, if you also have `wavescope-electric-indent' on."
  :group 'wavescope :type 'boolean)

;; Wavescope-Interactive
;; Configure via `wavescope-mode-hook'

(defcustom wavescope-skip-after-eval-phrase t
  "*Non-nil means skip to the end of the phrase after evaluation in the
Caml toplevel."
  :group 'wavescope :type 'boolean)

(defcustom wavescope-interactive-read-only-input nil
  "*Non-nil means input send to the Caml toplevel is read-only."
  :group 'wavescope :type 'boolean)

(defcustom wavescope-interactive-echo-phrase t
  "*Non-nil means echo phrases in the toplevel buffer when sending
them to the Caml toplevel."
  :group 'wavescope :type 'boolean)

(defcustom wavescope-interactive-input-font-lock t
  "*Non nil means Font-Lock for toplevel input phrases."
  :group 'wavescope :type 'boolean)

(defcustom wavescope-interactive-output-font-lock t
  "*Non nil means Font-Lock for toplevel output messages."
  :group 'wavescope :type 'boolean)

(defcustom wavescope-interactive-error-font-lock t
  "*Non nil means Font-Lock for toplevel error messages."
  :group 'wavescope :type 'boolean)

(defcustom wavescope-display-buffer-on-eval t
  "*Non nil means pop up the Caml toplevel when evaluating code."
  :group 'wavescope :type 'boolean)

(defcustom wavescope-manual-url "http://pauillac.inria.fr/ocaml/htmlman/index.html"
  "*URL to the Caml reference manual."
  :group 'wavescope :type 'string)

(defcustom wavescope-browser 'wavescope-netscape-manual
  "*Name of function that displays the Caml reference manual.
Valid names are `wavescope-netscape-manual', `wavescope-mmm-manual'
and `wavescope-xemacs-w3-manual' (XEmacs only)."
  :group 'wavescope)

(defcustom wavescope-library-path "/usr/local/lib/ocaml/"
  "*Path to the Caml library."
  :group 'wavescope :type 'string)

(defcustom wavescope-definitions-max-items 30
  "*Maximum number of items a definitions menu can contain."
  :group 'wavescope :type 'integer)

(defvar wavescope-options-list
  '(("Lazy parentheses indentation" . 'wavescope-lazy-paren)
    ("Force indentation after `let'" . 'wavescope-let-always-indent)
    "---"
    ("Automatic indentation of leading keywords" . 'wavescope-use-abbrev-mode)
    ("Electric indentation of ), ] and }" . 'wavescope-electric-indent)
    ("Electric matching of [| and {<" . 'wavescope-electric-close-vector)
    "---"
    ("Indent body of comments" . 'wavescope-indent-comments)
    ("Indent first line of comments" . 'wavescope-indent-leading-comments)
    ("Leading-`*' comment style" . 'wavescope-support-leading-star-comments))
  "*List of menu-configurable Wavescope options.")

(defvar wavescope-interactive-options-list
  '(("Skip phrase after evaluation" . 'wavescope-skip-after-eval-phrase)
    ("Echo phrase in interactive buffer" . 'wavescope-interactive-echo-phrase)
    "---"
    ("Font-lock interactive input" . 'wavescope-interactive-input-font-lock)
    ("Font-lock interactive output" . 'wavescope-interactive-output-font-lock)
    ("Font-lock interactive error" . 'wavescope-interactive-error-font-lock)
    "---"
    ("Read only input" . 'wavescope-interactive-read-only-input))
  "*List of menu-configurable Wavescope options.")

(defvar wavescope-interactive-program "ocaml"
  "*Default program name for invoking a Caml toplevel from Emacs.")
;; Could be interesting to have this variable buffer-local
;;   (e.g., ocaml vs. metaocaml buffers)
;; (make-variable-buffer-local 'wavescope-interactive-program)

(defgroup wavescope-faces nil
  "Special faces for the Wavescope mode."
  :group 'wavescope)

(defface wavescope-font-lock-governing-face
  '((((background light))
     (:foreground "darkorange3" :bold t))
    (t (:foreground "orange" :bold t)))
  "Face description for governing/leading keywords."
  :group 'wavescope-faces)
(defvar wavescope-font-lock-governing-face
  'wavescope-font-lock-governing-face)

(defface wavescope-font-lock-multistage-face
  '((((background light))
     (:foreground "darkblue" :background "lightgray" :bold t))
    (t (:foreground "steelblue" :background "darkgray" :bold t)))
  "Face description for MetaOCaml staging operators."
  :group 'wavescope-faces)
(defvar wavescope-font-lock-multistage-face
  'wavescope-font-lock-multistage-face)

(defface wavescope-font-lock-operator-face
  '((((background light))
     (:foreground "brown4"))
    (t (:foreground "salmon")))
  "Face description for all operators."
  :group 'wavescope-faces)
(defvar wavescope-font-lock-operator-face
  'wavescope-font-lock-operator-face)

(defface wavescope-font-lock-error-face
  '((t (:foreground "yellow" :background "red")))
  "Face description for all errors reported to the source."
  :group 'wavescope-faces)
(defvar wavescope-font-lock-error-face
  'wavescope-font-lock-error-face)

(defface wavescope-font-lock-interactive-output-face
  '((((background light))
     (:foreground "blue4"))
    (t (:foreground "cyan")))
  "Face description for all toplevel outputs."
  :group 'wavescope-faces)
(defvar wavescope-font-lock-interactive-output-face
  'wavescope-font-lock-interactive-output-face)

(defface wavescope-font-lock-interactive-error-face
  '((((background light))
     (:foreground "red3"))
    (t (:foreground "red2")))
  "Face description for all toplevel errors."
  :group 'wavescope-faces)
(defvar wavescope-font-lock-interactive-error-face
  'wavescope-font-lock-interactive-error-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Support definitions

(defvar wavescope-cache-stop (point-min))
(make-variable-buffer-local 'wavescope-cache-stop)
(defvar wavescope-cache nil)
(make-variable-buffer-local 'wavescope-cache)
(defvar wavescope-cache-local nil)
(make-variable-buffer-local 'wavescope-cache-local)
(defvar wavescope-cache-last-local nil)
(make-variable-buffer-local 'wavescope-cache-last-local)
(defvar wavescope-last-loc (cons nil nil))

(defun wavescope-leading-star-p ()
  (and wavescope-support-leading-star-comments
       (save-excursion ; this function does not make sense outside of a comment
	 (wavescope-beginning-of-literal-or-comment)
	 (and (or wavescope-leading-star-in-doc
		  (not (looking-at "(\\*[Tt][Ee][Xx]\\|(\\*\\*")))
	      (progn
		(forward-line 1)
		(back-to-indentation)
		(looking-at "\\*[^/]"))))))

(defun wavescope-auto-fill-insert-leading-star (&optional leading-star)
  ;; RRN POSSIBLY BAD "/" inserted:
  (let ((point-leading-comment (looking-at "/\\*")) (return-leading nil))
    (save-excursion
      (back-to-indentation)
      (if wavescope-electric-indent
	  (prog2
	      (if (and (wavescope-in-comment-p)
		       (or leading-star
			   (wavescope-leading-star-p)))
		  (prog2
		      (if (not (looking-at "(?\\*"))
			  (insert-before-markers "* "))
		      (setq return-leading t)))
	      (if (not point-leading-comment)
		  ;; Use optional argument to break recursion
		  (wavescope-indent-command t)))))
    return-leading))

(defun wavescope-auto-fill-function ()
  (if (wavescope-in-literal-p) ()
    (let ((leading-star
	   (if (not (char-equal ?\n last-command-char))
	       (wavescope-auto-fill-insert-leading-star)
	     nil)))
      (do-auto-fill)
      (if (not (char-equal ?\n last-command-char))
	  (wavescope-auto-fill-insert-leading-star leading-star)))))

(defun wavescope-forward-char (&optional step)
  (if step (goto-char (+ (point) step))
    (goto-char (1+ (point)))))

(defun wavescope-backward-char (&optional step)
  (if step (goto-char (- (point) step))
    (goto-char (1- (point)))))

(defun wavescope-in-indentation-p ()
  "Return non-nil if all chars between beginning of line and point are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun wavescope-before-change-function (begin end)
  (setq wavescope-cache-stop (min wavescope-cache-stop (1- begin))))

(defun wavescope-in-literal-p ()
  "Return non-nil if point is inside a Caml literal."
  (car (wavescope-in-literal-or-comment)))
(defun wavescope-in-comment-p ()
  "Return non-nil if point is inside a Caml comment."
  (cdr (wavescope-in-literal-or-comment)))
(defun wavescope-in-literal-or-comment-p ()
  "Return non-nil if point is inside a Caml literal or comment."
  (wavescope-in-literal-or-comment)
  (or (car wavescope-last-loc) (cdr wavescope-last-loc)))


;; ======================================================================
;; RRN: Targeting here mainly.
(defun wavescope-in-literal-or-comment ()
  "Return the pair `((wavescope-in-literal-p) . (wavescope-in-comment-p))'."
  (if (and (<= (point) wavescope-cache-stop) wavescope-cache)
      (progn
	(if (or (not wavescope-cache-local) (not wavescope-cache-last-local)
		(and (>= (point) (caar wavescope-cache-last-local))))
	    (setq wavescope-cache-local wavescope-cache))
	(while (and wavescope-cache-local (< (point) (caar wavescope-cache-local)))
	  (setq wavescope-cache-last-local wavescope-cache-local
		wavescope-cache-local (cdr wavescope-cache-local)))
	(setq wavescope-last-loc
	      (if wavescope-cache-local
		  (cons (eq (cadar wavescope-cache-local) 'b)
			(> (cddar wavescope-cache-local) 0))
		(cons nil nil))))
    (let ((flag t) (op (point)) (mp (min (point) (1- (point-max))))
	  (balance 0) (end-of-comment nil))
      (while (and wavescope-cache (<= wavescope-cache-stop (caar wavescope-cache)))
	(setq wavescope-cache (cdr wavescope-cache)))
      (if wavescope-cache
	  (if (eq (cadar wavescope-cache) 'b)
	      (progn
		(setq wavescope-cache-stop (1- (caar wavescope-cache)))
		(goto-char wavescope-cache-stop)
		(setq balance (cddar wavescope-cache))
		(setq wavescope-cache (cdr wavescope-cache)))
	    (setq balance (cddar wavescope-cache))
	    (setq wavescope-cache-stop (caar wavescope-cache))
	    (goto-char wavescope-cache-stop)
	    (skip-chars-forward "("))
	(goto-char wavescope-cache-stop))
      (skip-chars-backward "\\\\*")
      (while flag
	(if end-of-comment (setq balance 0 end-of-comment nil))
	;; RRN POSSIBLY BAD "/" inserted:
	(skip-chars-forward "^\\\\'`\"(\\*")

	(cond
	 ((looking-at "\\\\")
	  (wavescope-forward-char 2))
	 ;; rrn... Literal...
	 ((looking-at "'\\([^\n']\\|\\\\..?.?\\)'")
	  (wavescope-forward-char)
	  (setq wavescope-cache (cons (cons (point) (cons 'b balance))
				   wavescope-cache))
	  (skip-chars-forward "^'") (wavescope-forward-char)
	  (setq wavescope-cache (cons (cons (point) (cons 'e balance))
				   wavescope-cache)))
	 ;; rrn... Literal...
	 ((and wavescope-support-camllight
	       (looking-at "`\\([^\n']\\|\\\\..?.?\\)`"))
	  (wavescope-forward-char)
	  (setq wavescope-cache (cons (cons (point) (cons 'b balance))
				   wavescope-cache))
	  (skip-chars-forward "^`") (wavescope-forward-char)
	  (setq wavescope-cache (cons (cons (point) (cons 'e balance))
				   wavescope-cache)))
	 ;; rrn... Literal...
	 ((looking-at "\"")
	  (wavescope-forward-char)
	  (setq wavescope-cache (cons (cons (point) (cons 'b balance))
				   wavescope-cache))
	  (skip-chars-forward "^\\\\\"")
	  (while (looking-at "\\\\")
	    (wavescope-forward-char 2) (skip-chars-forward "^\\\\\""))
	  (wavescope-forward-char)
	  (setq wavescope-cache (cons (cons (point) (cons 'e balance))
				   wavescope-cache)))
	 	 
	 ;; RRN POSSIBLY BAD "/" inserted:
	 ((looking-at "(\\*")
	  (setq balance (1+ balance))
	  (setq wavescope-cache (cons (cons (point) (cons nil balance))
				   wavescope-cache))
	  (wavescope-forward-char 2))
	 ;; RRN POSSIBLY BAD "/" inserted:
	 ((looking-at "\\*)")
	  (wavescope-forward-char 2)
	  (if (> balance 1)
	      (prog2
		  (setq balance (1- balance))
		  (setq wavescope-cache (cons (cons (point) (cons nil balance))
					   wavescope-cache)))
	    (setq end-of-comment t)
	    (setq wavescope-cache (cons (cons (point) (cons nil 0))
				     wavescope-cache))))
	 (t (wavescope-forward-char)))
	(setq flag (<= (point) mp)))
      (setq wavescope-cache-local wavescope-cache
	    wavescope-cache-stop (point))
      (goto-char op)
      (if wavescope-cache (wavescope-in-literal-or-comment) 
	(setq wavescope-last-loc (cons nil nil))
	wavescope-last-loc))))

(defun wavescope-beginning-of-literal-or-comment ()
  "Skips to the beginning of the current literal or comment (or buffer)."
  (interactive)
  (if (wavescope-in-literal-or-comment-p)
      (wavescope-beginning-of-literal-or-comment-fast)))

(defun wavescope-beginning-of-literal-or-comment-fast ()
  (while (and wavescope-cache-local
	      (or (eq 'b (cadar wavescope-cache-local))
		  (> (cddar wavescope-cache-local) 0)))
    (setq wavescope-cache-last-local wavescope-cache-local
	  wavescope-cache-local (cdr wavescope-cache-local)))
  (if wavescope-cache-last-local
      (goto-char (caar wavescope-cache-last-local))
    (goto-char (point-min)))
  (if (eq 'b (cadar wavescope-cache-last-local)) (wavescope-backward-char)))

(defun wavescope-false-=-p ()
  "Is the underlying `=' the first/second letter of an operator?"
  (or (memq (preceding-char) '(?: ?> ?< ?=))
      (char-equal ?= (char-after (1+ (point))))))

(defun wavescope-at-phrase-break-p ()
  "Is the underlying `;' a phrase break?"
  (and (char-equal ?\; (following-char))
       (or (and (not (eobp))
		(char-equal ?\; (char-after (1+ (point)))))
	   (char-equal ?\; (preceding-char)))))

(defun wavescope-backward-up-list ()
  "Safe up-list regarding comments, literals and errors."
  (let ((balance 1) (op (point)) (oc nil))
    (wavescope-in-literal-or-comment)
    (while (and (> (point) (point-min)) (> balance 0))
      (setq oc (if wavescope-cache-local (caar wavescope-cache-local) (point-min)))
      (condition-case nil (up-list -1) (error (goto-char (point-min))))
      (if (>= (point) oc) (setq balance (1- balance))
	(goto-char op)
	(skip-chars-backward "^[]{}()") (wavescope-backward-char)
	(if (not (wavescope-in-literal-or-comment-p))
	    (cond
	     ((looking-at "[[{(]")
	      (setq balance (1- balance)))
	     ((looking-at "[]})]")
	      (setq balance (1+ balance))))
	  (wavescope-beginning-of-literal-or-comment-fast)))
      (setq op (point)))))

(defun wavescope-assoc-indent (kwop &optional look-for-let-or-and)
  "Return relative indentation of the keyword given in argument."
  (let ((ind (symbol-value (cdr (assoc kwop wavescope-keyword-alist))))
	(looking-let-or-and (and look-for-let-or-and
				 (looking-at "\\<\\(let\\|and\\)\\>"))))
    (if (string-match "\\<\\(with\\|function\\|parser?\\)\\>" kwop)
	(+ (if (and wavescope-let-always-indent
		    looking-let-or-and (< ind wavescope-let-indent))
	       wavescope-let-indent ind)
	   wavescope-|-extra-unindent)
      ind)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Font-Lock

(defun wavescope-fontify-buffer ()
  (font-lock-default-fontify-buffer)
  (wavescope-fontify (point-min) (point-max)))

(defun wavescope-fontify-region (begin end &optional verbose)
  (font-lock-default-fontify-region begin end verbose)
  (wavescope-fontify begin end))

(defun ryan-fontify-line-comment-hack ()
  "Hack for // comments."
  (interactive)

  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]*//")
	(progn 
	  (put-text-property (point) (+ 10 (point)) 'face
			     'font-lock-comment-face)
	  (print "WOOT"))
      (print "FLUB")))
  )
;	(re-search-forward "\\(\\.<\\|(\\|\\[[<|]?\\|{<?\\)[ \t]*")
    
  
(defun wavescope-fontify (begin end)
  (if (eq major-mode 'wavescope-mode)
      (save-excursion
	(let ((modified (buffer-modified-p))) ; Emacs hack (see below)
	  (goto-char begin)
	  (beginning-of-line)

	  (ryan-fontify-line-comment-hack)

	  (setq begin (point))
	  (goto-char (1- end))
	  (end-of-line)
	  ;; Dirty hack to trick `font-lock-default-unfontify-region'
	  (if (not wavescope-with-xemacs) (forward-line 2))
	  (setq end (point))
	  (while (> end begin)
	    (goto-char (1- end))
	    (wavescope-in-literal-or-comment)
	    (cond
	     ((cdr wavescope-last-loc)
	      (wavescope-beginning-of-literal-or-comment)
	      (put-text-property (max begin (point)) end 'face
				 (if (looking-at
				      "(\\*[Tt][Ee][Xx]\\|(\\*\\*[^*]")
				     wavescope-doc-face
				   'font-lock-comment-face))
	      (setq end (1- (point))))
	     ((car wavescope-last-loc)
	      (wavescope-beginning-of-literal-or-comment)
	      (put-text-property (max begin (point)) end 'face
				 'font-lock-string-face)
	      (setq end (point)))
	     (t (while (and wavescope-cache-local
			    (or (> (caar wavescope-cache-local) end)
				(eq 'b (cadar wavescope-cache-local))))
		  (setq wavescope-cache-local (cdr wavescope-cache-local)))
		(setq end (if wavescope-cache-local
			      (caar wavescope-cache-local) begin)))))
	  (if (not (or wavescope-with-xemacs modified)) ; properties taken
	      (set-buffer-modified-p nil))))))       ; too seriously...

;; XEmacs and Emacs have different documentation faces...
(defvar wavescope-doc-face (if (facep 'font-lock-doc-face)
			    'font-lock-doc-face
			  'font-lock-doc-string-face))

;; Patch by Stefan Monnier: redesigned font-lock installation
;; and use char classes

(defconst wavescope-use-char-classes (string-match "[[:alpha:]]" "x"))
(defconst wavescope-lower (if wavescope-use-char-classes "[:lower:]" "a-z¿-ÿ"))
(defconst wavescope-alpha (if wavescope-use-char-classes "[:alpha:]" "a-zA-Z¿-ÿ"))

(defconst wavescope-font-lock-syntactic-keywords
  ;; Char constants start with ' but ' can also appear in identifiers.
  ;; Beware not to match things like '*)hel' or '"hel' since the first '
  ;; might be inside a string or comment.
  '(("\\<\\('\\)\\([^'\\\n]\\|\\\\.[^\\'\n \")]*\\)\\('\\)"
     (1 '(7)) (3 '(7)))))

(defun wavescope-font-lock-syntactic-face-function (state)
  (if (nth 3 state) font-lock-string-face
    (let ((start (nth 8 state)))
      (if (and (> (point-max) (+ start 2))
	       (eq (char-after (+ start 2)) ?*)
	       (not (eq (char-after (+ start 3)) ?*)))
	  ;; This is a documentation comment
	  font-lock-doc-face
	font-lock-comment-face))))

(if (facep 'font-lock-constant-face) ()
  (defvar font-lock-constant-face font-lock-reference-face)
  (copy-face font-lock-reference-face 'font-lock-constant-face))
(if (facep 'font-lock-preprocessor-face) ()
  (defvar font-lock-preprocessor-face font-lock-keyword-face)
  (copy-face font-lock-keyword-face 'font-lock-preprocessor-face))

(defvar wavescope-font-lock-keywords
  (list
   (list "\\<\\(external\\|open\\|include\\|rule\\|s\\(ig\\|truct\\)\\|module\\|functor\\|with[ \t\n]+\\(type\\|module\\)\\|val\\|type\\|method\\|virtual\\|constraint\\|class\\|in\\|inherit\\|initializer\\|let\\|rec\\|and\\|begin\\|object\\|end\\)\\>"
	 0 'wavescope-font-lock-governing-face nil nil)
   (list "\\.<\\|>\\.\\|\\.~\\|\\.!"
	 0 'wavescope-font-lock-multistage-face nil nil)
   (list "\\<\\(false\\|true\\)\\>"
	 0 'font-lock-constant-face nil nil)
   (list "\\<\\(as\\|do\\(ne\\|wnto\\)?\\|else\\|for\\|if\\|m\\(atch\\|utable\\)\\|new\\|p\\(arser\\|rivate\\)\\|t\\(hen\\|o\\|ry\\)\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\|lazy\\|exception\\|raise\\|failwith\\|exit\\|assert\\|fun\\(ction\\)?\\)\\>"
	 0 'font-lock-keyword-face nil nil)
   (list "[][;,()|{}]\\|[@^!:*=<>&/%+~?---]\\.?\\|\\.\\.\\.*\\|\\<\\(asr\\|asl\\|lsr\\|lsl\\|l?or\\|l?and\\|xor\\|not\\|mod\\|of\\|ref\\)\\>"
	 0 'wavescope-font-lock-operator-face nil nil)
   (list (concat "\\<\\(\\(method\\([ \t\n]+\\(private\\|virtual\\)\\)?\\)\\([ \t\n]+virtual\\)?\\|val\\([ \t\n]+mutable\\)?\\|external\\|and\\|class\\|let\\([ \t\n]+rec\\)?\\)\\>[ \t\n]*\\(['_" wavescope-lower "]\\(\\w\\|[._]\\)*\\)\\>[ \t\n]*\\(\\w\\|[()_?~.]\\|:?\\(\\w\\|[ \t_'*.--->]\\)*=[ \t\n]*fun\\(ction\\)?\\>\\)")
	 8 'font-lock-function-name-face 'keep nil)
   (list "\\<method\\([ \t\n]+\\(private\\|virtual\\)\\)?\\>[ \t\n]*\\(\\(\\w\\|[_,?~.]\\)*\\)"
	 3 'font-lock-function-name-face 'keep nil)
   (list "\\<\\(fun\\(ction\\)?\\)\\>[ \t\n]*\\(\\(\\w\\|[_ \t()*,]\\)+\\)"
	 3 'font-lock-variable-name-face 'keep nil)
   (list "\\<\\(val\\([ \t\n]+mutable\\)?\\|external\\|and\\|class\\|let\\([ \t\n]+rec\\)?\\)\\>[ \t\n]*\\(\\(\\w\\|[_,?~.]\\)*\\)"
	 4 'font-lock-variable-name-face 'keep nil)
   (list "\\<\\(val\\([ \t\n]+mutable\\)?\\|external\\|and\\|class\\|let\\([ \t\n]+rec\\)?\\)\\>[ \t\n]*\\(\\(\\w\\|[_,?~.]\\)*\\)\\(\\(\\w\\|[->_ \t,?~.]\\|(\\(\\w\\|[--->_ \t,?~.=]\\)*)\\)*\\)"
	 6 'font-lock-variable-name-face 'keep nil)
   (list "\\<\\(open\\|\\(class\\([ \t\n]+type\\)?\\)\\([ \t\n]+virtual\\)?\\|inherit\\|include\\|module\\([ \t\n]+\\(type\\|rec\\)\\)?\\|type\\)\\>[ \t\n]*\\(['~?]*\\([_--->.* \t]\\|\\w\\|(['~?]*\\([_--->.,* \t]\\|\\w\\)*)\\)*\\)"
	 7 'font-lock-type-face 'keep nil)
   (list "[^:>=]:[ \t\n]*\\(['~?]*\\([_--->.* \t]\\|\\w\\|(['~?]*\\([_--->.,* \t]\\|\\w\\)*)\\)*\\)"
	 1 'font-lock-type-face 'keep nil)
   (list "\\<\\([A-Z]\\w*\\>\\)[ \t]*\\."
	 1 'font-lock-type-face 'keep nil)
   (list (concat "\\<\\([?~]?[_" wavescope-alpha "]\\w*\\)[ \t\n]*:[^:>=]")
	 1 'font-lock-variable-name-face 'keep nil)
   (list (concat "\\<exception\\>[ \t\n]*\\(\\<[_" wavescope-alpha "]\\w*\\>\\)")
	 1 'font-lock-variable-name-face 'keep nil)
   (list "^#\\w+\\>"
	 0 'font-lock-preprocessor-face t nil))
  "Font-Lock patterns for Wavescope mode.")

(when (featurep 'sym-lock)
  (make-face 'wavescope-font-lock-lambda-face
	     "Face description for fun keywords (lambda operator).")
  (set-face-parent 'wavescope-font-lock-lambda-face
		   font-lock-function-name-face)
  (set-face-font 'wavescope-font-lock-lambda-face
		 sym-lock-font-name)
  
  ;; To change this table, xfd -fn '-adobe-symbol-*--12-*' may be
  ;; used to determine the symbol character codes.
  (defvar wavescope-sym-lock-keywords
    '(("<-" 0 1 172 nil)
      ("->" 0 1 174 nil)
      ("<=" 0 1 163 nil)
      (">=" 0 1 179 nil)
      ("<>" 0 1 185 nil)
      ("==" 0 1 186 nil)
      ("||" 0 1 218 nil)
      ("&&" 0 1 217 nil)
      ("[^*]\\(\\*\\)\\." 1 8 180 nil)
      ("\\(/\\)\\." 1 3 184 nil)
      (";;" 0 1 191 nil)
      ("\\<sqrt\\>" 0 3 214 nil)
      ("\\<fun\\>" 0 3 108 wavescope-font-lock-lambda-face)
      ("\\<or\\>" 0 3 218 nil)
      ("\\<not\\>" 0 3 216 nil))
    "If non nil: Overrides default Sym-Lock patterns for Wavescope."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Keymap

(defvar wavescope-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "|" 'wavescope-electric)
    (define-key map ")" 'wavescope-electric-rp)
    (define-key map "}" 'wavescope-electric-rc)
    (define-key map "]" 'wavescope-electric-rb)
    (define-key map "\M-q" 'wavescope-indent-phrase)
    (define-key map "\C-c\C-q" 'wavescope-indent-phrase)
    (define-key map "\M-\C-\\" 'indent-region)
    (define-key map "\C-c\C-a" 'wavescope-find-alternate-file)
    (define-key map "\C-c\C-c" 'compile)
    (define-key map "\C-xnd" 'wavescope-narrow-to-phrase)
    (define-key map "\M-\C-x" 'wavescope-eval-phrase)
    (define-key map "\C-x\C-e" 'wavescope-eval-phrase)
    (define-key map "\C-c\C-e" 'wavescope-eval-phrase)
    (define-key map "\C-c\C-r" 'wavescope-eval-region)
    (define-key map "\C-c\C-b" 'wavescope-eval-buffer)
    (define-key map "\C-c\C-s" 'wavescope-run-caml)
    (define-key map "\C-c\C-i" 'wavescope-interrupt-caml)
    (define-key map "\C-c\C-k" 'wavescope-kill-caml)
    (define-key map "\C-c\C-n" 'wavescope-next-phrase)
    (define-key map "\C-c\C-p" 'wavescope-previous-phrase)
    (define-key map [(control c) (home)] 'wavescope-move-inside-block-opening)
    (define-key map [(control c) (control down)] 'wavescope-next-phrase)
    (define-key map [(control c) (control up)] 'wavescope-previous-phrase)
    (define-key map [(meta control down)]  'wavescope-next-phrase)
    (define-key map [(meta control up)] 'wavescope-previous-phrase)
    (define-key map [(meta control h)] 'wavescope-mark-phrase)
    (define-key map "\C-c`" 'wavescope-interactive-next-error-source)
    (define-key map "\C-c?" 'wavescope-interactive-next-error-source)
    (define-key map "\C-c.c" 'wavescope-insert-class-form)
    (define-key map "\C-c.b" 'wavescope-insert-begin-form)
    (define-key map "\C-c.f" 'wavescope-insert-for-form)
    (define-key map "\C-c.w" 'wavescope-insert-while-form)
    (define-key map "\C-c.i" 'wavescope-insert-if-form)
    (define-key map "\C-c.l" 'wavescope-insert-let-form)
    (define-key map "\C-c.m" 'wavescope-insert-match-form)
    (define-key map "\C-c.t" 'wavescope-insert-try-form)
    (when wavescope-with-caml-mode-p
      ;; Trigger caml-types
      (define-key map [?\C-c ?\C-t] 'caml-types-show-type)
      ;; To prevent misbehavior in case of error during exploration.
      (define-key map [(control mouse-2)] 'caml-types-mouse-ignore)
      (define-key map [(control down-mouse-2)] 'caml-types-explore)
      ;; Trigger caml-help
      (define-key map [?\C-c ?i] 'ocaml-add-path)
      (define-key map [?\C-c ?\[] 'ocaml-open-module)
      (define-key map [?\C-c ?\]] 'ocaml-close-module)
      (define-key map [?\C-c ?h] 'caml-help)
      (define-key map [?\C-c ?\t] 'caml-complete))
    map)
  "Keymap used in Wavescope mode.")
  
(defvar wavescope-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?? "w" st)
    (modify-syntax-entry ?~ "w" st)
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?' "w" st)	; ' is part of words (for primes).
    (modify-syntax-entry
     ;; ` is punctuation or character delimiter (Caml Light compatibility).
     ?` (if wavescope-support-camllight "\"" ".") st)
    (modify-syntax-entry ?\" "\"" st)	; " is a string delimiter
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?*  ". 23" st)
    (condition-case nil
	(progn
	  (modify-syntax-entry ?\( "()1n" st)
	  (modify-syntax-entry ?\) ")(4n" st))
      (error		   ;XEmacs signals an error instead of ignoring `n'.
       (modify-syntax-entry ?\( "()1" st)
       (modify-syntax-entry ?\) ")(4" st)))
    st)
  "Syntax table in use in Wavescope mode buffers.")

(defconst wavescope-font-lock-syntax
  '((?_ . "w") (?` . ".") (?\" . ".") (?\( . ".") (?\) . ".") (?* . ".")
    (?~ . ".") (?? . "."))
  "Syntax changes for Font-Lock.")

(defvar wavescope-mode-abbrev-table ()
  "Abbrev table used for Wavescope mode buffers.")
(defun wavescope-define-abbrev (keyword)
  (define-abbrev wavescope-mode-abbrev-table keyword keyword 'wavescope-abbrev-hook))
(if wavescope-mode-abbrev-table ()
  (setq wavescope-mode-abbrev-table (make-abbrev-table))
  (mapcar 'wavescope-define-abbrev
	  '("module" "class" "functor" "object" "type" "val" "inherit"
	    "include" "virtual" "constraint" "exception" "external" "open"
	    "method" "and" "initializer" "to" "downto" "do" "done" "else"
	    "begin" "end" "let" "in" "then" "with"))
  (setq abbrevs-changed nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              The major mode

;;;###autoload (add-to-list 'auto-mode-alist '("\\.ml[ily]?\\'" . wavescope-mode))

;;;###autoload
(defun wavescope-mode ()
  "Major mode for editing Caml code.

Dedicated to Emacs and XEmacs, version 20 and higher. Provides
automatic indentation and compilation interface. Performs font/color
highlighting using Font-Lock. It is designed for Objective Caml but
handles Objective Labl and Caml Light as well.

Report bugs, remarks and questions to Albert.Cohen@prism.uvsq.fr.

The Font-Lock minor-mode is used according to your customization
options. Within XEmacs (non-MULE versions only) you may also want to
use Sym-Lock:

\(if (and (boundp 'window-system) window-system)
    (when (string-match \"XEmacs\" emacs-version)
       	(if (not (and (boundp 'mule-x-win-initted) mule-x-win-initted))
            (require 'sym-lock))
       	(require 'font-lock)))

You have better byte-compile wavescope.el (and sym-lock.el if you use it)
because symbol highlighting is very time consuming.

For customization purposes, you should use `wavescope-mode-hook'
\(run for every file) or `wavescope-load-hook' (run once) and not patch
the mode itself. You should add to your configuration file something like:
  (add-hook 'wavescope-mode-hook
            (lambda ()
               ... ; your customization code
            ))
For example you can change the indentation of some keywords, the
`electric' flags, Font-Lock colors... Every customizable variable is
documented, use `C-h-v' or look at the mode's source code.

A special case is Sym-Lock customization: You may set
`wavescope-sym-lock-keywords' in your `.emacs' configuration file
to override default Sym-Lock patterns.

`custom-wavescope.el' is a sample customization file for standard changes.
You can append it to your `.emacs' or use it as a tutorial.

`M-x camldebug' FILE starts the Caml debugger camldebug on the executable
FILE, with input and output in an Emacs buffer named *camldebug-FILE*.

A Wavescope Interactive Mode to evaluate expressions in a toplevel is included.
Type `M-x wavescope-run-caml' or see special-keys below.

Some elementary rules have to be followed in order to get the best of
indentation facilities.
  - Because the `function' keyword has a special indentation (to handle
    case matches) use the `fun' keyword when no case match is performed.
  - In OCaml, `;;' is no longer necessary for correct indentation,
    except before top level phrases not introduced by `type', `val', `let'
    etc. (i.e., phrases used for their side-effects or to be executed
    in a top level.)
  - Long sequences of `and's may slow down indentation slightly, since
    some computations (few) require to go back to the beginning of the
    sequence. Some very long nested blocks may also lead to slow
    processing of `end's, `else's, `done's...
  - Multiline strings are handled properly, but the string concatenation `^'
    is preferred to break long strings (the C-j keystroke can help).

Known bugs:
  - When writting a line with mixed code and comments, avoid putting
    comments at the beginning or middle of the text. More precisely, 
    writing comments immediately after `=' or parentheses then writing
    some more code on the line leads to indentation errors. You may write
    `let x (* blah *) = blah' but should avoid `let x = (* blah *) blah'.

Special keys for Wavescope mode:\\{wavescope-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'wavescope-mode)
  (setq mode-name "Wavescope")
  (use-local-map wavescope-mode-map)
  (set-syntax-table wavescope-mode-syntax-table)
  (setq local-abbrev-table wavescope-mode-abbrev-table)

  (wavescope-build-menu)

  (make-local-variable 'paragraph-start)
  ;; RRN: Possibly broke:
  (setq paragraph-start (concat "^[ \t]*$\\|\\*/$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "/*")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+[ \t]*")
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line t)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'wavescope-indent-command)
  (make-local-hook 'before-change-functions)
  (add-hook 'before-change-functions 'wavescope-before-change-function nil t)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'wavescope-auto-fill-function)
	 
  ;; Hooks for wavescope-mode, use them for wavescope-mode configuration
  (run-hooks 'wavescope-mode-hook)
  (wavescope-install-font-lock)
  (if wavescope-use-abbrev-mode (abbrev-mode 1))
  (message (concat "Major mode for Caml programs, "
		   wavescope-mode-version ".")))

(defun wavescope-install-font-lock (&optional no-sym-lock)
  (if (and (not no-sym-lock)
	   (featurep 'sym-lock))
      (progn
	(setq sym-lock-color
	      (face-foreground 'wavescope-font-lock-operator-face))
	(if (not sym-lock-keywords)
	    (sym-lock wavescope-sym-lock-keywords))))
  (setq font-lock-defaults
	(list
	 'wavescope-font-lock-keywords t nil
	 wavescope-font-lock-syntax nil
	 '(font-lock-syntactic-keywords
	   . wavescope-font-lock-syntactic-keywords)
	 '(parse-sexp-lookup-properties
	   . t)
	 '(font-lock-syntactic-face-function
	   . wavescope-font-lock-syntactic-face-function)
	 '(font-lock-fontify-region-function
	   . wavescope-fontify-region)))
  (make-local-variable 'font-lock-fontify-region-function)
  (if (boundp 'font-lock-fontify-region-function)
      (setq font-lock-fontify-region-function 'wavescope-fontify-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Error processing

(require 'compile)

;; In some versions of Emacs, the regexps in
;; compilation-error-regexp-alist do not match the error messages when
;; the language is not English. Hence we add a regexp.

(defconst wavescope-error-regexp
  "^[^\0-@]+ \"\\([^\"\n]+\\)\", [^\0-@]+ \\([0-9]+\\)[-,:]"
  "Regular expression matching the error messages produced by (o)camlc.")

(if (boundp 'compilation-error-regexp-alist)
    (or (assoc wavescope-error-regexp
               compilation-error-regexp-alist)
        (setq compilation-error-regexp-alist
              (cons (list wavescope-error-regexp 1 2)
               compilation-error-regexp-alist))))

;; A regexp to extract the range info.

(defconst wavescope-error-chars-regexp
  ".*, .*, [^\0-@]+ \\([0-9]+\\)-\\([0-9]+\\):"
  "Regexp matching the char numbers in an error message produced by (o)camlc.")

;; Wrapper around next-error.

;; itz 04-21-96 instead of defining a new function, use defadvice
;; that way we get our effect even when we do \C-x` in compilation buffer  

(defadvice next-error (after wavescope-next-error activate)
 "Read the extra positional information provided by the Caml compiler.

Puts the point and the mark exactly around the erroneous program
fragment. The erroneous fragment is also temporarily highlighted if
possible."
 (if (eq major-mode 'wavescope-mode)
     (let ((beg nil) (end nil))
       (save-excursion
	 (set-buffer compilation-last-buffer)
	 (save-excursion
	   (goto-char (window-point (get-buffer-window (current-buffer) t)))
	   (if (looking-at wavescope-error-chars-regexp)
	       (setq beg (string-to-int (wavescope-match-string 1))
		     end (string-to-int (wavescope-match-string 2))))))
       (beginning-of-line)
       (if beg
	   (progn
	     (setq beg (+ (point) beg) end (+ (point) end))
	     (goto-char beg) (push-mark end t t))))))

(defvar wavescope-interactive-error-regexp
  (concat "\\(\\("
	  "Toplevel input:"
	  "\\|Entr.e interactive:"
	  "\\|Characters [0-9-]*:"
	  "\\|Toplevel input:"
	  "\\|The global value [^ ]* is referenced before being defined."
	  "\\|La valeur globale [^ ]* est utilis.e avant d'.tre d.finie."
	  "\\|Reference to undefined global"
	  "\\|The C primitive \"[^\"]*\" is not available."
	  "\\|La primitive C \"[^\"]*\" est inconnue."
	  "\\|Cannot find \\(the compiled interface \\)?file"
	  "\\|L'interface compil.e [^ ]* est introuvable."
	  "\\|Le fichier [^ ]* est introuvable."
	  "\\|Exception non rattrap.e:"
	  "\\|Uncaught exception:"
	  "\\)[^#]*\\)" )
  "Regular expression matching the error messages produced by Caml.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Indentation stuff

(defconst wavescope-keyword-regexp "\\<\\(object\\|initializer\\|and\\|c\\(onstraint\\|lass\\)\\|m\\(atch\\|odule\\|ethod\\|utable\\)\\|s\\(ig\\|truct\\)\\|begin\\|e\\(lse\\|x\\(ception\\|ternal\\)\\)\\|t\\(o\\|hen\\|ry\\|ype\\)\\|v\\(irtual\\|al\\)\\|w\\(h\\(ile\\|en\\)\\|ith\\)\\|i\\(f\\|n\\(herit\\)?\\)\\|f\\(or\\|un\\(ct\\(or\\|ion\\)\\)?\\)\\|let\\|do\\(wnto\\)?\\|parser?\\|rule\\|of\\)\\>\\|->\\|[;,|]"
  "Regexp for all recognized keywords.")

(defconst wavescope-match-|-keyword-regexp
  "\\<\\(and\\|fun\\(ction\\)?\\|type\\|with\\|parser?\\)\\>\\|[[({|=]"
  "Regexp for keywords supporting case match.")

(defconst wavescope-operator-regexp "[---+*/=<>@^&|]\\|:>\\|::\\|\\<\\(or\\|l\\(and\\|x?or\\|s[lr]\\)\\|as[lr]\\|mod\\)\\>"
  "Regexp for all operators.")

(defconst wavescope-kwop-regexp (concat wavescope-keyword-regexp "\\|=")
  "Regexp for all keywords, and the = operator which is generally
considered as a special keyword.")

(defconst wavescope-matching-keyword-regexp
  "\\<\\(and\\|do\\(ne\\)?\\|e\\(lse\\|nd\\)\\|in\\|then\\|\\(down\\)?to\\)\\>\\|>\\."
  "Regexp matching Caml keywords which act as end block delimiters.")

(defconst wavescope-leading-kwop-regexp
  (concat wavescope-matching-keyword-regexp "\\|\\<with\\>\\|[|>]?\\]\\|>?}\\|[|)]\\|;;")
  "Regexp matching Caml keywords which need special indentation.")

(defconst wavescope-governing-phrase-regexp
  "\\<\\(val\\|type\\|m\\(ethod\\|odule\\)\\|c\\(onstraint\\|lass\\)\\|in\\(herit\\|itializer\\)\\|ex\\(ternal\\|ception\\)\\|open\\|let\\|object\\|include\\)\\>"
  "Regexp matching wavescope phrase delimitors.")

(defconst wavescope-governing-phrase-regexp-with-break
  (concat wavescope-governing-phrase-regexp "\\|;;"))

(defconst wavescope-keyword-alist
  '(("module" . wavescope-default-indent)
    ("class" . wavescope-class-indent)
    ("sig" . wavescope-sig-struct-indent)
    ("struct" . wavescope-sig-struct-indent)
    ("method" . wavescope-method-indent)
    ("object" . wavescope-begin-indent)
    ("begin" . wavescope-begin-indent)
    (".<" . wavescope-begin-indent)
    ("for" . wavescope-for-while-indent)
    ("while" . wavescope-for-while-indent)
    ("do" . wavescope-do-indent)
    ("type" . wavescope-type-indent) ; in some cases, `type' acts like a match
    ("val" . wavescope-val-indent)
    ("fun" . wavescope-fun-indent)
    ("if" . wavescope-if-then-else-indent)
    ("then" . wavescope-if-then-else-indent)
    ("else" . wavescope-if-then-else-indent)
    ("let" . wavescope-let-indent)
    ("match" . wavescope-match-indent)
    ("try" . wavescope-try-indent)
    ("rule" . wavescope-rule-indent)

    ;; Case match keywords
    ("function" . wavescope-function-indent)
    ("with" . wavescope-with-indent)
    ("parse" . wavescope-parse-indent)
    ("parser" . wavescope-parser-indent)

    ;; Default indentation keywords
    ("when" . wavescope-default-indent)
    ("functor" . wavescope-default-indent)
    ("exception" . wavescope-default-indent)
    ("inherit" . wavescope-default-indent)
    ("initializer" . wavescope-default-indent)
    ("constraint" . wavescope-default-indent)
    ("virtual" . wavescope-default-indent)
    ("mutable" . wavescope-default-indent)
    ("external" . wavescope-default-indent)
    ("in" . wavescope-in-indent)
    ("of" . wavescope-default-indent)
    ("to" . wavescope-default-indent)
    ("downto" . wavescope-default-indent)
    (".<" . wavescope-default-indent)
    ("[" . wavescope-default-indent)
    ("(" . wavescope-default-indent)
    ("{" . wavescope-default-indent)
    ("->" . wavescope-default-indent)
    ("|" . wavescope-default-indent))
"Association list of indentation values based on governing keywords.")

(defconst wavescope-leading-kwop-alist
  '(("|" . wavescope-find-|-match)
    ("}" . wavescope-find-match)
    (">}" . wavescope-find-match)
    (">." . wavescope-find-match)
    (")" . wavescope-find-match)
    ("]" . wavescope-find-match)
    ("|]" . wavescope-find-match)
    (">]" . wavescope-find-match)
    ("end" . wavescope-find-match)
    ("done" . wavescope-find-done-match)
    ("in"  . wavescope-find-in-match)
    ("with" . wavescope-find-with-match)
    ("else" . wavescope-find-else-match)
    ("then" . wavescope-find-match)
    ("do" . wavescope-find-do-match)
    ("to" . wavescope-find-match)
    ("downto" . wavescope-find-match)
    ("and" . wavescope-find-and-match))
  "Association list used in Wavescope mode for skipping back over nested blocks.")

(defun wavescope-find-meaningful-word ()
  "Look back for a word, skipping comments and blanks.
Returns the actual text of the word, if found."
  (let ((found nil) (kwop nil))
    (while (and (not found)
		(re-search-backward
		 (concat
		  "[^ \t\n'_0-9" wavescope-alpha "]\\|\\<\\(\\w\\|_\\)+\\>\\|\\*)")
		 (point-min) t))
      (setq kwop (wavescope-match-string 0))
      (if kwop
	  (if (wavescope-in-comment-p)
	      (wavescope-beginning-of-literal-or-comment-fast)
	    (setq found t))
	(setq found t)))
    (if found kwop (goto-char (point-min)) nil)))

(defconst wavescope-find-kwop-regexp
  (concat wavescope-matching-keyword-regexp "\\|\\<\\(for\\|while\\|do\\|if\\|begin\\|s\\(ig\\|truct\\)\\|object\\)\\>\\|[][(){}]\\|\\.<\\|>\\.\\|\\*)"))
(defun wavescope-make-find-kwop-regexp (kwop-regexp)
  (concat wavescope-find-kwop-regexp "\\|" kwop-regexp))

(defun wavescope-find-kwop (kwop-regexp &optional do-not-skip-regexp)
  "Look back for a Caml keyword or operator matching KWOP-REGEXP.
Skips blocks etc...

Ignore occurences inside literals and comments.
If found, return the actual text of the keyword or operator."
  (let ((found nil) (kwop nil))
    (while (and (not found)
		(re-search-backward kwop-regexp (point-min) t)
		(setq kwop (wavescope-match-string 0)))
      (cond
       ((wavescope-in-literal-or-comment-p)
	(wavescope-beginning-of-literal-or-comment-fast))
       ((looking-at "[]})]")
	(wavescope-backward-up-list))
       ((wavescope-at-phrase-break-p)
	(setq found t))
       ((and do-not-skip-regexp (looking-at do-not-skip-regexp))
	(if (and (string= kwop "|") (char-equal ?| (preceding-char)))
	    (backward-char)
	  (setq found t)))
       ((looking-at wavescope-matching-keyword-regexp)
	(funcall (cdr (assoc (wavescope-match-string 0)
			     wavescope-leading-kwop-alist))))
       (t (setq found t))))
    (if found kwop (goto-char (point-min)) nil)))

(defun wavescope-find-match ()
  (wavescope-find-kwop wavescope-find-kwop-regexp))

(defconst wavescope-find-,-match-regexp
  (wavescope-make-find-kwop-regexp
   "\\<\\(and\\|match\\|begin\\|else\\|exception\\|then\\|try\\|with\\|or\\|fun\\|function\\|let\\|do\\)\\>\\|->\\|[[{(]"))
(defun wavescope-find-,-match ()
  (wavescope-find-kwop wavescope-find-,-match-regexp))

(defconst wavescope-find-with-match-regexp
  (wavescope-make-find-kwop-regexp
   "\\<\\(match\\|try\\|module\\|begin\\|with\\)\\>\\|[[{(]"))
(defun wavescope-find-with-match ()
  (let ((kwop (wavescope-find-kwop wavescope-find-with-match-regexp
				"\\<with\\>")))
    (if (string= kwop "with")
	(progn
	  (wavescope-find-with-match)
	  (wavescope-find-with-match)))
    kwop))

(defconst wavescope-find-in-match-regexp
  (wavescope-make-find-kwop-regexp "\\<let\\>"))
(defun wavescope-find-in-match ()
  (let ((kwop (wavescope-find-kwop wavescope-find-in-match-regexp "\\<and\\>")))
    (cond ((string= kwop "and") (wavescope-find-in-match))
	  (t kwop))))

(defconst wavescope-find-else-match-regexp
  (wavescope-make-find-kwop-regexp ";"))
(defun wavescope-find-else-match ()
  (let ((kwop (wavescope-find-kwop wavescope-find-else-match-regexp
				     "\\<then\\>")))
    (cond ((string= kwop "then")
	   (wavescope-find-match) kwop)
	  ((string= kwop ";")
	   (wavescope-find-semi-colon-match)
	   (wavescope-find-else-match) kwop))))

(defun wavescope-find-do-match ()
  (let ((kwop (wavescope-find-kwop wavescope-find-kwop-regexp
				   "\\<\\(down\\)?to\\>")))
    (if (or (string= kwop "to") (string= kwop "downto"))
	(wavescope-find-match) kwop)))

(defun wavescope-find-done-match ()
  (let ((kwop (wavescope-find-kwop wavescope-find-kwop-regexp "\\<do\\>")))
    (if (string= kwop "do")
	(wavescope-find-do-match) kwop)))

(defconst wavescope-find-and-match-regexp
  "\\<\\(do\\(ne\\)?\\|e\\(lse\\|nd\\)\\|in\\|then\\|\\(down\\)?to\\)\\>\\|\\<\\(for\\|while\\|do\\|if\\|begin\\|s\\(ig\\|truct\\)\\|class\\)\\>\\|[][(){}]\\|\\*)\\|\\<\\(rule\\|exception\\|let\\|in\\|type\\|val\\|module\\)\\>")
(defconst wavescope-find-and-match-regexp-dnr
  (concat wavescope-find-and-match-regexp "\\|\\<and\\>"))
(defun wavescope-find-and-match (&optional do-not-recurse)
  (let* ((kwop (wavescope-find-kwop (if do-not-recurse
				     wavescope-find-and-match-regexp-dnr
				   wavescope-find-and-match-regexp)
				 "\\<and\\>"))
	 (old-point (point)))
    (cond ((or (string= kwop "type") (string= kwop "module"))
	   (let ((kwop2 (wavescope-find-meaningful-word)))
	     (cond ((string= kwop2 "with")
		    kwop2)
		   ((string= kwop2 "and")
		    (wavescope-find-and-match))
		   ((and (string= kwop "module")
			(string= kwop2 "let"))
		    kwop2)
		   (t (goto-char old-point) kwop))))
	  (t kwop))))

(defconst wavescope-find-=-match-regexp
  (wavescope-make-find-kwop-regexp "\\<\\(val\\|let\\|m\\(ethod\\|odule\\)\\|type\\|class\\|when\\|i[fn]\\)\\>\\|="))
(defun wavescope-find-=-match ()
  (let ((kwop (wavescope-find-kwop wavescope-find-=-match-regexp
				"\\<\\(and\\|in\\)\\>\\|=")))
    (cond
     ((string= kwop "and")
      (wavescope-find-and-match))
     ((and (string= kwop "=")
	   (not (wavescope-false-=-p)))
      (while (and (string= kwop "=")
		  (not (wavescope-false-=-p)))
	(setq kwop (wavescope-find-=-match)))
      kwop)
     (t kwop))))

(defun wavescope-if-when-= ()
  (save-excursion
    (wavescope-find-=-match)
    (looking-at "\\<\\(if\\|when\\)\\>")))

(defun wavescope-captive-= ()
  (save-excursion
    (wavescope-find-=-match)
    (looking-at "\\<\\(let\\|if\\|when\\|module\\|type\\|class\\)\\>")))

(defconst wavescope-find-|-match-regexp
  (wavescope-make-find-kwop-regexp
   "\\<\\(with\\|fun\\(ction\\)?\\|type\\|parser?\\)\\>\\|[=|]"))
(defun wavescope-find-|-match ()
  (let* ((kwop (wavescope-find-kwop wavescope-find-|-match-regexp
				 "\\<\\(and\\|with\\)\\>\\||"))
	 (old-point (point)))
    (cond ((string= kwop "and")
	   (setq old-point (point))
	   (setq kwop (wavescope-find-and-match))
	   (goto-char old-point)
	   kwop)
	  ((and (string= kwop "|")
		(looking-at "|[^|]")
		(wavescope-in-indentation-p))
	   kwop)
	  ((string= kwop "|") (wavescope-find-|-match))
	  ((and (string= kwop "=")
		(or (looking-at "=[ \t]*\\((\\*\\|$\\)")
		    (wavescope-false-=-p)
		    (not (string= (save-excursion (wavescope-find-=-match))
				  "type"))))
	   (wavescope-find-|-match))
	  ((string= kwop "parse")
	   (if (and (string-match "\\.mll" (buffer-name))
		    (save-excursion
		      (string= (wavescope-find-meaningful-word) "=")))
	       kwop (wavescope-find-|-match)))
	  (t kwop))))

(defconst wavescope-find-->-match-regexp
  (wavescope-make-find-kwop-regexp "\\<\\(external\\|val\\|method\\|let\\|with\\|fun\\(ction\\|ctor\\)?\\|parser\\)\\>\\|[|:;]"))
(defun wavescope-find-->-match ()
  (let ((kwop (wavescope-find-kwop wavescope-find-->-match-regexp "\\<with\\>")))
    (cond
     ((string= kwop "|")
      (if (wavescope-in-indentation-p)
	  kwop
	(prog2 (forward-char -1) (wavescope-find-->-match))))
     ((not (string= kwop ":")) kwop)
     ;; If we get this far, we know we're looking at a colon.
     ((or (char-equal (char-before) ?:)
	  (char-equal (char-after (1+ (point))) ?:)
	  (char-equal (char-after (1+ (point))) ?>))
      (wavescope-find-->-match))
     ;; Patch by T. Freeman
     (t (let ((oldpoint (point))
	      (match (wavescope-find-->-match)))
	  (if (looking-at ":")
	      match
	    (progn
	      ;; Go back to where we were before the recursive call.
	      (goto-char oldpoint)
	      kwop)))))))

(defconst wavescope-find-semi-colon-match-regexp
  (wavescope-make-find-kwop-regexp ";[ \t]*\\((\\*\\|$\\)\\|->\\|\\<\\(let\\|method\\|with\\|try\\|initializer\\)\\>"))
(defun wavescope-find-semi-colon-match (&optional leading-semi-colon)
  (wavescope-find-kwop wavescope-find-semi-colon-match-regexp
			 "\\<\\(in\\|end\\|and\\|do\\|with\\)\\>")
  ;; We don't need to find the keyword matching `and' since we know it's `let'!
  (cond
   ((looking-at ";[ \t]*\\((\\*\\|$\\)")
    (forward-line 1)
    (while (or (wavescope-in-comment-p)
	       (looking-at "^[ \t]*\\((\\*\\|$\\)"))
      (forward-line 1))
    (back-to-indentation)
    (current-column))
   ((and leading-semi-colon
	 (looking-at "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*[^ \t\n]")
	 (not (looking-at "[[{(][|<]?[ \t]*\\((\\*\\|$\\)")))
    (current-column))
   ((looking-at "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*\\((\\*\\|$\\)")
    (wavescope-back-to-paren-or-indentation t)
    (+ (current-column) wavescope-default-indent))
   ((looking-at "\\(\\.<\\|(\\|\\[[<|]?\\|{<?\\)[ \t]*[^ \t\n]")
    (wavescope-search-forward-paren)
    (current-column))
   ((looking-at "\\<method\\>[ \t]*\\((\\*\\|$\\)")
    (wavescope-back-to-paren-or-indentation)
    (+ (current-column) wavescope-method-indent))
   ((looking-at "\\<begin\\>[ \t]*\\((\\*\\|$\\)")
    (wavescope-back-to-paren-or-indentation t)
    (+ (current-column) wavescope-begin-indent))
   ((looking-at "->")
    (if (save-excursion
	  (wavescope-find-->-match)
	  (looking-at "\\<\\(with\\|fun\\(ction\\)?\\|parser\\)\\>\\||"))
	(progn
	  (wavescope-back-to-paren-or-indentation)
	  (+ (current-column) wavescope-default-indent))
      (wavescope-find-semi-colon-match)))
   ((looking-at "\\<end\\>")
    (wavescope-find-match)
    (wavescope-find-semi-colon-match))
   ((looking-at "\\<in\\>")
    (wavescope-find-in-match)
    (wavescope-back-to-paren-or-indentation)
    (+ (current-column) wavescope-in-indent))
   ((looking-at "\\<let\\>")
    (+ (current-column) wavescope-let-indent))
   (t (wavescope-back-to-paren-or-indentation t)
      (+ (current-column) wavescope-default-indent))))

(defconst wavescope-find-phrase-indentation-regexp
  (wavescope-make-find-kwop-regexp (concat wavescope-governing-phrase-regexp
					"\\|\\<and\\>")))
(defconst wavescope-find-phrase-indentation-regexp-pb
  (concat wavescope-find-phrase-indentation-regexp "\\|;;"))
(defconst wavescope-find-phrase-indentation-class-regexp
  (concat wavescope-matching-keyword-regexp "\\|\\<class\\>"))
(defun wavescope-find-phrase-indentation (&optional phrase-break)
  (if (and (looking-at "\\<\\(type\\|module\\)\\>") (> (point) (point-min))
	   (save-excursion
	     (wavescope-find-meaningful-word)
	     (looking-at "\\<\\(module\\|with\\|and\\|let\\)\\>")))
      (progn
	(wavescope-find-meaningful-word)
	(+ (current-column) wavescope-default-indent))
    (let ((looking-at-and (looking-at "\\<and\\>"))
	  (kwop (wavescope-find-kwop
		 (if phrase-break
		     wavescope-find-phrase-indentation-regexp-pb
		   wavescope-find-phrase-indentation-regexp)
		 "\\<\\(end\\|and\\|with\\|in\\)\\>"))
	  (tmpkwop nil) (curr nil))
      (if (and kwop (string= kwop "and"))
	  (setq kwop (wavescope-find-and-match)))
      (if (not kwop) (current-column)
	(cond
	 ((string= kwop "end")
	  (if (not (save-excursion
		     (setq tmpkwop (wavescope-find-match))
		     (setq curr (point))
		     (string= tmpkwop "object")))
	      (prog2
		  (wavescope-find-match)
		  (wavescope-find-phrase-indentation phrase-break))
	    (wavescope-find-kwop wavescope-find-phrase-indentation-class-regexp)
	    (current-column)))
	 ((and (string= kwop "with")
	       (not (save-excursion
		      (setq tmpkwop (wavescope-find-with-match))
		      (setq curr (point))
		      (string= tmpkwop "module"))))
	  (goto-char curr)
	  (wavescope-find-phrase-indentation phrase-break))
	 ((and (string= kwop "in")
	       (not (save-excursion
		      (setq tmpkwop (wavescope-find-in-match))
		      (if (string= tmpkwop "and")
			  (setq tmpkwop (wavescope-find-and-match)))
		      (setq curr (point))
		      (and (string= tmpkwop "let")
			   (not (wavescope-looking-at-expression-let))))))
	  (goto-char curr)
	  (wavescope-find-phrase-indentation phrase-break))
	 ((wavescope-at-phrase-break-p)
	  (end-of-line)
	  (wavescope-skip-blank-and-comments)
	  (current-column))
	 ((string= kwop "let")
	  (if (wavescope-looking-at-expression-let)
	      (wavescope-find-phrase-indentation phrase-break)
	    (current-column)))
	 ((string= kwop "with")
	  (current-column))
	 ((string= kwop "end")
	  (current-column))
	 ((string= kwop "in")
	  (wavescope-find-in-match)
	  (current-column))
	 ((string= kwop "class")
	  (wavescope-back-to-paren-or-indentation)
	  (current-column))
	 ((looking-at "\\<\\(object\\|s\\(ig\\|truct\\)\\)\\>")
	  (wavescope-back-to-paren-or-indentation t)
	  (+ (wavescope-assoc-indent kwop) (current-column)))
	 ((or (string= kwop "type") (string= kwop "module"))
	  (if (or (wavescope-looking-at-false-type)
		  (wavescope-looking-at-false-module))
	      (if looking-at-and (current-column)
		(wavescope-find-meaningful-word)
		(if (looking-at "\\<and\\>")
		    (prog2
			(wavescope-find-and-match)
			(wavescope-find-phrase-indentation phrase-break))
		  (wavescope-find-phrase-indentation phrase-break)))
	    (current-column)))
	 ((looking-at
	   "\\(\\.<\\|(\\|\\[[<|]?\\|{<?\\)[ \t]*\\((\\*\\|$\\)")
	  (wavescope-back-to-paren-or-indentation)
	  (+ (current-column) wavescope-default-indent))
	 ((looking-at "\\(\\.<\\|(\\|\\[[<|]?\\|{<?\\)[ \t]*[^ \t\n]")
	  (wavescope-search-forward-paren)
	  (current-column))
	 ((string= kwop "open") ; compatible with Caml Light `#open'
	  (wavescope-back-to-paren-or-indentation) (current-column))
	 (t (current-column)))))))

(defconst wavescope-back-to-paren-or-indentation-regexp
  "[][(){}]\\|\\.<\\|>\\.\\|\\*)\\|^[ \t]*\\(.\\|\n\\)")
(defconst wavescope-back-to-paren-or-indentation-in-regexp
  (concat "\\<in\\>\\|" wavescope-back-to-paren-or-indentation-regexp))
(defconst wavescope-back-to-paren-or-indentation-lazy-regexp
  "[])}]\\|\\.<\\|>\\.\\|\\*)\\|^[ \t]*\\(.\\|\n\\)")
(defconst wavescope-back-to-paren-or-indentation-lazy-in-regexp
  (concat "\\<in\\>\\|" wavescope-back-to-paren-or-indentation-regexp))
(defun wavescope-back-to-paren-or-indentation (&optional forward-in)
  "Search backwards for the first open paren in line, or skip to indentation.
Returns t iff skipped to indentation."
  (if (or (bolp) (wavescope-in-indentation-p)) (prog2 (back-to-indentation) t)
    (let ((kwop (wavescope-find-kwop
		 (if wavescope-lazy-paren
		     (if forward-in
			 wavescope-back-to-paren-or-indentation-lazy-in-regexp
		       wavescope-back-to-paren-or-indentation-lazy-regexp)
		   (if forward-in
		       wavescope-back-to-paren-or-indentation-in-regexp
		     wavescope-back-to-paren-or-indentation-regexp))
		 "\\<and\\|with\\|in\\>"))
	  (retval))
      (if (string= kwop "with")
	  (let ((with-point (point)))
	    (setq kwop (wavescope-find-with-match))
	    (if (or (string= kwop "match") (string= kwop "try"))
		(wavescope-find-kwop
		 wavescope-back-to-paren-or-indentation-regexp
		 "\\<and\\>")
	      (setq kwop "with") (goto-char with-point))))
      (setq retval
	    (cond
	     ((string= kwop "with") nil)
	     ((string= kwop "in") (wavescope-in-indentation-p))
	     ((looking-at "[[{(]\\|\\.<") (wavescope-search-forward-paren) nil)
	     (t (back-to-indentation) t)))
      (cond
       ((looking-at "|[^|]")
	(prog2 (re-search-forward "|[^|][ \t]*") nil))
       ((and forward-in (string= kwop "in"))
	(wavescope-find-in-match)
	(wavescope-back-to-paren-or-indentation forward-in)
	(if (looking-at "\\<\\(let\\|and\\)\\>")
	    (forward-char wavescope-in-indent)) nil)
       (t retval)))))

(defun wavescope-search-forward-paren ()
  (if wavescope-lazy-paren (wavescope-back-to-paren-or-indentation)
    (re-search-forward "\\(\\.<\\|(\\|\\[[<|]?\\|{<?\\)[ \t]*")))

(defun wavescope-add-default-indent (leading-operator)
  (if leading-operator 0 wavescope-default-indent))

(defconst wavescope-compute-argument-indent-regexp
  (wavescope-make-find-kwop-regexp wavescope-kwop-regexp))
(defun wavescope-compute-argument-indent (leading-operator)
  (let ((old-point (save-excursion (beginning-of-line) (point)))
	(match-end-point) (kwop))
    (setq kwop (wavescope-find-kwop wavescope-compute-argument-indent-regexp
				 wavescope-keyword-regexp))
    (setq match-end-point (+ (point) (length kwop))) ; match-end is invalid !
    (cond
     ((and (string= kwop "->")
	   (not (looking-at "->[ \t]*\\((\\*.*\\)?$")))
      (let* (matching-kwop matching-pos)
	(save-excursion
	  (setq matching-kwop (wavescope-find-->-match))
	  (setq matching-pos (point)))
	(cond
	 ((string= matching-kwop ":")
	  (goto-char matching-pos)
	  (wavescope-find-->-match) ; matching `val' or `let'
	  (+ (current-column) wavescope-val-indent))
	 ((string= matching-kwop "|")
	  (goto-char matching-pos)
	  (+ (wavescope-add-default-indent leading-operator)
	     (current-column) wavescope-|-extra-unindent wavescope-default-indent))
	 (t
	  (wavescope-back-to-paren-or-indentation)
	  (+ (wavescope-add-default-indent leading-operator) (current-column))))))
     ((string= kwop "fun")
      (wavescope-back-to-paren-or-indentation t)
      (+ (current-column)
	 (wavescope-assoc-indent kwop)))
     ((<= old-point (point))
      (+ (wavescope-add-default-indent leading-operator) (current-column)))
     (t
      (forward-line 1)
      (beginning-of-line)
      (while (or (wavescope-in-comment-p) (looking-at "^[ \t]*\\((\\*.*\\)?$"))
	(forward-line 1))
      (wavescope-back-to-paren-or-indentation)
      (if (save-excursion (goto-char match-end-point)
			  (looking-at "[ \t]*\\((\\*.*\\)?$"))
	  (+ (wavescope-add-default-indent leading-operator)
	     (current-column))
	(current-column))))))

(defun wavescope-indent-from-paren (&optional leading-operator)
  (if (looking-at
       "\\(\\.<\\|(\\|\\[[<|]?\\|{<?\\)[ \t]*\\((\\*\\|$\\)")
      (prog2
	  (wavescope-back-to-paren-or-indentation t)
	  (+ wavescope-default-indent
	     (current-column))) ; parens do not operate
    (wavescope-search-forward-paren)
    (+ (wavescope-add-default-indent leading-operator)
       (current-column))))

(defconst wavescope-compute-normal-indent-regexp
  (concat wavescope-compute-argument-indent-regexp "\\|^.[ \t]*"))
(defun wavescope-compute-normal-indent ()
  (let ((leading-operator (looking-at wavescope-operator-regexp)))
    (beginning-of-line)
    ;; Operator ending previous line used to be considered leading
    ;; (save-excursion
    ;;  (wavescope-find-meaningful-word)
    ;;  (if (looking-at wavescope-operator-regexp)
    ;;	  (setq leading-operator t)))
    (save-excursion
      (let ((kwop (wavescope-find-kwop (if leading-operator
					wavescope-compute-argument-indent-regexp
				      wavescope-compute-normal-indent-regexp)
				    wavescope-keyword-regexp)))
	(if (string= kwop "and") (setq kwop (wavescope-find-and-match)))
	(while (or (and (string= kwop "=")
			(wavescope-false-=-p))
		   (and (looking-at "^[ \t]*\\((\\*.*\\)?$")
			(not (= (point) (point-min)))))
	  (setq kwop (wavescope-find-kwop wavescope-compute-normal-indent-regexp
				       wavescope-keyword-regexp))
	  (if (string= kwop "and") (setq kwop (wavescope-find-and-match))))
	(if (not kwop) (current-column)
	  (cond
	   ((wavescope-at-phrase-break-p)
	    (wavescope-find-phrase-indentation t))
	   ((and (string= kwop "|") (not  (char-equal ?\[ (preceding-char))))
	    (wavescope-backward-char)
	    (wavescope-back-to-paren-or-indentation)
	    (+ (current-column) wavescope-default-indent
	       (wavescope-add-default-indent leading-operator)))
	   ((or (looking-at "[[{(]\\|\\.<")
		(and (looking-at "[<|]")
		     (char-equal ?\[ (preceding-char))
		     (prog2 (wavescope-backward-char) t))
		(and (looking-at "<")
		     (char-equal ?\{ (preceding-char))
		     (prog2 (wavescope-backward-char) t)))
	    (wavescope-indent-from-paren leading-operator))
	   ((looking-at "->")
	    (let ((keyword-->-match (save-excursion (wavescope-find-->-match))))
	      (cond ((string= keyword-->-match "|")
		     (wavescope-find-->-match)
		     (re-search-forward "|[ \t]*")
		     (+ (current-column) wavescope-default-indent))
		    ((string= keyword-->-match ":")
		     (wavescope-find-->-match) ; slow, better to save the column
		     (wavescope-find-->-match) ; matching `val' or `let'
		     (+ (current-column) wavescope-val-indent))
		    (t (wavescope-back-to-paren-or-indentation)
		       (+ wavescope-default-indent (current-column))))))
	   ((looking-at wavescope-keyword-regexp)
	    (cond ((string= kwop ";")
		   (if (looking-at ";[ \t]*\\((\\*\\|$\\)")
		       (wavescope-find-semi-colon-match)
		     (wavescope-back-to-paren-or-indentation t)
		     (+ (current-column) wavescope-default-indent)))
		  ((string= kwop ",")
		   (if (looking-at ",[ \t]*\\((\\*\\|$\\)")
		       (progn
			 (setq kwop (wavescope-find-,-match))
			 (if (or (looking-at "[[{(]\\|\\.<")
				 (and (looking-at "[<|]")
				      (char-equal ?\[ (preceding-char))
				      (prog2 (wavescope-backward-char) t))
				 (and (looking-at "<")
				      (char-equal ?\{ (preceding-char))
				      (prog2 (wavescope-backward-char) t)))
			     (wavescope-indent-from-paren t)
			   (wavescope-back-to-paren-or-indentation t)
			   (+ (current-column)
			      (wavescope-assoc-indent kwop))))
		     (wavescope-back-to-paren-or-indentation t)
		     (+ (current-column) wavescope-default-indent)))
		  ((and (looking-at "\\<\\(in\\|begin\\|do\\)\\>\\|->")
			(not (looking-at
			      "\\([a-z]+\\|->\\)[ \t]*\\((\\*\\|$\\)")))
		   (if (string= kwop "in")
		       (re-search-forward "\\<in\\>[ \t]*")
		     (wavescope-back-to-paren-or-indentation t))
		   (+ (current-column)
		      (wavescope-add-default-indent leading-operator)
		      (if (string= kwop "in") 0 ; aligned, do not indent
			(wavescope-assoc-indent kwop))))
		  ((string= kwop "with")
		   (if (save-excursion
			 (let ((tmpkwop (wavescope-find-with-match)))
			   (or (string= tmpkwop "module")
			       (string= tmpkwop "{"))))
		       (prog2
			   (wavescope-back-to-paren-or-indentation)
			   (+ (current-column) wavescope-default-indent))
		     (wavescope-back-to-paren-or-indentation)
		     (+ (current-column)
			(wavescope-assoc-indent kwop t))))
		  ((string= kwop "in")
		   (wavescope-find-in-match)
		   (wavescope-back-to-paren-or-indentation)
		   (+ (current-column) wavescope-in-indent))
		  ((or (string= kwop "let") (string= kwop "and"))
		   (wavescope-back-to-paren-or-indentation t)
		   (+ (current-column)
		      wavescope-default-indent
		      (wavescope-assoc-indent kwop t)))
		  (t (wavescope-back-to-paren-or-indentation t)
		     (+ (current-column)
			(wavescope-assoc-indent kwop t)))))
	   ((and (looking-at "=") (not (wavescope-false-=-p)))
	    (let ((current-column-module-type nil))
	      (+
	       (progn
		 (wavescope-find-=-match)
		 (save-excursion
		   (if (looking-at "\\<and\\>") (wavescope-find-and-match))
		   (cond
		    ((looking-at "\\<type\\>")
		     (wavescope-find-meaningful-word)
		     (if (looking-at "\\<module\\>")
			 (progn
			   (setq current-column-module-type (current-column))
			   wavescope-default-indent)
		       (if (looking-at "\\<\\(with\\|and\\)\\>")
			   (progn
			     (wavescope-find-with-match)
			     (setq current-column-module-type (current-column))
			     wavescope-default-indent)
			 (re-search-forward "\\<type\\>")
			 (beginning-of-line)
			 (+ wavescope-type-indent
			    wavescope-|-extra-unindent))))
		    ((looking-at
		      "\\<\\(val\\|let\\|m\\(ethod\\|odule\\)\\|class\\|when\\|\\|for\\|if\\)\\>")
		     (let ((matched-string (wavescope-match-string 0)))
		       (wavescope-back-to-paren-or-indentation t)
		       (setq current-column-module-type (current-column))
		       (wavescope-assoc-indent matched-string)))
		    ((looking-at "\\<object\\>")
		     (wavescope-back-to-paren-or-indentation t)
		     (setq current-column-module-type (current-column))
		     (+ (wavescope-assoc-indent "object")
			wavescope-default-indent))
		    (t (wavescope-back-to-paren-or-indentation t)
		       (setq current-column-module-type
			     (+ (current-column) wavescope-default-indent))
		       wavescope-default-indent))))
	       (if current-column-module-type
		   current-column-module-type
		 (current-column)))))
	   (nil 0)
	   (t (wavescope-compute-argument-indent leading-operator))))))))

(defun wavescope-looking-at-expression-let ()
  (save-excursion
    (and (wavescope-find-meaningful-word)
	 (not (wavescope-at-phrase-break-p))
	 (or (looking-at "[[({;=]\\|\\.<\\|\\<\\(begin\\|i[fn]\\|do\\|t\\(ry\\|hen\\)\\|else\\|match\\|wh\\(ile\\|en\\)\\)\\>")
	     (looking-at wavescope-operator-regexp)))))

(defun wavescope-looking-at-false-module ()
  (save-excursion (wavescope-find-meaningful-word)
		  (looking-at "\\<\\(let\\|with\\|and\\)\\>")))

(defun wavescope-looking-at-false-sig-struct ()
  (save-excursion (wavescope-find-module)
		  (looking-at "\\<module\\>")))

(defun wavescope-looking-at-false-type ()
  (save-excursion (wavescope-find-meaningful-word)
		  (looking-at "\\<\\(class\\|with\\|module\\|and\\)\\>")))

(defun wavescope-looking-at-in-let ()
  (save-excursion (string= (wavescope-find-meaningful-word) "in")))

(defconst wavescope-find-module-regexp
  (wavescope-make-find-kwop-regexp "\\<module\\>"))
(defun wavescope-find-module ()
  (wavescope-find-kwop wavescope-find-module-regexp))

(defun wavescope-modify-syntax ()
  "Switch to modified internal syntax."
  (modify-syntax-entry ?. "w" wavescope-mode-syntax-table)
  (modify-syntax-entry ?_ "w" wavescope-mode-syntax-table))

(defun wavescope-restore-syntax ()
  "Switch back to interactive syntax."
  (modify-syntax-entry ?. "." wavescope-mode-syntax-table)
  (modify-syntax-entry ?_ "_" wavescope-mode-syntax-table))

(defun wavescope-indent-command (&optional from-leading-star)
  "Indent the current line in Wavescope mode.

Compute new indentation based on Caml syntax."
  (interactive "*")
  (let ((old-cfs case-fold-search))
    (if (not from-leading-star)
	(wavescope-auto-fill-insert-leading-star))
    (setq case-fold-search nil)
    (wavescope-modify-syntax)
    (save-excursion
      (back-to-indentation)
      (indent-line-to (wavescope-compute-indent)))
    (if (wavescope-in-indentation-p) (back-to-indentation))
    (setq case-fold-search old-cfs)
    (wavescope-restore-syntax)))

(defun wavescope-compute-indent ()
  (save-excursion
    (cond
     ((wavescope-in-comment-p)
      (cond
       ((looking-at "(\\*")
	(if wavescope-indent-leading-comments
	    (save-excursion
	      (while (and (progn (beginning-of-line)
				 (> (point) 1))
			  (progn (forward-line -1)
				 (back-to-indentation)
				 (wavescope-in-comment-p))))
	      (if (looking-at "[ \t]*$")
		  (progn
		    (wavescope-skip-blank-and-comments)
		    (if (or (looking-at "$") (wavescope-in-comment-p))
			0
		      (wavescope-compute-indent)))
		(forward-line 1)
		(wavescope-compute-normal-indent)))
	  (current-column)))
       ((looking-at "\\*\\**)")
	(wavescope-beginning-of-literal-or-comment-fast)
	(if (wavescope-leading-star-p)
	    (+ (current-column)
	       (if (save-excursion
		     (forward-line 1)
		     (back-to-indentation)
		     (looking-at "*")) 1
		 wavescope-comment-end-extra-indent))
	  (+ (current-column) wavescope-comment-end-extra-indent)))
       (wavescope-indent-comments
	(let ((star (and (wavescope-leading-star-p)
			 (looking-at "\\*"))))
	  (wavescope-beginning-of-literal-or-comment-fast)
	  (if star (re-search-forward "(") (re-search-forward "(\\*+[ \t]*"))
	  (current-column)))))
     ((wavescope-in-literal-p)
      (current-column))
     ((looking-at "\\<let\\>")
      (if (wavescope-looking-at-expression-let)
	  (if (wavescope-looking-at-in-let)
	      (progn
		(wavescope-find-meaningful-word)
		(wavescope-find-in-match)
		(wavescope-back-to-paren-or-indentation)
		(current-column))
	    (wavescope-compute-normal-indent))
	(wavescope-find-phrase-indentation)))
     ((looking-at wavescope-governing-phrase-regexp-with-break)
      (wavescope-find-phrase-indentation))
     ((and wavescope-sig-struct-align (looking-at "\\<\\(sig\\|struct\\)\\>"))
      (if (string= (wavescope-find-module) "module") (current-column)
	(wavescope-back-to-paren-or-indentation)
	(+ wavescope-default-indent (current-column))))
     ((looking-at ";") (wavescope-find-semi-colon-match t))
     ((or (looking-at "%\\|;;")
	  (and wavescope-support-camllight (looking-at "#"))
	  (looking-at "#\\<\\(open\\|load\\|use\\)\\>")) 0)
     ((looking-at wavescope-leading-kwop-regexp)
      (let ((kwop (wavescope-match-string 0)))
	(let* ((old-point (point))
	       (paren-match-p (looking-at "[|>]?[]})]\\|>\\."))
	       (need-not-back-kwop (string= kwop "and"))
	       (real-| (looking-at "|\\([^|]\\|$\\)"))
	       (matching-kwop
		(if (string= kwop "and")
		    (wavescope-find-and-match t)
		  (funcall (cdr (assoc kwop wavescope-leading-kwop-alist)))))
	       (match-|-keyword-p
		(and matching-kwop
		     (looking-at wavescope-match-|-keyword-regexp))))
	  (cond
	   ((and (string= kwop "|") real-|)
	    (cond
	     ((string= matching-kwop "|")
	      (if (not need-not-back-kwop)
		  (wavescope-back-to-paren-or-indentation))
	      (current-column))
	     ((and (string= matching-kwop "=")
		   (not (wavescope-false-=-p)))
	      (re-search-forward "=[ \t]*")
	      (current-column))
	     (match-|-keyword-p
	      (if (not need-not-back-kwop)
		  (wavescope-back-to-paren-or-indentation))
	      (- (+ (wavescope-assoc-indent
		     matching-kwop t)
		    (current-column))
		 (if (string= matching-kwop "type") 0
		   wavescope-|-extra-unindent)))
	     (t (goto-char old-point)
		(wavescope-compute-normal-indent))))
	   ((and (string= kwop "|") (not real-|))
	    (goto-char old-point)
	    (wavescope-compute-normal-indent))
	   ((and
	     (looking-at "\\(\\[|?\\|{<?\\|(\\|\\.<\\)[ \t]*[^ \t\n]")
	     (not (looking-at "\\([[{(][|<]?\\|\\.<\\)[ \t]*\\((\\*\\|$\\)")))
	    (if (and (string= kwop "|") real-|)
		(current-column)
	      (if (not paren-match-p)
		  (wavescope-search-forward-paren))
	      (if wavescope-lazy-paren
		  (wavescope-back-to-paren-or-indentation))
	      (current-column)))
	   ((and (string= kwop "with")
		 (or (string= matching-kwop "module")
		     (string= matching-kwop "struct")))
	    (wavescope-back-to-paren-or-indentation nil)
	    (+ (current-column) wavescope-default-indent))
	   ((not need-not-back-kwop)
	    (wavescope-back-to-paren-or-indentation (not (string= kwop "in")))
	    (current-column))
	   (t (current-column))))))
     (t (wavescope-compute-normal-indent)))))

(defun wavescope-split-string ()
  "Called whenever a line is broken inside a Caml string literal."
  (insert-before-markers "\" ^\"")
  (wavescope-backward-char))

(defadvice newline-and-indent (around
			       wavescope-newline-and-indent
			       activate)
  "Handle multi-line strings in Wavescope mode."
    (let ((hooked (and (eq major-mode 'wavescope-mode) (wavescope-in-literal-p)))
	  (split-mark))
      (if (not hooked) nil
	(setq split-mark (set-marker (make-marker) (point)))
	(wavescope-split-string))
      ad-do-it
      (if (not hooked) nil
	(goto-char split-mark)
	(set-marker split-mark nil))))

(defun wavescope-electric ()
  "If inserting a | operator at beginning of line, reindent the line."
  (interactive "*")
  (let ((electric (and wavescope-electric-indent
		       (wavescope-in-indentation-p)
		       (not (wavescope-in-literal-p))
		       (not (wavescope-in-comment-p)))))
    (self-insert-command 1)
    (if (and electric
	     (not (and (char-equal ?| (preceding-char))
		       (save-excursion
			 (wavescope-backward-char)
			 (wavescope-find-|-match)
			 (not (looking-at wavescope-match-|-keyword-regexp))))))
	(indent-according-to-mode))))

(defun wavescope-electric-rp ()
  "If inserting a ) operator or a comment-end at beginning of line,
reindent the line."
  (interactive "*")
  (let ((electric (and wavescope-electric-indent
		       (or (wavescope-in-indentation-p)
			   (char-equal ?* (preceding-char)))
		       (not (wavescope-in-literal-p))
		       (or (not (wavescope-in-comment-p))
			   (save-excursion
			     (back-to-indentation)
			     (looking-at "\\*"))))))
    (self-insert-command 1)
    (if electric
	(indent-according-to-mode))))

(defun wavescope-electric-rc ()
  "If inserting a } operator at beginning of line, reindent the line.

Reindent also if } is inserted after a > operator at beginning of line.
Also, if the matching { is followed by a < and this } is not preceded
by >, insert one >."
  (interactive "*")
  (let* ((prec (preceding-char))
	 (look-bra (and wavescope-electric-close-vector
			(not (wavescope-in-literal-or-comment-p))
			(not (char-equal ?> prec))))
	 (electric (and wavescope-electric-indent
			(or (wavescope-in-indentation-p)
			    (and (char-equal ?> prec)
				 (save-excursion (wavescope-backward-char)
						 (wavescope-in-indentation-p))))
			(not (wavescope-in-literal-or-comment-p)))))
    (self-insert-command 1)
    (if look-bra
	(save-excursion
	  (let ((inserted-char
		 (save-excursion
		   (wavescope-backward-char)
		   (wavescope-backward-up-list)
		   (cond ((looking-at "{<") ">")
			 (t "")))))
	    (wavescope-backward-char)
	    (insert inserted-char))))
    (if electric (indent-according-to-mode))))

(defun wavescope-electric-rb ()
  "If inserting a ] operator at beginning of line, reindent the line.

Reindent also if ] is inserted after a | operator at beginning of line.
Also, if the matching [ is followed by a | and this ] is not preceded
by |, insert one |."
  (interactive "*")
  (let* ((prec (preceding-char))
	 (look-|-or-bra (and wavescope-electric-close-vector
			     (not (wavescope-in-literal-or-comment-p))
			     (not (and (char-equal ?| prec)
				       (not (char-equal
					     (save-excursion
					       (wavescope-backward-char)
					       (preceding-char)) ?\[))))))
	 (electric (and wavescope-electric-indent
			(or (wavescope-in-indentation-p)
			    (and (char-equal ?| prec)
				 (save-excursion (wavescope-backward-char)
						 (wavescope-in-indentation-p))))
			(not (wavescope-in-literal-or-comment-p)))))
    (self-insert-command 1)
    (if look-|-or-bra
	(save-excursion
	  (let ((inserted-char
		 (save-excursion
		   (wavescope-backward-char)
		   (wavescope-backward-up-list)
		   (cond ((looking-at "\\[|") "|")
			 (t "")))))
	    (wavescope-backward-char)
	    (insert inserted-char))))
    (if electric (indent-according-to-mode))))

(defun wavescope-abbrev-hook ()
  "If inserting a leading keyword at beginning of line, reindent the line."
  (if (not (wavescope-in-literal-or-comment-p))
      (let* ((bol (save-excursion (beginning-of-line) (point)))
	     (kw (save-excursion
		   (and (re-search-backward "^[ \t]*\\(\\w\\|_\\)+\\=" bol t)
			(wavescope-match-string 1)))))
	(if kw (progn
		   (insert " ")
		   (indent-according-to-mode)
		   (backward-delete-char-untabify 1))))))

(defun wavescope-skip-to-end-of-phrase ()
  (let ((old-point (point)))
    (if (and (string= (wavescope-find-meaningful-word) ";")
	     (char-equal (preceding-char) ?\;))
	(setq old-point (1- (point))))
    (goto-char old-point)
    (let ((kwop (wavescope-find-meaningful-word)))
      (goto-char (+ (point) (length kwop))))))

(defun wavescope-skip-blank-and-comments ()
  (skip-chars-forward " \t\n")
  (while (and (not (eobp)) (wavescope-in-comment-p)
	      (search-forward "*)" nil t))
    (skip-chars-forward " \t\n")))

(defun wavescope-skip-back-blank-and-comments ()
  (skip-chars-backward " \t\n")
  (while (save-excursion (wavescope-backward-char)
			 (and (> (point) (point-min)) (wavescope-in-comment-p)))
    (wavescope-backward-char)
    (wavescope-beginning-of-literal-or-comment) (skip-chars-backward " \t\n")))

(defconst wavescope-beginning-phrase-regexp
  "^#[ \t]*[a-z][_a-z]*\\>\\|\\<\\(end\\|type\\|module\\|sig\\|struct\\|class\\|exception\\|open\\|let\\)\\>\\|;;"
  "Regexp matching wavescope phrase delimitors.")
(defun wavescope-find-phrase-beginning ()
  "Find `real' phrase beginning and return point."
  (beginning-of-line)
  (wavescope-skip-blank-and-comments)
  (end-of-line)
  (wavescope-skip-to-end-of-phrase)
  (let ((old-point (point)))
    (wavescope-find-kwop wavescope-beginning-phrase-regexp)
    (while (and (> (point) (point-min)) (< (point) old-point)
		(or (not (looking-at wavescope-beginning-phrase-regexp))
		    (and (looking-at "\\<let\\>")
			 (wavescope-looking-at-expression-let))
		    (and (looking-at "\\<module\\>")
			 (wavescope-looking-at-false-module))
		    (and (looking-at "\\<\\(sig\\|struct\\)\\>")
			 (wavescope-looking-at-false-sig-struct))
		    (and (looking-at "\\<type\\>")
			 (wavescope-looking-at-false-type))))
      (if (looking-at "\\<end\\>")
	  (wavescope-find-match)
	(if (not (bolp)) (wavescope-backward-char))
	(setq old-point (point))
	(wavescope-find-kwop wavescope-beginning-phrase-regexp)))
    (if (wavescope-at-phrase-break-p)
	(prog2 (end-of-line) (wavescope-skip-blank-and-comments)))
    (back-to-indentation)
    (point)))

(defun wavescope-search-forward-end-iter (begin)
  (if (re-search-forward "\\<end\\>" (point-max) t)
      (let ((kwop) (iter))
	(save-excursion
	  (wavescope-backward-char 3)
	  (setq kwop (wavescope-find-match))
	  (cond
	   ((looking-at "\\<\\(object\\)\\>")
	    (wavescope-find-phrase-beginning))
	   ((and (looking-at "\\<\\(struct\\|sig\\)\\>")
		 (wavescope-looking-at-false-sig-struct))
	    (wavescope-find-phrase-beginning)))
	  (if (> (point) begin)
	      (setq iter t)))
	(cond
	 (iter
	  (wavescope-search-forward-end-iter begin))
	 ((and (string= kwop "sig")
	       (looking-at "[ \t\n]*\\(\\<with\\>[ \t\n]*\\<type\\>\\|=\\)"))
	  (wavescope-search-forward-end-iter begin))
	 (t t)))
    nil))
(defun wavescope-search-forward-end ()
  (wavescope-search-forward-end-iter (point)))

(defconst wavescope-inside-block-opening "\\<\\(struct\\|sig\\|object\\)\\>")
(defconst wavescope-inside-block-opening-full
  (concat wavescope-inside-block-opening "\\|\\<\\(module\\|class\\)\\>"))
(defconst wavescope-inside-block-regexp
  (concat wavescope-matching-keyword-regexp "\\|" wavescope-inside-block-opening))
(defun wavescope-inside-block-find-kwop ()
  (let ((kwop (wavescope-find-kwop wavescope-inside-block-regexp
				"\\<\\(and\\|end\\)\\>")))
    (if (string= kwop "and") (setq kwop (wavescope-find-and-match)))
    (if (string= kwop "with") (setq kwop nil))
    (if (string= kwop "end")
	(progn
	  (wavescope-find-match)
	  (wavescope-find-kwop wavescope-inside-block-regexp)
	  (wavescope-inside-block-find-kwop))
      kwop)))

(defun wavescope-inside-block-p ()
  (let ((begin) (end) (and-end) (kwop t))
    (save-excursion
      (if (looking-at "\\<and\\>")
	  (wavescope-find-and-match))
      (setq begin (point))
      (if (or (and (looking-at "\\<class\\>")
		   (save-excursion
		     (re-search-forward "\\<object\\>"
					(point-max) t)
		     (wavescope-find-phrase-beginning)
		     (> (point) begin)))
	      (and (looking-at "\\<module\\>")
		   (save-excursion
		     (re-search-forward "\\<\\(sig\\|struct\\)\\>"
					(point-max) t)
		     (wavescope-find-phrase-beginning)
		     (> (point) begin)))) ()
	(if (not (looking-at wavescope-inside-block-opening-full))
	    (setq kwop (wavescope-inside-block-find-kwop)))
	(if (not kwop) ()
	  (setq begin (point))
	  (if (not (wavescope-search-forward-end)) ()
	    (wavescope-backward-char 3)
	    (if (not (looking-at "\\<end\\>")) ()
	      (wavescope-forward-char 3)
	      (setq end (point))
	      (setq and-end (point))
	      (wavescope-skip-blank-and-comments)
	      (while (looking-at "\\<and\\>")
		(setq and-end (point))
		(if (not (wavescope-search-forward-end)) ()
		  (wavescope-backward-char 3)
		  (if (not (looking-at "\\<end\\>")) ()
		    (wavescope-forward-char 3)
		    (setq and-end (point))
		    (wavescope-skip-blank-and-comments))))
	      (list begin end and-end))))))))

(defun wavescope-move-inside-block-opening ()
  "Go to the beginning of the enclosing module or class.

Notice that white-lines (or comments) located immediately before a
module/class are considered enclosed in this module/class."
  (interactive)
  (let* ((old-point (point))
	 (kwop (wavescope-inside-block-find-kwop)))
    (if (not kwop)
	(goto-char old-point))
    (wavescope-find-phrase-beginning)))

(defun wavescope-discover-phrase (&optional quiet)
  (end-of-line)
  (let ((end (point)) (old-cfs case-fold-search))
    (setq case-fold-search nil)
    (wavescope-modify-syntax)
    (wavescope-find-phrase-beginning)
    (if (> (point) end) (setq end (point)))
    (save-excursion
      (let ((begin (point)) (cpt 0) (lines-left 0) (stop)
	    (inside-block (wavescope-inside-block-p))
	    (looking-block (looking-at wavescope-inside-block-opening-full)))
	(if (and looking-block inside-block)
	    (progn
	      (setq begin (nth 0 inside-block))
	      (setq end (nth 2 inside-block))
	      (goto-char end))
	  (if inside-block
	      (progn
		(setq stop (save-excursion (goto-char (nth 1 inside-block))
					   (beginning-of-line) (point)))
		(if (< stop end) (setq stop (point-max))))
	    (setq stop (point-max)))
	  (save-restriction
	    (goto-char end)
	    (while (and (= lines-left 0)
			(or (not inside-block) (< (point) stop))
			(<= (save-excursion
			      (wavescope-find-phrase-beginning)) end))
	      (if (not quiet)
		  (prog2
		      (setq cpt (1+ cpt))
		      (if (= 8 cpt)
			  (message "Looking for enclosing phrase..."))))
	      (setq end (point))
	      (wavescope-skip-to-end-of-phrase)
	      (beginning-of-line)
	      (narrow-to-region (point) (point-max))
	      (goto-char end)
	      (setq lines-left (forward-line 1)))))
	(if (>= cpt 8) (message "Looking for enclosing phrase... done."))
	(save-excursion (wavescope-skip-blank-and-comments) (setq end (point)))
	(wavescope-skip-back-blank-and-comments)
	(setq case-fold-search old-cfs)
	(wavescope-restore-syntax)
	(list begin (point) end)))))

(defun wavescope-mark-phrase ()
  "Put mark at end of this Caml phrase, point at beginning.
The Caml phrase is the phrase just before the point."
  (interactive)
  (let ((pair (wavescope-discover-phrase)))
    (goto-char (nth 1 pair)) (push-mark (nth 0 pair) t t)))

(defun wavescope-next-phrase (&optional quiet)
  "Skip to the beginning of the next phrase."
  (interactive "i")
  (goto-char (save-excursion (nth 2 (wavescope-discover-phrase quiet))))
  (if (looking-at "\\<end\\>") (wavescope-next-phrase quiet))
  (if (looking-at ";;")
      (progn
	(forward-char 2)
	(wavescope-skip-blank-and-comments))))

(defun wavescope-previous-phrase ()
  "Skip to the beginning of the previous phrase."
  (interactive)
  (beginning-of-line)
  (wavescope-skip-to-end-of-phrase)
  (wavescope-discover-phrase))

(defun wavescope-indent-phrase ()
  "Depending of the context: justify and indent a comment,
or indent all lines in the current phrase."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (if (wavescope-in-comment-p)
	(let* ((cobpoint (save-excursion
			   (wavescope-beginning-of-literal-or-comment)
			   (point)))
	       (begpoint (save-excursion
			   (while (and (> (point) cobpoint)
				       (wavescope-in-comment-p)
				       (not (looking-at "^[ \t]*$")))
			     (forward-line -1))
			   (max cobpoint (point))))
	       (coepoint (save-excursion
			   (while (wavescope-in-comment-p)
			     (re-search-forward "\\*)"))
			   (point)))
	       (endpoint (save-excursion
			   (re-search-forward "^[ \t]*$" coepoint 'end)
			   (beginning-of-line)
			   (forward-line 1)
			   (point)))
	       (leading-star (wavescope-leading-star-p)))
	  (goto-char begpoint)
	  (while (and leading-star
		      (< (point) endpoint)
		      (not (looking-at "^[ \t]*$")))
	    (forward-line 1)
	    (back-to-indentation)
	    (if (looking-at "\\*\\**\\([^)]\\|$\\)")
		(progn
		  (delete-char 1)
		  (setq endpoint (1- endpoint)))))
	  (goto-char (min (point) endpoint))
	  (fill-region begpoint endpoint)
	  (re-search-forward "\\*)")
	  (setq endpoint (point))
	  (if leading-star
	      (progn
		(goto-char begpoint)
		(forward-line 1)
		(if (< (point) endpoint)
		    (wavescope-auto-fill-insert-leading-star t))))
	  (indent-region begpoint endpoint nil))
      (let ((pair (wavescope-discover-phrase)))
	(indent-region (nth 0 pair) (nth 1 pair) nil)))))

(defun wavescope-find-alternate-file ()
  "Switch Implementation/Interface."
  (interactive)
  (let ((name (buffer-file-name)))
    (if (string-match "\\`\\(.*\\)\\.ml\\(i\\)?\\'" name)
	(find-file (concat (wavescope-match-string 1 name)
			   (if (match-beginning 2) ".ml" ".mli"))))))

(defun wavescope-insert-class-form ()
  "Insert a nicely formatted class-end form, leaving a mark after end."
  (interactive "*")
  (let ((prec (preceding-char))) 
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
        (insert " ")))
  (let ((old (point)))
    (insert "class  = object (self)\ninherit  as super\nend;;\n")
    (end-of-line)
    (indent-region old (point) nil)
    (indent-according-to-mode)
    (push-mark)
    (forward-line -2)
    (indent-according-to-mode)))

(defun wavescope-insert-begin-form ()
  "Insert a nicely formatted begin-end form, leaving a mark after end."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "begin\n\nend\n")
    (end-of-line)
    (indent-region old (point) nil)
    (push-mark)
    (forward-line -2)
    (indent-according-to-mode)))

(defun wavescope-insert-for-form ()
  "Insert a nicely formatted for-to-done form, leaving a mark after done."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "for  do\n\ndone\n")
    (end-of-line)
    (indent-region old (point) nil)
    (push-mark)
    (forward-line -2)
    (indent-according-to-mode)
    (beginning-of-line 1)
    (backward-char 4)))

(defun wavescope-insert-while-form ()
  "Insert a nicely formatted for-to-done form, leaving a mark after done."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "while  do\n\ndone\n")
    (end-of-line)
    (indent-region old (point) nil)
    (push-mark)
    (forward-line -2)
    (indent-according-to-mode)
    (beginning-of-line 1)
    (backward-char 4)))

(defun wavescope-insert-if-form ()
  "Insert a nicely formatted if-then-else form, leaving a mark after else."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "if\n\nthen\n\nelse\n")
    (end-of-line)
    (indent-region old (point) nil)
    (indent-according-to-mode)
    (push-mark)
    (forward-line -2)
    (indent-according-to-mode)
    (forward-line -2)
    (indent-according-to-mode)))

(defun wavescope-insert-match-form ()
  "Insert a nicely formatted math-with form, leaving a mark after with."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "match\n\nwith\n")
    (end-of-line)
    (indent-region old (point) nil)
    (indent-according-to-mode)
    (push-mark)
    (forward-line -2)
    (indent-according-to-mode)))

(defun wavescope-insert-let-form ()
  "Insert a nicely formatted let-in form, leaving a mark after in."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "let  in\n")
    (end-of-line)
    (indent-region old (point) nil)
    (indent-according-to-mode)
    (push-mark)
    (beginning-of-line)
    (backward-char 4)
    (indent-according-to-mode)))

(defun wavescope-insert-try-form ()
  "Insert a nicely formatted try-with form, leaving a mark after with."
  (interactive "*")
  (let ((prec (preceding-char)))
    (if (and prec (not (char-equal ?\  (char-syntax prec))))
	(insert " ")))
  (let ((old (point)))
    (insert "try\n\nwith\n")
    (end-of-line)
    (indent-region old (point) nil)
    (indent-according-to-mode)
    (push-mark)
    (forward-line -2)
    (indent-according-to-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Wavescope interactive mode

;; Augment Wavescope mode with a Caml toplevel.

(require 'comint)

(defvar wavescope-interactive-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "|" 'wavescope-electric)
    (define-key map ")" 'wavescope-electric-rp)
    (define-key map "}" 'wavescope-electric-rc)
    (define-key map "]" 'wavescope-electric-rb)
    (define-key map "\C-c\C-i" 'wavescope-interrupt-caml)
    (define-key map "\C-c\C-k" 'wavescope-kill-caml)
    (define-key map "\C-c`" 'wavescope-interactive-next-error-toplevel)
    (define-key map "\C-m" 'wavescope-interactive-send-input)
    (define-key map "\C-j" 'wavescope-interactive-send-input-or-indent)
    (define-key map "\M-\C-m" 'comint-send-input)
    (define-key map [kp-enter] 'comint-send-input)
    map))

(defconst wavescope-interactive-buffer-name "*caml-toplevel*")

(defconst wavescope-interactive-toplevel-error-regexp
  "[ \t]*Characters \\([0-9]+\\)-\\([0-9]+\\):"
  "Regexp matching the char numbers in ocaml toplevel's error messages.")
(defvar wavescope-interactive-last-phrase-pos-in-source 0)
(defvar wavescope-interactive-last-phrase-pos-in-toplevel 0)

(defun wavescope-interactive-filter (text)
  (when (eq major-mode 'wavescope-interactive-mode)
    (save-excursion
      (when (>= comint-last-input-end comint-last-input-start)
	(if (and wavescope-with-xemacs wavescope-interactive-read-only-input)
	    (add-text-properties
	     comint-last-input-start comint-last-input-end
	     (list 'read-only t)))
	(if (and font-lock-mode wavescope-interactive-input-font-lock)
	    (progn
	      (font-lock-fontify-region comint-last-input-start
					comint-last-input-end)
	      (if (featurep 'sym-lock)
		  (sym-lock-make-symbols-atomic comint-last-input-start
						comint-last-input-end))))
	(if wavescope-interactive-output-font-lock
	    (save-excursion
	      (goto-char (point-max))
	      (re-search-backward comint-prompt-regexp
				  comint-last-input-end t)
	      (add-text-properties
	       comint-last-input-end (point)
	       '(face wavescope-font-lock-interactive-output-face))))
	(if wavescope-interactive-error-font-lock
	    (save-excursion
	      (goto-char comint-last-input-end)
	      (while (re-search-forward wavescope-interactive-error-regexp () t)
		(let ((matchbeg (match-beginning 1))
		      (matchend (match-end 1)))
		  (save-excursion
		    (add-text-properties
		     matchbeg matchend
		     '(face wavescope-font-lock-interactive-error-face))
		    (goto-char matchbeg)
		    (if (looking-at wavescope-interactive-toplevel-error-regexp)
			(let ((beg (string-to-int (wavescope-match-string 1)))
			      (end (string-to-int (wavescope-match-string 2))))
			  (add-text-properties
			   (+ comint-last-input-start beg)
			   (+ comint-last-input-start end)
			   '(face wavescope-font-lock-error-face))
			  )))))))))))

(define-derived-mode wavescope-interactive-mode comint-mode "Wavescope-Interactive"
  "Major mode for interacting with a Caml process.
Runs a Caml toplevel as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in Caml mode.

Special keys for Wavescope interactive mode:\\{wavescope-interactive-mode-map}"
  (wavescope-install-font-lock t)
  (if (or wavescope-interactive-input-font-lock
	  wavescope-interactive-output-font-lock
	  wavescope-interactive-error-font-lock)
      (font-lock-mode 1))
  (add-hook 'comint-output-filter-functions 'wavescope-interactive-filter)
  (if (not (boundp 'after-change-functions)) ()
    (make-local-hook 'after-change-functions)
    (remove-hook 'after-change-functions 'font-lock-after-change-function t))
  (if (not (boundp 'pre-idle-hook)) ()
    (make-local-hook 'pre-idle-hook)
    (remove-hook 'pre-idle-hook 'font-lock-pre-idle-hook t))
  (setq comint-prompt-regexp "^#  *")
  (setq comint-process-echoes nil)
  (setq comint-get-old-input 'wavescope-interactive-get-old-input)
  (setq comint-scroll-to-bottom-on-output t)
  (set-syntax-table wavescope-mode-syntax-table)
  (setq local-abbrev-table wavescope-mode-abbrev-table)

  (setq indent-line-function 'wavescope-indent-command)

  (easy-menu-add wavescope-interactive-mode-menu)
  (wavescope-update-options-menu))

(defun wavescope-run-caml ()
  "Run a Caml toplevel process. I/O via buffer `*caml-toplevel*'."
  (interactive)
  (wavescope-run-process-if-needed)
  (when wavescope-display-buffer-on-eval
    (display-buffer wavescope-interactive-buffer-name)))

(defun wavescope-run-process-if-needed (&optional cmd)
  "Run a Caml toplevel process if needed, with an optional command name.
I/O via buffer `*caml-toplevel*'."
  (if cmd
      (setq wavescope-interactive-program cmd)
    (if (not (comint-check-proc wavescope-interactive-buffer-name))
	(setq wavescope-interactive-program
	      (read-shell-command "Caml toplevel to run: "
				  wavescope-interactive-program))))
  (if (not (comint-check-proc wavescope-interactive-buffer-name))
      (let ((cmdlist (wavescope-args-to-list wavescope-interactive-program))
            (process-connection-type nil))
	(set-buffer (apply (function make-comint) "caml-toplevel"
			   (car cmdlist) nil (cdr cmdlist)))
	(wavescope-interactive-mode)
	(sleep-for 1))))

(defun wavescope-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (wavescope-args-to-list (substring string (+ 1 where)
						 (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (wavescope-args-to-list (substring string pos
						 (length string)))))))))

(defun wavescope-interactive-get-old-input ()
  (save-excursion
    (let ((end (point)))
      (re-search-backward comint-prompt-regexp (point-min) t)
      (if (looking-at comint-prompt-regexp)
	  (re-search-forward comint-prompt-regexp))
      (buffer-substring-no-properties (point) end))))

(defun wavescope-interactive-end-of-phrase ()
  (save-excursion
    (end-of-line)
    (wavescope-find-meaningful-word)
    (wavescope-find-meaningful-word)
    (looking-at ";;")))

(defconst wavescope-interactive-send-warning
  "Note: toplevel processing requires a terminating `;;'")
(defun wavescope-interactive-send-input ()
  "Process if the current line ends with `;;' then send the
current phrase else insert a newline."
  (interactive)
  (if (wavescope-interactive-end-of-phrase)
      (progn
	(comint-send-input)
	(goto-char (point-max)))
    (insert "\n")
    (message wavescope-interactive-send-warning)))

(defun wavescope-interactive-send-input-or-indent ()
  "Process if the current line ends with `;;' then send the
current phrase else insert a newline and indent."
  (interactive)
  (if (wavescope-interactive-end-of-phrase)
      (progn
	(comint-send-input)
	(goto-char (point-max)))
    (insert "\n")
    (wavescope-indent-command)
    (message wavescope-interactive-send-warning)))

(defun wavescope-eval-region (start end)
  "Eval the current region in the Caml toplevel."
  (interactive "r")
  (save-excursion (wavescope-run-process-if-needed))
  (comint-preinput-scroll-to-bottom)
  (setq wavescope-interactive-last-phrase-pos-in-source start)
  (save-excursion
    (goto-char start)
    (wavescope-skip-blank-and-comments)
    (setq start (point))
    (goto-char end)
    (wavescope-skip-to-end-of-phrase)
    (setq end (point))
    (let ((text (buffer-substring-no-properties start end)))
      (goto-char end)
      (if (string= text "")
	  (message "Cannot send empty commands to Caml toplevel!")
	(set-buffer wavescope-interactive-buffer-name)
	(goto-char (point-max))
	(setq wavescope-interactive-last-phrase-pos-in-toplevel (point))
	(if wavescope-interactive-echo-phrase
	    (progn
	      (insert (concat text ";;"))
	      (comint-send-input))
	  (comint-send-string wavescope-interactive-buffer-name
			      (concat text ";;"))
	  (comint-send-input))))
    (when wavescope-display-buffer-on-eval
      (display-buffer wavescope-interactive-buffer-name))))

(defun wavescope-narrow-to-phrase ()
  "Narrow the editting window to the surrounding Caml phrase (or block)."
  (interactive)
  (save-excursion
    (let ((pair (wavescope-discover-phrase)))
      (narrow-to-region (nth 0 pair) (nth 1 pair)))))

(defun wavescope-eval-phrase ()
  "Eval the surrounding Caml phrase (or block) in the Caml toplevel."
  (interactive)
  (let ((end))
    (save-excursion
      (let ((pair (wavescope-discover-phrase)))
	(setq end (nth 2 pair))
	(wavescope-eval-region (nth 0 pair) (nth 1 pair))))
    (if wavescope-skip-after-eval-phrase
	(goto-char end))))

(defun wavescope-eval-buffer ()
  "Send the buffer to the Wavescope Interactive process."
  (interactive)
  (wavescope-eval-region (point-min) (point-max)))

(defun wavescope-interactive-next-error-source ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (save-excursion
      (set-buffer wavescope-interactive-buffer-name)
      (goto-char wavescope-interactive-last-phrase-pos-in-toplevel)
      (setq error-pos
	    (re-search-forward wavescope-interactive-toplevel-error-regexp
			       (point-max) t))
      (if error-pos
	  (progn
	    (setq beg (string-to-int (wavescope-match-string 1))
		  end (string-to-int (wavescope-match-string 2))))))
    (if (not error-pos)
	(message "No syntax or typing error in last phrase.")
      (setq beg (+ wavescope-interactive-last-phrase-pos-in-source beg)
	    end (+ wavescope-interactive-last-phrase-pos-in-source end))
      (add-text-properties beg end '(face wavescope-font-lock-error-face))
      (goto-char beg))))

(defun wavescope-interactive-next-error-toplevel ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (save-excursion
      (goto-char wavescope-interactive-last-phrase-pos-in-toplevel)
      (setq error-pos
	    (re-search-forward wavescope-interactive-toplevel-error-regexp
			       (point-max) t))
      (if error-pos
	  (setq beg (string-to-int (wavescope-match-string 1))
		end (string-to-int (wavescope-match-string 2)))))
    (if (not error-pos)
	(message "No syntax or typing error in last phrase.")
      (setq beg (+ wavescope-interactive-last-phrase-pos-in-toplevel beg)
	    end (+ wavescope-interactive-last-phrase-pos-in-toplevel end))
      (add-text-properties beg end '(face wavescope-font-lock-error-face))
      (goto-char beg))))

(defun wavescope-interrupt-caml ()
  (interactive)
  (if (comint-check-proc wavescope-interactive-buffer-name)
      (save-excursion
	(set-buffer wavescope-interactive-buffer-name)
	(comint-interrupt-subjob))))

(defun wavescope-kill-caml ()
  (interactive)
  (if (comint-check-proc wavescope-interactive-buffer-name)
      (save-excursion
	(set-buffer wavescope-interactive-buffer-name)
	(comint-kill-subjob))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Menu support

(defun wavescope-about () (interactive)
  (describe-variable 'wavescope-mode-version))
(defun wavescope-help () (interactive)
  (describe-function 'wavescope-mode))
(defun wavescope-interactive-help () (interactive)
  (describe-function 'wavescope-interactive-mode))

(defvar wavescope-definitions-menu-last-buffer nil)
(defvar wavescope-definitions-keymaps nil)

(defun wavescope-build-menu ()
  (easy-menu-define
   wavescope-mode-menu (list wavescope-mode-map)
   "Wavescope Mode Menu."
   '("Wavescope"
     ("Interactive Mode"
      ["Run Caml Toplevel" wavescope-run-caml t]
      ["Interrupt Caml Toplevel" wavescope-interrupt-caml
       :active (comint-check-proc wavescope-interactive-buffer-name)]
      ["Kill Caml Toplevel" wavescope-kill-caml
       :active (comint-check-proc wavescope-interactive-buffer-name)]
      ["Evaluate Region" wavescope-eval-region
       ;; Region-active-p for XEmacs and mark-active for Emacs
       :active (if (fboundp 'region-active-p) (region-active-p) mark-active)]
      ["Evaluate Phrase" wavescope-eval-phrase t]
      ["Evaluate Buffer" wavescope-eval-buffer t])
     ("Caml Forms"
      ["try .. with .." wavescope-insert-try-form t]
      ["match .. with .." wavescope-insert-match-form t]
      ["let .. in .." wavescope-insert-let-form t]
      ["if .. then .. else .." wavescope-insert-if-form t]
      ["while .. do .. done" wavescope-insert-while-form t]
      ["for .. do .. done" wavescope-insert-for-form t]
      ["begin .. end" wavescope-insert-begin-form t])
     ["Switch .ml/.mli" wavescope-find-alternate-file t]
     "---"
     ["Compile..." compile t]
     ["Reference Manual..." wavescope-browse-manual t]
     ["Caml Library..." wavescope-browse-library t]
     ("Definitions"
      ["Scan..." wavescope-list-definitions t])
     "---"
     [ "Show type at point" caml-types-show-type
       wavescope-with-caml-mode-p]
     "---"
     [ "Complete identifier" caml-complete
       wavescope-with-caml-mode-p]
     [ "Help for identifier" caml-help
       wavescope-with-caml-mode-p]
     [ "Add path for documentation" ocaml-add-path
       wavescope-with-caml-mode-p]
     [ "Open module for documentation" ocaml-open-module
       wavescope-with-caml-mode-p]
     [ "Close module for documentation" ocaml-close-module
       wavescope-with-caml-mode-p]
     "---"
     ["Customize Wavescope Mode..." (customize-group 'wavescope) t]
     ("Wavescope Options" ["Dummy" nil t])
     ("Wavescope Interactive Options" ["Dummy" nil t])
     "---"
     ["About" wavescope-about t]
     ["Help" wavescope-help t]))
  (easy-menu-add wavescope-mode-menu)
  (wavescope-update-options-menu)
  ;; Save and update definitions menu
  (if wavescope-with-xemacs
      (add-hook 'activate-menubar-hook 'wavescope-update-definitions-menu)
    (if (not (functionp 'easy-menu-create-keymaps)) ()
      ;; Patch for Emacs
      (add-hook 'menu-bar-update-hook
		'wavescope-with-emacs-update-definitions-menu)
      (make-local-variable 'wavescope-definitions-keymaps)
      (setq wavescope-definitions-keymaps
	    (cdr (easy-menu-create-keymaps
		  "Definitions" wavescope-definitions-menu)))
      (setq wavescope-definitions-menu-last-buffer nil))))

(easy-menu-define
  wavescope-interactive-mode-menu wavescope-interactive-mode-map
  "Wavescope Interactive Mode Menu."
  '("Wavescope"
    ("Interactive Mode"
     ["Run Caml Toplevel" wavescope-run-caml t]
     ["Interrupt Caml Toplevel" wavescope-interrupt-caml
      :active (comint-check-proc wavescope-interactive-buffer-name)]
     ["Kill Caml Toplevel" wavescope-kill-caml
      :active (comint-check-proc wavescope-interactive-buffer-name)]
     ["Evaluate Region" wavescope-eval-region :active (region-active-p)]
     ["Evaluate Phrase" wavescope-eval-phrase t]
     ["Evaluate Buffer" wavescope-eval-buffer t])
    "---"
    ["Customize Wavescope Mode..." (customize-group 'wavescope) t]
    ("Wavescope Options" ["Dummy" nil t])
    ("Wavescope Interactive Options" ["Dummy" nil t])
    "---"
    ["About" wavescope-about t]
    ["Help" wavescope-interactive-help t]))

(defun wavescope-update-definitions-menu ()
  (if (eq major-mode 'wavescope-mode)
      (easy-menu-change
       '("Wavescope") "Definitions"
       wavescope-definitions-menu)))

(defun wavescope-with-emacs-update-definitions-menu ()
  (if (current-local-map)
      (let ((keymap
	     (lookup-key (current-local-map) [menu-bar Wavescope Definitions])))
	(if (and
	     (keymapp keymap)
	     (not (eq wavescope-definitions-menu-last-buffer (current-buffer))))
	    (setcdr keymap wavescope-definitions-keymaps)
	  (setq wavescope-definitions-menu-last-buffer (current-buffer))))))

(defun wavescope-toggle-option (symbol)
  (interactive)
  (set symbol (not (symbol-value symbol)))
  (if (eq 'wavescope-use-abbrev-mode symbol)
      (abbrev-mode wavescope-use-abbrev-mode)) ; toggle abbrev minor mode
  (if wavescope-with-xemacs nil (wavescope-update-options-menu)))

(defun wavescope-update-options-menu ()
  (easy-menu-change
   '("Wavescope") "Wavescope Options"
   (mapcar (lambda (pair)
	     (if (consp pair)
		 (vector (car pair)
			 (list 'wavescope-toggle-option (cdr pair))
			 ':style 'toggle
			 ':selected (nth 1 (cdr pair))
			 ':active t)
	       pair)) wavescope-options-list))
  (easy-menu-change
   '("Wavescope") "Wavescope Interactive Options"
   (mapcar (lambda (pair)
	     (if (consp pair)
		 (vector (car pair)
			 (list 'wavescope-toggle-option (cdr pair))
			 ':style 'toggle
			 ':selected (nth 1 (cdr pair))
			 ':active t)
	       pair)) wavescope-interactive-options-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Browse Manual

;; From M. Quercia

(defun wavescope-browse-manual ()
  "*Browse Caml reference manual."
  (interactive)
  (setq wavescope-manual-url (read-from-minibuffer "URL: " wavescope-manual-url))
  (funcall wavescope-browser wavescope-manual-url))

(defun wavescope-xemacs-w3-manual (url)
  "*Browse Caml reference manual."
  (w3-fetch-other-frame url))

(defun wavescope-netscape-manual (url)
  "*Browse Caml reference manual."
  (start-process-shell-command
   "netscape" nil
   (concat "netscape -remote 'openURL ("
	   url ", newwindow)' || netscape " url)))

(defun wavescope-mmm-manual (url)
  "*Browse Caml reference manual."
  (start-process-shell-command
   "mmm" nil
   (concat "mmm_remote " url " || mmm -external " url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Browse Library

;; From M. Quercia

(defun wavescope-browse-library()
  "Browse the Caml library."
  (interactive)
  (let ((buf-name "*caml-library*") (opoint)
	(dir (read-from-minibuffer "Library path: " wavescope-library-path)))
    (if (and (file-directory-p dir) (file-readable-p dir))
	(progn
	  (setq wavescope-library-path dir)
	  ;; List *.ml and *.mli files
	  (with-output-to-temp-buffer buf-name
	    (buffer-disable-undo standard-output)
	    (save-excursion
	      (set-buffer buf-name)
	      (kill-all-local-variables)
	      (make-local-variable 'wavescope-library-path)
	      (setq wavescope-library-path dir)
	      ;; Help
	      (insert "Directory \"" dir "\".\n") 
	      (insert "Select a file with middle mouse button or RETURN.\n\n")
	      (insert "Interface files (.mli):\n\n")
	      (insert-directory (concat dir "/*.mli") "-C" t nil)
	      (insert "\n\nImplementation files (.ml):\n\n")
	      (insert-directory (concat dir "/*.ml") "-C" t nil)
	      ;; '.', '-' and '_' are now letters
	      (modify-syntax-entry ?. "w")
	      (modify-syntax-entry ?_ "w")
	      (modify-syntax-entry ?- "w")
	      ;; Every file name is now mouse-sensitive
	      (goto-char (point-min))
	      (while (< (point) (point-max))
		(re-search-forward "\\.ml.?\\>")
		(setq opoint (point))
		(re-search-backward "\\<" (point-min) 1)
		(put-text-property (point) opoint 'mouse-face 'highlight)
		(goto-char (+ 1 opoint)))
	      ;; Activate wavescope-library mode
	      (setq major-mode 'wavescope-library-mode)
	      (setq mode-name "wavescope-library")
	      (use-local-map wavescope-library-mode-map)
	      (setq buffer-read-only t)))))))
  
(defvar wavescope-library-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [return] 'wavescope-library-find-file)
    (define-key map [mouse-2] 'wavescope-library-mouse-find-file)
    map))

(defun wavescope-library-find-file ()
  "Load the file whose name is near point."
  (interactive)
  (save-excursion
    (if (text-properties-at (point))
	(let (beg)
	  (re-search-backward "\\<") (setq beg (point))
	  (re-search-forward "\\>")
	  (find-file-read-only (concat wavescope-library-path "/"
				       (buffer-substring-no-properties
					beg (point))))))))

(defun wavescope-library-mouse-find-file (event)
  "Visit the file name you click on."
  (interactive "e")
  (let ((owindow (selected-window)))
    (mouse-set-point event)
    (wavescope-library-find-file)
    (select-window owindow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Definitions List

;; Designed from original code by M. Quercia

(defconst wavescope-definitions-regexp
  "\\<\\(and\\|val\\|type\\|module\\|class\\|exception\\|let\\)\\>"
  "Regexp matching definition phrases.")

(defconst wavescope-definitions-bind-skip-regexp
  (concat "\\<\\(rec\\|type\\|virtual\\)\\>\\|'[" wavescope-alpha "][0-9_'"
	  wavescope-alpha "]*\\|('.*)")
  "Regexp matching stuff to ignore after a binding keyword.")

(defvar wavescope-definitions-menu (list ["Scan..." wavescope-list-definitions t])
  "Initial content of the definitions menu.")
(make-variable-buffer-local 'wavescope-definitions-menu)

(defun wavescope-list-definitions ()
  "Parse the buffer and gather toplevel definitions for quick
jump via the definitions menu."
  (interactive)
  (message "Searching definitions...")
  (save-excursion
    (let ((cpt 0) (kw) (menu) (scan-error)
	  (value-list) (type-list) (module-list) (class-list) (misc-list))
      (goto-char (point-min))
      (wavescope-skip-blank-and-comments)
      (while (and (< (point) (point-max)) (not scan-error))
	(if (looking-at wavescope-definitions-regexp)
	    (progn
	      (setq kw (wavescope-match-string 0))
	      (if (string= kw "and")
		  (setq kw (save-match-data
			     (save-excursion (wavescope-find-and-match)))))
	      (if (or (string= kw "exception")
		      (string= kw "val")) (setq kw "let"))
	      ;; Skip optional elements
	      (goto-char (match-end 0))
	      (wavescope-skip-blank-and-comments)
	      (if (looking-at wavescope-definitions-bind-skip-regexp)
		  (goto-char (match-end 0)))
	      (wavescope-skip-blank-and-comments)
	      (if (looking-at
		   (concat "\\<[" wavescope-alpha "][0-9_'" wavescope-alpha "]*\\>"))
		  ;; Menu item : [name (goto-char ...) t]
		  (let* ((p (make-marker))
			 (ref (vector (wavescope-match-string 0)
				      (list 'wavescope-goto p) t)))
		    (setq cpt (1+ cpt))
		    (message (concat "Searching definitions... ("
				     (number-to-string cpt) ")"))
		    (set-marker p (point))
		    (cond
		     ((string= kw "let")
		      (setq value-list (cons ref value-list)))
		     ((string= kw "type")
		      (setq type-list (cons ref type-list)))
		     ((string= kw "module")
		      (setq module-list (cons ref module-list)))
		     ((string= kw "class")
		      (setq class-list (cons ref class-list)))
		     (t (setq misc-list (cons ref misc-list))))))))
	;; Skip to next phrase or next top-level `and'
	(wavescope-forward-char)
	(let ((old-point (point)) (last-and))
	  (wavescope-next-phrase t)
	  (setq last-and (point))
	  (if (< last-and old-point)
	      (setq scan-error t)
	    (save-excursion
	      (while (and (re-search-backward "\\<and\\>" old-point t)
			  (not (wavescope-in-literal-or-comment-p))
			  (save-excursion (wavescope-find-and-match)
					  (>= old-point (point))))
		(setq last-and (point)))))
	  (goto-char last-and)))
      (if scan-error
	  (message "Parse error when scanning definitions: line %s!"
		   (line-number))
	;; Sort and build lists
	(mapcar (lambda (pair)
		  (if (cdr pair)
		      (setq menu
			    (append (wavescope-split-long-list
				     (car pair) (wavescope-sort-definitions (cdr pair)))
				    menu))))
		(list (cons "Miscellaneous" misc-list)
		      (cons "Values" value-list)
		      (cons "Classes" class-list)
		      (cons "Types" type-list)
		      (cons "Modules" module-list)))
	;; Update definitions menu
	(setq wavescope-definitions-menu
	      (append menu (list "---"
				 ["Rescan..." wavescope-list-definitions t])))
	(if (or wavescope-with-xemacs
		(not (functionp 'easy-menu-create-keymaps))) ()
	  ;; Patch for Emacs 20.2
	  (setq wavescope-definitions-keymaps
		(cdr (easy-menu-create-keymaps 
		      "Definitions" wavescope-definitions-menu)))
	  (setq wavescope-definitions-menu-last-buffer nil))
	(message "Searching definitions... done"))))
  (wavescope-update-definitions-menu))

(defun wavescope-goto (pos)
  (goto-char pos)
  (recenter))

(defun wavescope-sort-definitions (list)
  (let* ((last "") (cpt 1)
	 (list (sort (nreverse list)
		     (lambda (p q) (string< (elt p 0) (elt q 0)))))
	 (tail list))
    (while tail
      (if (string= (elt (car tail) 0) last)
	  (prog2
	      (setq cpt (1+ cpt))
	      (aset (car tail) 0 (format "%s (%d)" last cpt)))
	(setq cpt 1)
	(setq last (elt (car tail) 0)))
      (setq tail (cdr tail)))
    list))

;; Look for the (n-1)th or last element of a list
(defun wavescope-nth (n list)
  (if (or (<= n 1) (null list) (null (cdr list))) list
    (wavescope-nth (1- n) (cdr list))))
    
;; Split a definition list if it is too long
(defun wavescope-split-long-list (title list)
  (let ((tail (wavescope-nth wavescope-definitions-max-items list)))
    (if (or (null tail) (null (cdr tail)))
        ;; List not too long, cons the title
        (list (cons title list))
      ;; List too long, split and add initials to the title
      (let (lists)
        (while list
          (let ((beg (substring (elt (car list) 0) 0 1))
                (end (substring (elt (car tail) 0) 0 1)))
            (setq lists (cons
                         (cons (format "%s %s-%s" title beg end) list)
                         lists))
            (setq list (cdr tail))
            (setcdr tail nil)
            (setq tail (wavescope-nth wavescope-definitions-max-items list))))
        (nreverse lists)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Hooks and Exit

(condition-case nil
    (progn (require 'speedbar)
	   (speedbar-add-supported-extension
	    '(".ml" ".mli" ".mll" ".mly")))
  (error nil))

(defvar wavescope-load-hook nil
  "This hook is run when Wavescope is loaded in. It is a good place to put
key-bindings or hack Font-Lock keywords...")

(run-hooks 'wavescope-load-hook)

(provide 'wavescope)
;; For compatibility with caml support modes
;; you may also link caml.el to wavescope.el
(provide 'caml)

;;; wavescope.el ends here
