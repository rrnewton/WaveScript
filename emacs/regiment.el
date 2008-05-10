
;; Ryan Newton [2005]
;; Some convenient Emacs functions for use with Regiment.

 (defun cap-dir (dir)
    "Make sure a directory ends in a slash."
   (if (= 47 (car (reverse (string-to-list dir))))
        dir
      (concat dir "/")))
;;;; Add the following to your .emacs to use this:
;; (let ((reglib (concat (cap-dir (getenv "REGIMENTD")) "emacs/regiment.el")))
;;   (if (file-readable-p reglib)
;;       (load-library reglib)))

; ======================================================================

;; Put us in the path so that we can load the modified scheme.el:
(setq load-path (cons (concat (cap-dir (getenv "REGIMENTD")) "emacs/") load-path))

(setq auto-mode-alist 
      (append '(("\\.rs$" . scheme-mode)
		("\\.tm$" . scheme-mode)
		("\\.ss$" . scheme-mode)
		("\\.sexp$" . scheme-mode))
	      auto-mode-alist))

(global-unset-key [(control shift c) (control shift s)])
(global-set-key [(control shift c) (control shift s)] 'run-scheme)
(global-set-key [(control c) (b)] 'switch-dir-and-load-regiment)

;; Light up FIXMEs
(font-lock-add-keywords
 'scheme-mode
 '(("\\<\\(FIXME\\)" 1 font-lock-warning-face t)))
(modify-face (quote font-lock-warning-face) "Red" "yellow" nil t nil t nil nil)

(defun cap-dir (dir)
   "Make sure a directory ends in a slash."
   (if (= 47 (car (reverse (string-to-list dir))))
       dir
     (concat dir "/")))


(defun load-all-files-of-interest-from (dir)
  ;; Sometimes it doesn't work if I don't do this first:
  (progn
    (cd dir)
    (mapcar (lambda (f)
	      (let ((filename (file-name-nondirectory f))
		    (ext      (file-name-extension f)))
		(let ((len (length filename)))
		  (if (> len 3)
		      (if (and (not (string= (substring filename 0 1) "_"))
			       (not (string= (substring filename 0 3) ".__"))
			       (not (string= (substring filename 0 2) ".#"))
			       (not (string= (substring filename (- len 1) len) "~"))
			       )
			  (if (or (string= ext "ss")
				  (string= ext "sls")
				  (string= ext "rs") ;; Regiment source (sexp)
				  (string= ext "tm")
				  (string= ext "ws") ;; Wavescript source
				  (string= ext "cpp")
				  (string= ext "h")
				  (string= ext "tests"))
			      (progn
				(print (concat "Loading " f))
				(find-file-noselect f))
			    ))))))
	    (all-contained-files dir))))

;; rrn: this is convenient for me because I use the moccur package and the mtorus package.
(defun load-regiment-all-source ()
  "Load all the source code for regiment."
  (interactive)
  (let ((regd (cap-dir (getenv "REGIMENTD"))))
    (print "LOAD\n")
    (load-all-files-of-interest-from (concat regd "src/ws"))
    (find-file-noselect (concat regd "/src/main.sls"))
    (find-file-noselect (concat regd "/src/main_r6rs_import_list.sexp"))
    (find-file-noselect (concat regd "/src/common_import_list.sexp"))
    (find-file-noselect (concat regd "/src/regiment.ss"))
    (find-file-noselect (concat regd "/src/config.ss"))

;    (load-all-files-of-interest-from (concat regd "lib"))
;    (load-all-files-of-interest-from (concat regd "apps"))
					;'("~/cur/generic/"))
					;(setq find-file-wildcards t)
					;(find-file "~/cur/*.ss")
    ))

      
;;       (mapcar (lambda (dir)
;; 	)
;; 	  '("generic/" "generic/passes" "generic/passes/normalize_source/" 
;; 	    "generic/passes/normalize_query" "generic/passes/analyze_query/" 
;; 	    "generic/passes/deglobalize/" "generic/passes/tokmac_bkend/" 
;; 	    "generic/passes/wavescope_bkend" "generic/passes/nesc_bkend/" 
	    
;; 	    "" "chez/" "plt/" "C/" 
;; 	    "demos/regiment/" "demos/token_machs/" "demos/firelightning/" "demos/wavescope/" 
;; 	    "linked_lib/"))

(defun all-contained-files (dir)
  "return a list of all subdirectories under a given path"
  (interactive)
    (apply #'append 
	   (mapcar (lambda (fatt)
		     ;(insert "Foo: ")(insert (car fatt))(insert "\n")
		     (if (or (equal "." (nth 0 fatt)) 
			     (equal ".." (nth 0 fatt))
			     (equal ".svn" (nth 0 fatt)))
			 '()
		       (if (equal t (nth 1 fatt))
			   (all-contained-files (concat dir "/" (nth 0 fatt)))
			 (list ;(nth 0 fatt)

			       (concat dir "/" (nth 0 fatt))
			       ))))
		   (directory-files-and-attributes dir))))

;(all-contained-files "~/wavescript/src/ws/passes")


;;  0. t for directory, string (name linked to) for symbolic link, or nil.
;;  1. Number of links to file.
;;  2. File uid.
;;  3. File gid.
;;  4. Last access time, as a list of two integers.
;;   First integer has high-order 16 bits of time, second has low 16 bits.
;;  5. Last modification time, likewise.
;;  6. Last status change time, likewise.
;;  7. Size in bytes.
;;   This is a floating point number if the size is too large for an integer.
;;  8. File modes, as a string of ten letters or dashes as in ls -l.
;;  9. t iff file's gid would change if file were deleted and recreated.
;; 10. inode number.  If inode number is larger than the Emacs integer,
;;   this is a cons cell containing two integers: first the high part,
;;   then the low 16 bits.
;; 11. Device number.  If it is larger than the Emacs integer, this is
;;   a cons cell, similar to the inode number.


(defvar wsd (concat (cap-dir "~/WaveScope") "code/v1/"))

;; [2006.08.24]
(defun load-wavescope-all-source ()
  "Load all the source code for the WaveScope engine."
  (interactive)
  ;; Hardcoded location for now:
  (let ((wsd (concat (cap-dir "~/WaveScope") "code/v1/")))
    ;; Sometimes it doesn't work if I don't do this first:
    (cd wsd)
    (mapcar (lambda (dir)
	    (mapcar (lambda (f)
		      (if (> (length f) 3)
			  (if (and (not (string= (substring f 0 1) "_")) ;; Ignore files starting with "_"
				   (not (string= (substring f 0 2) ".#"))
				   )
			      (if (or (string= (substring f -4) ".hpp")
				      (string= (substring f -4) ".cpp")
				      (string= f "Makefile")		      
				      )
				  (progn 
				    (print (concat "Loading " f))
				    (find-file-noselect (concat wsd dir f))
				)))))
		    (directory-files (concat wsd dir))))
	  '("" "include/" "Library/" "Sources/" "Storage/" "Misc/" 
	    "Timebases/" "Engine/" "FFI/" 
	    ))
	  ;'("~/cur/generic/"))
  ;(setq find-file-wildcards t)
  ;(find-file "~/cur/*.ss")
  ))




(defun revert-scheme-source ()
  "Revert all buffers ending in .ss"
  (interactive)
  (mapcar (lambda (b) (if (string= (substring (buffer-name b) -3) ".ss")
			  (progn
			    (princ (format "Reverting buffer: %s \n" (buffer-name b)))
			    (save-excursion
			      (set-buffer b)
			     ;(switch-to-buffer b)
			      (revert-buffer t t t)
			      )
			    )))
	  (buffer-list)))


(defun switch-dir-and-load-regiment ()
  "Insert text to switch to Regiment dir and load compiler."
  (interactive)
  (insert "(begin (cd \"")
  (insert (getenv "REGIMENTD"))
  (insert "src/\") (load \"compiler_chez.ss\"))"))
;(fset 'switch-dir-and-load-regiment
;   [?(?b ?e ?g ?i ?n ?  ?( ?c ?d ?  ?\" ?~ ?/ ?c ?u ?r ?\" ?) ?  
;	 ?( ?l ?o ?a ?d ?  ?\" ?c ?o ?m ?p ?i ?l ?e ?r ?_ ?c ?h ?e ?z ?. ?s ?s ?\" ?) ?) return])


;; This isn't working for some reason:
(defun kill-scheme-source ()
  "Kill all buffers ending in .ss"
  (interactive)
  (mapcar (lambda (b) (if (string= (substring (buffer-name b) -3) ".ss")
			  (kill-buffer b)))
	  (buffer-list)))

; ======================================================================

;; These are some hacks to the c-mode to make WaveScript code look nicer.

(defun ryan-fontify-wavescript-keywords ()
  "Color WaveScript keywords."
  (interactive)  
  (save-excursion
    ;(beginning-of-line)
    (goto-char (point-min))

    (rrn-applyface-regexp "let " 'font-lock-comment-face)
    (rrn-applyface-regexp "iterate" 'font-lock-comment-face)
    (rrn-applyface-regexp "deep_iterate" 'font-lock-comment-face)
    )
  )


(defun rrn-color-regexp (rexp thecolor)
  "colors matching regexps"
  (rrn-process-regexp rexp #'(lambda () (facemenu-set-foreground thecolor))))

(defun rrn-applyface-regexp (rexp inputface)
  "colors matching regexps"
  (rrn-process-regexp 
   rexp #'(lambda () 
	   (progn 
	     (put-text-property (mark) (point) 'face inputface))
	   )))

(defun rrn-process-regexp (rexp thunk)
  "selects a regexp, executes thunk"
  ;(interactive)
  (let ((res (search-forward-regexp rexp nil t)))
    (if (eq res nil)
	'done
      (let ()
	(set-mark (match-beginning 0))
	(funcall thunk)
	(rrn-process-regexp rexp thunk)))))

; (goto-char (point-min)) (rrn-color-regexp "\\\".*\\\"" "salmon3")
;(add-hook 'c-mode-hook 'turn-on-font-lock)

;(font-lock-add-keywords 'c-mode '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))
(font-lock-add-keywords 'c-mode '(("\\<\\(iterate\\)" 1 font-lock-keyword-face t)))
;(font-lock-add-keywords 'c-mode '(("\\<\\(in\\)" 1 font-lock-keyword-face t)))

;(font-lock-add-keywords 'c-mode '(("\\<\\(union\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode '(("\\<\\(uniontype\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode '(("\\<\\(match\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode '(("\\<\\(state\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode '(("\\<\\(let\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode '(("\\<\\(then\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode '(("\\<\\(emit\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode '(("\\<\\(fun\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode '(("\\<\\(foreign\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode '(("\\<\\(foreign_box\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode '(("\\<\\(foreign_source\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode '(("\\<\\(pointerargs\\)" 1 font-lock-keyword-face t)))

(font-lock-add-keywords 'c-mode '(("\\<\\(smap\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode '(("\\<\\(stream_map\\)" 1 font-lock-keyword-face t)))
(font-lock-add-keywords 'c-mode '(("\\<\\(stream_filter\\)" 1 font-lock-keyword-face t)))

;(font-lock-add-keywords 'c-mode '(("\\<\\( = \\)" 1 font-lock-keyword-face t)))


