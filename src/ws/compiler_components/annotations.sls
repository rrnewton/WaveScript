#!r6rs

;;;; Tools for manipulating annotations

;;;; .author Michael Craig

(library (ws compiler_components annotations) 
  (export annotq
	  annot-keys
	  annotation-list?
	  merge-annotations)
  (import (except (rnrs (6)) error) (ws common))

;;
;; An individual annotation is defined as an association -- an element in a Scheme association
;; list -- whose key (car) is a symbol. An annotation list is an association list tagged at
;; the front with the symbol: annotations. The value (cdr) of an association is expected to
;; never be #f.
;;


;;
;; 
;;
(define annotq
  (case-lambda
    [(annot-name annot-list) (annotq annot-name annot-list #f)]
    [(annot-name annot-list default)
     ;(ASSERT (and (pair? annot-list) (eq? 'annotations (car annot-list))))
     (let ((annot-pair (assoc annot-name annot-list)))
       (if annot-pair
           (cdr annot-pair)
           default))]))


;;
;;
;;
(define (annot-keys annot-list)
  ;(ASSERT (pair? annot-list))
  ;(ASSERT (eq? (car annot-list) 'annotations))
  (DEBUGASSERT annotation-list? `(annotations . ,annot-list))
  ;(map car (cdr annot-list))
  (map car annot-list)
  )

(define (annotation-list? ls)
  (match ls
    [(annotations (,left* . ,right*) ...)
     (andmap symbol? left*)]
    [,_ #f]))

;;
;; two annotation lists al-left and al-right are merged as follows:
;; keys not in both lists are included in the merge list;
;; keys in both lists are merged based on the given hints;
;; hints are structured as follows:
;;   FIXME: explain this for real
;;   - left
;;   - left-only: never take the right
;;   - right
;;   - right-only: never take the left
;;   - (manual #<proc>)
;;
(define merge-annotations
  (case-lambda
    [(al-left al-right) (merge-annotations al-left al-right '())]
    [(al-left al-right merge-hints)
     (ASSERT annotation-list? `(annotations . ,al-left))
     (ASSERT annotation-list? `(annotations . ,al-right))
     (let* ([keys-left  (annot-keys al-left)]
	    [keys-right (annot-keys al-right)]
	    ;; Could make this venn diagram in one call:
	    [keys-both       (intersection keys-left  keys-right)]
	    [keys-left-only  (difference   keys-left  keys-right)]
	    [keys-right-only (difference   keys-right keys-left)])
       
       ;; take all from keys-left-only *unless* they are hinted as right-only
       (append
        (filter id
          (map (lambda (k)
                 (match (cdr (or (assoc k merge-hints) '(_ . #f)))
                   [right-only #f]
                   [,oth `(,k . ,(annotq k al-left))]))
            keys-left-only))

        ;; for keys in both, apply the hint
        (map (lambda (k)
               (match (cdr (or (assoc k merge-hints) '(_ . #f)))
                 [,l (guard (or (eq? l 'left) (eq? l 'left-only)))
                  `(,k . ,(annotq k al-left))]
                 [,r (guard (or (eq? r 'right) (eq? r 'right-only)))
                  `(,k . ,(annotq k al-right))]
                 [(manual ,f)
                  `(,k . ,(f (annotq k al-left) (annotq k al-right)))]
                 [,oth
                  ;; default merging behavior: take the left
                  `(,k . ,(annotq k al-left))]))
          keys-both)

        ;; take all from keys-right-only *unless* they are hinted as left-only
        (filter id
          (map (lambda (k)
                 (match (cdr (or (assoc k merge-hints) '(_ . #f)))
                   [left-only #f]
                   [,oth `(,k . ,(annotq k al-right))]))
            keys-right-only))
        ))]))


;;
;;
;;
) ;; End module
