;; ----------------------------------------------------------------------------
;; Chapter 10
;; -----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; (add)
;; -----------------------------------------------------------------------------

;; add take 2 args, if its numbers : add them
;;                         list    : append them

;; > (add 3 4)
;; 7
;; > (add '(a b) '(c d))
;; (A B C D)

;; WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
;; 
;; This is not how we should write generic functions in lisp !
;; Should be done using type dispatch using defmethod !
;; 
;; WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!

(defun add (a b)
     (cond ((and (numberp a) (numberp b)) (+ a b))
           ((and (listp a) (listp b)) (append a b))))

