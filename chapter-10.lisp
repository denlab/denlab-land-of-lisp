;; ----------------------------------------------------------------------------
;; Chapter 10
;; -----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; (add-bad-style)
;; -----------------------------------------------------------------------------

;; add-bad-style take 2 args, if its numbers : add them
;;                         list    : append them

;; > (add-bad-style 3 4)
;; 7
;; > (add-bad-style '(a b) '(c d))
;; (A B C D)

;; WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
;; 
;; This is not how we should write generic functions in lisp !
;; Should be done using type dispatch using defmethod !
;; 
;; WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!

(defun add-bad-style (a b)
     (cond ((and (numberp a) (numberp b)) (+ a b))
           ((and (listp a) (listp b)) (append a b))))

;; ----------------------------------------------------------------------------
;; (add)
;; -----------------------------------------------------------------------------

;; Like add-bad-style but with a good style (defmethod)

(defmethod add ((a number) (b number))
  (+ a b))

(defmethod add ((a list) (b list))
  (append a b))

;; ----------------------------------------------------------------------------
;; ORC BATTLE !
;; -----------------------------------------------------------------------------

(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

;; ----------------------------------------------------------------------------
;; (orc-battle)
;; -----------------------------------------------------------------------------

;; TODO

;; ----------------------------------------------------------------------------
;; (game-loop)
;; -----------------------------------------------------------------------------

;; TODO

;; ----------------------------------------------------------------------------
;; Player Management Function
;; -----------------------------------------------------------------------------

(defun init-player ()
    (setf *player-health* 30)
    (setf *player-agility* 30)
    (setf *player-strength* 30))

(defun player-dead ()
    (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

;; ----------------------------------------------------------------------------
;; (player-attack)
;; -----------------------------------------------------------------------------

