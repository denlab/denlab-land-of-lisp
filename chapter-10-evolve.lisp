;; -----------------------------------------------------------------------------
;; Evolve!
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Game parameters
;; -----------------------------------------------------------------------------

;; coordinates are (x y width height)

;; -----------------------------------------------------------------------------
;; define an animal structure
;; -----------------------------------------------------------------------------

;; properties are : x y energy dir genes
;; 
;; dir notation :      axis direction : 
;;                   
;; | 0 | 1 | 2 |           x    
;; |---+---+---|        +------>
;; | 7 |   | 3 |        |       
;; |---+---+---|      y |       
;; | 6 | 5 | 4 |        v       

;; genes is a list of integer of size 8, will determine the dir of the animal

(defstruct animal x y energy dir genes)

(defun make-plant-hash-table ()
    (make-hash-table :test #'equal))

(defun init-evolve ()
  (defparameter *width* 100)
  (defparameter *height* 30)
  (defparameter *jungle* '(45 10 10 10))
  (defparameter *plant-energy* 80)
  (defparameter *reproduction-energy* 200)
  ;; Hash table to store the position of plants
  ;; keys are (x . y) so we need a custom test for key equality
  (defparameter *plants* (make-plant-hash-table))
  ;; -----------------------------------------------------------------------------
  ;; create the first animal "adam", and put it in *animals*
  ;; -----------------------------------------------------------------------------
  
  ;; start in the middle of the universe
  ;; energy of 1000
  ;; has a direction of top left
  ;; and random genes [1, 10]
  (defparameter *animals*
    (list (make-animal :x (ash *width* -1)
                       :y (ash *height* -1)
                       :energy 1000
                       :dir 0
                       :genes (loop repeat 8
                                 collecting (1+ (random 10)))))))

(init-evolve)

;; -----------------------------------------------------------------------------
;; (random-plant)
;; -----------------------------------------------------------------------------

;; set a plant randomly in the specified region of the world

(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

;; -----------------------------------------------------------------------------
;; (add-plants)
;; -----------------------------------------------------------------------------

;; add plants randomly in the jungle and in the steppes

(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

;; -----------------------------------------------------------------------------
;; move function
;; -----------------------------------------------------------------------------

(use-package :lisp-unit)

;; change the position (x, y) of the animal according to the dir
;; handle the toroid
;; decrease by one the energy

(defun move (animal)
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
                                    (cond ((and (>= dir 2) (< dir 5)) 1)
                                          ((or (= dir 1) (= dir 5)) 0)
                                          (t -1))
                                    *width*)
                                 *width*))
    (setf (animal-y animal) (mod (+ y
                                    (cond ((and (>= dir 0) (< dir 3)) -1)
                                          ((and (>= dir 4) (< dir 7)) 1)
                                          (t 0))
                                    *height*)
                                 *height*))
    (decf (animal-energy animal))))

(define-test test-xy-move
  (flet ((set-xy-dir-move-get-xy (xy-cons dir)
         (let ((a (make-animal :x (car xy-cons) :y (cdr xy-cons) :dir dir :energy 1000)))
           (move a)
           (cons (animal-x a) (animal-y a)))))
    (assert-equal '(0 . 0) (set-xy-dir-move-get-xy '(1 . 1) 0))
    (assert-equal '(1 . 0) (set-xy-dir-move-get-xy '(1 . 1) 1))
    (assert-equal '(2 . 0) (set-xy-dir-move-get-xy '(1 . 1) 2))
    (assert-equal '(2 . 1) (set-xy-dir-move-get-xy '(1 . 1) 3))
    (assert-equal '(2 . 2) (set-xy-dir-move-get-xy '(1 . 1) 4))
    (assert-equal '(1 . 2) (set-xy-dir-move-get-xy '(1 . 1) 5))
    (assert-equal '(0 . 2) (set-xy-dir-move-get-xy '(1 . 1) 6))
    (assert-equal '(0 . 1) (set-xy-dir-move-get-xy '(1 . 1) 7))))

;; -----------------------------------------------------------------------------
;; (turn)
;; -----------------------------------------------------------------------------

;; will change the direction of the animal, depending on the genes
;; genes are organised like that, its determine how the animal will
;; turn.
;; The bigger the value for a gene the more probable its slot will be
;; choose for the direction
;; 
;; | slot | value |
;; |------+-------|
;; |    0 | 1     |
;; |    1 | 1     |
;; |    2 | 101   |
;; |    3 | 1     |
;; |    4 | 1     |
;; |    5 | 1     |
;; |    6 | 1     |
;; |    7 | 1     |
;;
;; Here an impl of the algorithm :
;; - sum all the value
;; - choose a random number R in [0, sum-all[
;; - substract the value of the first slot from R
;; - if R < 0 : you found the slot
;;              otherwise : continue substracting the other slot
;; - once the slot found (so it's between 0 and 7) : Add it to the
;; dir, wrapping around 8

(define-test test-turn-all-genes-equals
  (flet ((turn-with-random (random-value)
           (let ((a (make-animal :dir 0 :genes '(1 1 1 1 1 1 1 1))))
             (turn a :random-value random-value)
             (animal-dir a))))
    (assert-equal 0 (turn-with-random 0))
    (assert-equal 1 (turn-with-random 1))
    (assert-equal 2 (turn-with-random 2))
    (assert-equal 3 (turn-with-random 3))
    (assert-equal 4 (turn-with-random 4))
    (assert-equal 5 (turn-with-random 5))
    (assert-equal 6 (turn-with-random 6))
    (assert-equal 7 (turn-with-random 7))))

(define-test test-turn-gene-2-bigger
  (flet ((turn-with-random (random-value)
           (let ((a (make-animal :dir 0 :genes '(1 1 10 1 1 1 1 1))))
             (turn a :random-value random-value)
             (animal-dir a))))
    (assert-equal 0 (turn-with-random 0))
    (assert-equal 1 (turn-with-random 1))
    (assert-equal 2 (turn-with-random 2))
    (assert-equal 2 (turn-with-random 3))
    (assert-equal 2 (turn-with-random 11))
    (assert-equal 3 (turn-with-random 12))
    (assert-equal 4 (turn-with-random 13))
    (assert-equal 5 (turn-with-random 14))))

(define-test test-turn-wrap-dir
  (flet ((turn-with-random (random-value)
           (let ((a (make-animal :dir 4 :genes '(1 1 1 1 1 1 1 1))))
             (turn a :random-value random-value)
             (animal-dir a))))
    (assert-equal 7 (turn-with-random 3))
    (assert-equal 0 (turn-with-random 4))
    (assert-equal 1 (turn-with-random 5))))

;; I added a hook for random, I could de probably better, overring
;; random in the tests ? 
(defun turn (animal &key (random-value nil random-value-supplied-p))
  (let ((x (if random-value-supplied-p
               random-value
               (random (apply #'+ (animal-genes animal))))))
     (labels ((angle (genes x)
                (let ((xnu (- x (car genes))))
                   (if (< xnu 0)
                      0
                       (1+ (angle (cdr genes) xnu))))))
       (setf (animal-dir animal)
                (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
                    8)))))

;; -----------------------------------------------------------------------------
;; (eat)
;; -----------------------------------------------------------------------------

;; if there's a plant in the animal location :
;; - add *plant-energy* to the animal energy
;; - remove the plant from the world, you can use remhash

(define-test test-eat-there-is-a-plant
  (let ;; given
      ((a (make-animal :x 0 :y 0 :energy 10)))
    (setf (gethash '(0 . 0) *plants*) t)
    (setf *plant-energy* 80)
    ;; pre-conditions
    (assert-equal 10 (animal-energy a))
    (assert-equal t (gethash '(0 . 0) *plants*))
    ;; when
    (eat a)
    ;; then
    (assert-equal 90 (animal-energy a))
    (assert-equal nil (gethash '(0 . 0) *plants*))))

(define-test test-eat-there-is-a-plant
  (assert-equal 1 1))

(define-test test-eat-there-is-no-plant
  (let ;; given
      ((a (make-animal :x 0 :y 0 :energy 10)))
    (remhash '(0 . 0) *plants*) 
    (setf *plant-energy* 80)
    ;; pre-conditions
    (assert-equal 10 (animal-energy a))
    (assert-equal nil (gethash '(0 . 0) *plants*))
    ;; when
    (eat a)
    ;; then
    (assert-equal 10 (animal-energy a))
    (assert-equal nil (gethash '(0 . 0) *plants*))))

(defun eat (a)
  (let ((pos (cons (animal-x a) (animal-y a))))
    (when (gethash pos *plants*)
      (incf (animal-energy a) *plant-energy*)
      (remhash pos *plants*))))

;; -----------------------------------------------------------------------------
;; (reproduce (animal &key (rand-val-list nil rand-val-list-supplied-p)))
;; -----------------------------------------------------------------------------

;; only reproduce if energy >= *reproduction-energy*
;; the animal will leave half the energy to its child
;; one of the genes is choosen randomly and mutated :
;;   - equal chance to : not change, add 1 or dec 1
;;   - genes must all be >= 1
;;
;; You can use
;;   - copy-structure to shallow copy structs
;;   - copy-list to copy list (shallow also ? )

(defun reproduce (animal &key (rand-val-list nil rand-val-list-supplied-p))
  (when (>= (animal-energy animal) *reproduction-energy*)
    (setf (animal-energy animal) (ash *reproduction-energy* -1))
    (let ((half-energy (ash *reproduction-energy* -1))
          (child (copy-structure animal))
          (random-8 (if rand-val-list-supplied-p (car rand-val-list)  (random 8)))
          (random-3 (if rand-val-list-supplied-p (cadr rand-val-list) (random 3)))
          (genes (copy-list (animal-genes animal))))
      (setf (nth random-8 genes)
            (max 1 (+ (nth random-8 genes) (1- random-3))))
      (setf (animal-genes child) genes)
      (push child *animals*))))

(define-test test-equal-deep
  (defstruct s :prop1 :prop2)
  (assert-equal t (equal-deep (make-s :prop1 "foo" :prop2 '(1 2 3))
                              (make-s :prop1 "foo" :prop2 '(1 2 3))))
  (assert-equal nil (equal-deep (make-s :prop1 "foo" :prop2 '(1 2 3))
                                (make-s :prop1 "foo" :prop2 '(1 2 4)))))

(defun equal-deep (a b)
  (equal (princ-to-string a) (princ-to-string b)))




(define-test test-reproduce
  (loop
     for parent in (loop repeat 4
                      collect (make-animal :x 1 :y 2 :energy *reproduction-energy* :dir 3 :genes '(1 2 1 1 1 1 1 1)))
     with half-parent-energy = (ash *reproduction-energy* -1)
     for rand-values in '((1 0) (2 1) (3 2) (4 0))
     for expected-child in (list (make-animal :x 1 :y 2 :energy half-parent-energy :dir 3 :genes '(1 1 1 1 1 1 1 1))
                                 (make-animal :x 1 :y 2 :energy half-parent-energy :dir 3 :genes '(1 2 1 1 1 1 1 1))
                                 (make-animal :x 1 :y 2 :energy half-parent-energy :dir 3 :genes '(1 2 1 2 1 1 1 1))
                                 (make-animal :x 1 :y 2 :energy half-parent-energy :dir 3 :genes '(1 2 1 1 1 1 1 1)))
     do 
       (init-evolve)
       (setf *animals* nil)
       (reproduce parent :rand-val-list rand-values)
       (assert-equal half-parent-energy (animal-energy parent) parent)
       (let ((actual-child (car *animals*)))
         (assert-equal t (equal-deep expected-child actual-child) expected-child actual-child rand-values))))

(define-test test-reproduce-not-enough-energy
  (init-evolve)
  (setf *animals* nil)
  (reproduce (make-animal :energy (1- *reproduction-energy*)))
  (assert-equal nil *animals*))

;; -----------------------------------------------------------------------------
;; (update-world ())
;; -----------------------------------------------------------------------------
;;
;; - remove all dead animals from the world (energy <= 0)
;; - for each animal : turn, move, eat, reproduce
;; - adds two plants




(defun test-util-dummy-animal ()
    (make-animal :x 0 :y 0 :energy 1 :dir 0 :genes '(1 1 1 1 1 1 1 1)))

(define-test test-update-world-remove-dead
  (init-evolve)
  (setf *animals* nil)
  (push (make-animal :energy -1) *animals*)
  (push (make-animal :energy 0) *animals*)
  (push (test-util-dummy-animal) *animals*)
  (update-world)
  (assert-equal 1 (length *animals*)))

(define-test test-update-world-add-two-plants
  (init-evolve)
  (setf *plants* (make-hash-table))
  (update-world)
  (assert-equal 2 (hash-table-count *plants*)))

(defun update-world ()
  (setf *animals*  (remove-if-not (lambda (a) (> (animal-energy a) 0))
                                  *animals*))
  (mapc (lambda (a) (turn a) (move a) (eat a) (reproduce a))
        *animals*)
  (add-plants))

;; -----------------------------------------------------------------------------
;; (draw-world)
;; -----------------------------------------------------------------------------

;; CL-USER> (draw-world)
;; 
;; |                                              M  M       M                               *          |
;; |             M                                         M         M                        *    M    |
;; |       *                                                   *                                      M |
;; |                   M                *

(make-string 5 :initial-element #\A)

(defun get-row-string (animal-list plant-hashmap world-width world-height row-pos)
  (let ((result (make-string world-width :initial-element #\space)))
    (loop for i below world-width
       do
         (let ((c (if (gethash (cons i row-pos) plant-hashmap)
                      "*"
                      " ")))
           (setf (subseq result i (1+ i)) c)))
    (mapc (lambda (a)
            (if  (= row-pos (animal-y a))
                 (setf (subseq result (animal-x a) (1+ (animal-x a))) "M")))
          animal-list)
    (concatenate 'string "|" result "|")))

;; alternative version run x25 time faster on my machine :)
(defun draw-world-2 ()
  (loop for i below *height*
     do
       (fresh-line)
       (princ (get-row-string *animals* *plants* *width* *height* i))))


(define-test test-get-row-string
  ;; |   |   0 | 1 | 2 |
  ;; |---+-----+---+---|
  ;; | 0 | M * |   | M |
  ;; +---+-----+---+---+
  ;; | 1 | M * |   | * |
  ;; +---+-----+---+---+
  (let ((world-width 3)
        (world-height 2)
        (animal-list (list (make-animal :x 0 :y 0)
                           (make-animal :x 2 :y 0)
                           (make-animal :x 0 :y 1)))
        (plant-hashmap (make-plant-hash-table)))
    (setf (gethash '(0 . 0) plant-hashmap) t)
    (setf (gethash '(0 . 1) plant-hashmap) t)
    (setf (gethash '(2 . 1) plant-hashmap) t)
    (assert-equal "|M M|" (get-row-string animal-list plant-hashmap world-width world-height 0))
    (assert-equal "|M *|" (get-row-string animal-list plant-hashmap world-width world-height 1))))

(defun draw-world ()
   (loop for y
          below *height*
          do (progn (fresh-line)
                    (princ "|")
                   (loop for x
                          below *width*
                         do (princ (cond ((some (lambda (animal)
                                                   (and (= (animal-x animal) x)
                                                        (= (animal-y animal) y)))
                                                 *animals*)
                                         #\M)
                                        ((gethash (cons x y) *plants*) #\*)
                                         (t #\space))))
                    (princ "|"))))

;; -----------------------------------------------------------------------------
;; (evolution)
;; -----------------------------------------------------------------------------

;; - using the repl
;; - let the user enter a number of days and run it
;; - if the input is incorrect : only run update
;; - the keyboard)
;; - display a dot every 1000 iteration to show that gui not freezed
;; - quit : quit the simulation
;; - you can use :
;;   - read-line
;;   - parse-integer :junk-allowed t

(defun evolution ()
   (draw-world)
    (fresh-line)
   (let ((str (read-line)))
     (cond ((equal str "quit") ())
           (t (let ((x (parse-integer str :junk-allowed t)))
                 (if x
                    (loop for i
                        below x
                        do (update-world)
                        if (zerop (mod i 1000))
                        do (princ #\.))
                     (update-world))
                (evolution))))))

(run-tests)
