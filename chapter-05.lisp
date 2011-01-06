; ----------------------------------------------------------------------------
; land of lisp functions and data
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
; NODES & EDGES
; ----------------------------------------------------------------------------

(defparameter *nodes* 
'(
    (living-room (you are in the living-room.
                  a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden.
             there is a well in front of you.))
    (attic (you are in the attic.
            there is a giant welding torch in the corner.))))

(defparameter *edges*
'( (living-room (garden west door)
                (attic upstairs ladder))
   (garden (living-room east door))
   (attic (living-room downstairs ladder))))

; ----------------------------------------------------------------------------
; (describe-location)
; ----------------------------------------------------------------------------

;> (describe-location 'living-room *nodes*)
;(YOU ARE IN THE LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH.)

(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

; ----------------------------------------------------------------------------
; (describe-path)
; ----------------------------------------------------------------------------

;> (describe-path '(garden west door))
;(THERE IS A DOOR GOING WEST FROM HERE.)

(defun describe-path (edge)
                 `(there is a ,(caddr edge) going ,(cadr edge) from here.))

; ----------------------------------------------------------------------------
; (describe-paths)
; ----------------------------------------------------------------------------

;> (describe-paths 'living-room *edges*)
;(THERE IS A DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FROM HERE.)

(defun describe-paths (location edges) (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

; ----------------------------------------------------------------------------
; objects & object-locations
; ----------------------------------------------------------------------------

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                                  (bucket living-room)
                                                  (chain garden)
                                                  (frog garden)))

; ----------------------------------------------------------------------------
; (objects-at)
; ----------------------------------------------------------------------------

;(objects-at 'living-room *objects* *object-locations*)
;(WHISKEY BUCKET)

(defun objects-at (loc objs obj-locs)
   (labels ((at-loc-p (obj)
              (eq (cadr (assoc obj obj-locs)) loc)))
     (remove-if-not #'at-loc-p objs)))

; ----------------------------------------------------------------------------
; (describe-objects)
; ----------------------------------------------------------------------------

;> (describe-objects 'living-room *objects* *object-locations*)
;(YOU SEE A WHISKEY ON THE FLOOR. YOU SEE A BUCKET ON THE FLOOR)

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
               `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

; ----------------------------------------------------------------------------
; (look)
; ----------------------------------------------------------------------------

;> (look)
;(YOU ARE IN THE LIVING-ROOM OF A WIZARD’S HOUSE.
;THERE IS A WIZARD SNORING LOUDLY ON THE COUCH.
;THERE IS A DOOR GOING WEST FROM HERE.
;THERE IS A LADDER GOING UPSTAIRS FROM HERE.
;YOU SEE A WHISKEY ON THE FLOOR.
;YOU SEE A BUCKET ON THE FLOOR)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

; ----------------------------------------------------------------------------
; (walk)
; ----------------------------------------------------------------------------

;> (walk 'west)
;(YOU ARE IN A BEAUTIFUL GARDEN.
;THERE IS A WELL IN FRONT OF YOU.
;THERE IS A DOOR GOING EAST FROM HERE.
;YOU SEE A CHAIN ON THE FLOOR.
;YOU SEE A FROG ON THE FLOOR.)

(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
  (if next
      (progn (setf *location* (car next))
             (look))
      '(you cannot go that way.))))

; ----------------------------------------------------------------------------
; (pickup)
; ----------------------------------------------------------------------------

;> (pickup 'whiskey)
;(YOU ARE NOW CARRYING THE WHISKEY)

(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
           `(you are now carrying the ,object))
          (t '(you cannot get that.))))

; ----------------------------------------------------------------------------
; (inventory)
; ----------------------------------------------------------------------------

;> (inventory)
;(ITEMS- WHISKEY)

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*))) 

; ----------------------------------------------------------------------------
; (game-repl)
; ----------------------------------------------------------------------------

(defun game-repl ()
     (loop (print (eval (read)))))

; ----------------------------------------------------------------------------
; (game-read)
; ----------------------------------------------------------------------------

;> (game-read)
;walk east
;(WALK 'EAST)

(defun game-read ()
    (let ((cmd (read-from-string
                     (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                         (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

; ----------------------------------------------------------------------------
; (game-eval)
; ----------------------------------------------------------------------------

(defparameter *allowed-commands* '(look walk pickup inventory))

;> (game-eval '(walk 'north))
;YOU-ARE-NOT-SUPPOSED-TO-GO-THERE

(defun game-eval (sexp) (if (member (car sexp) *allowed-commands* )
                  (eval sexp)
                  '(i dont know this command))
)

; ----------------------------------------------------------------------------
; (tweak-text)
; ----------------------------------------------------------------------------

(defparameter *tweak-text-test-input* 
    (coerce 
        "NOT ONLY DOES THIS SENTENCE HAVE A \"comma,\" IT ALSO MENTIONS THE \"iPad.\""
        'list	
    ))

(defparameter *tweak-text-test-expected*
    (coerce
        "Not only does this sentence have a comma, it also mentions the iPad."
        'list
    ))

(defun tweak-text-test ()
    (if (equal (tweak-text *tweak-text-test-input* t nil)
               *tweak-text-test-expected*
         )
            '(test passed!)
            '(test failed!)
    ))

(defun tweak-text (lst caps lit)
   (when lst
  (let ((item (car lst))
         (rest (cdr lst)))
   (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
         ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
         ((eq item #\") (tweak-text rest caps (not lit)))
           (lit (cons item (tweak-text rest nil lit)))
         ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
         (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

; ----------------------------------------------------------------------------
; (game-print)
; ----------------------------------------------------------------------------

;> (game-print '(not only does this sentence
; have a "comma," it also mentions the "iPad."))
;Not only does this sentence have a comma, it also mentions the iPad.

; ----------------------------------------------------------------------------
; (game-repl) version 2
; ----------------------------------------------------------------------------

(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

; ----------------------------------------------------------------------------
; alist
; ----------------------------------------------------------------------------

(defparameter *wizard-nodes* '((living-room (you are in the living-room.
                                a wizard is snoring loudly on the couch.))
                               (garden (you are in a beautiful garden.
                                there is a well in front of you.))
                               (attic (you are in the attic. there
                                is a giant welding torch in the corner.))))
(defparameter *wizard-edges* '((living-room (garden west door)
                                            (attic upstairs ladder))
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder))))




