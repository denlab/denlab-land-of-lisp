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

;; -----------------------------------------------------------------------------
;; (player-attack)
;; -----------------------------------------------------------------------------

;; TODO

;; -----------------------------------------------------------------------------
;; building a graph of the functions of orc-battle
;; -----------------------------------------------------------------------------

;; commented out as the graph is already generated

;; (load "graph-util")

;; easier to switch to data mode than to comment :)

'(graph->png "orc-battle-deps"
            '((game-loop)
              (init-monsters) (init-player)
              (monster-attack) (monster-dead) (monster-hit) (random-monster) (monster-health) (monsters-dead) (monster-show)
              (orc-battle)
              (player-attack) (pick-monster) (player-dead)
              (randval)
              (show-player) (show-monsters))
            '((orc-battle     (init-monsters) (init-player) (game-loop) (player-dead) (monster-attack))
              (game-loop      (player-dead) (monster-dead) (show-player) (player-attack))
              (player-attack  (monster-hit) (monsters-dead) (pick-monster) (random-monster) (monster-attack))
              (random-monster (monster-dead) (random-monster))
              (init-monsters)
              (show-monsters (monster-dead) (monster-health) (monster-show))
              (monster-hit (monster-dead))
              (init-player)
              (monster-attack)
              (monster-dead (monster-health))
              (monster-health (randval))
              (monsters-dead)
              (pick-monster (pick-monster) (monster-dead))
              (player-dead)
              (monster-show)
              (show-player)))

;; -----------------------------------------------------------------------------
;; (monster-dead)
;; -----------------------------------------------------------------------------

(defun monster-dead (m)
  (<= (monster-health m) 0))

;; -----------------------------------------------------------------------------
;; (randval)
;; -----------------------------------------------------------------------------

;; CL-USER> (loop for i below 10
;;     collect (randval 3))
;; (3 3 1 3 1 1 2 2 3 1)

;; CL-USER> (loop for i below 10
;;             collect (randval -2))
;; (1 1 1 1 1 1 1 1 1 1)

(defun randval (n)
  (1+ (random (max 1 n))))

;; -----------------------------------------------------------------------------
;; Generic monster
;; -----------------------------------------------------------------------------

;; A monster with one property : health, with a default value of
;; (randval 10)

(defstruct monster
  (health (randval 10)))

;; -----------------------------------------------------------------------------
;; (monster-hit) method
;; -----------------------------------------------------------------------------

;; CL-USER> (defparameter *m* (make-monster))
;; CL-USER> (setf (monster-health *m*) 3)
;; 3
;; CL-USER> (monster-hit *m* 2)
;; You hit the MONSTER, knocking off 2 health points! " health points! "
;; CL-USER> (monster-hit *m* 2)
;; You killed the MONSTER! "! "

(defmethod monster-hit (m x)
   (decf (monster-health m) x)
    (if (monster-dead m)
        (progn (princ "You killed the ")
              (princ (type-of m))
               (princ "! "))
        (progn (princ "You hit the ")
               (princ (type-of m))
               (princ ", knocking off ")
               (princ x)
               (princ " health points! "))))

;; -----------------------------------------------------------------------------
;; (monster-show) method
;; -----------------------------------------------------------------------------

(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

;; -----------------------------------------------------------------------------
;; (monster-attack) method
;; -----------------------------------------------------------------------------

;; just a place holder (a kind of abstract method)

(defmethod monster-attack (m))

;; -----------------------------------------------------------------------------
;; orc monster
;; -----------------------------------------------------------------------------

(defstruct (orc (:include monster)) (club-level (randval 8)))

(push #'make-orc *monster-builders*)

;; -----------------------------------------------------------------------------
;; (init-monsters)
;; -----------------------------------------------------------------------------

;; build *monster-num* monsters, using *monster-builders*

(defun init-monsters ()
    (setf *monsters*
          (map 'vector
               (lambda (x)
                 (funcall (nth (random (length *monster-builders*))
                               *monster-builders*)))
               (make-array *monster-num*))))

;; -----------------------------------------------------------------------------
;; (show-monsters)
;; -----------------------------------------------------------------------------

;; CL-USER> (show-monsters)
;; 
;; Your foes:
;;    1. **dead**
;;    2. (Health=5) A fierce ORC
;;    3. (Health=1) A fierce ORC

(defun show-monsters ()
    (fresh-line)
    (princ "Your foes:")
    (let ((x 0))
      (map 'list
           (lambda (m)
             (fresh-line)
             (princ "   ")
             (princ (incf x))
             (princ ". ")
             (if (monster-dead m)
                 (princ "**dead**")
                 (progn (princ "(Health=")
                        (princ (monster-health m))
                        (princ ") ")
                        (monster-show m))))
           *monsters*)))

;; -----------------------------------------------------------------------------
;; (monsters-dead)
;; -----------------------------------------------------------------------------

;; return T if all monsters are dead

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

;; -----------------------------------------------------------------------------
;; (pick-monster)
;; -----------------------------------------------------------------------------

;; pick a random monster assuring that it's not already dead

;; NOTES
;;  - Do not handler all monsters are dead
;;  - Monster number starts a 1 

;; CL-USER> (pick-monster)
;; Monster #:ffjakjf
;; That is not a valid monster number.
;; Monster #:100
;; That is not a valid monster number.
;; Monster #:9
;; That monster is alread dead.
;; Monster #:6
;; #S(ORC :HEALTH 1 :CLUB-LEVEL 4)

(defun pick-monster ()
    (fresh-line)
   (princ "Monster #:")
   (let ((x (read)))
     (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
          (progn (princ "That is not a valid monster number.")
                 (pick-monster))
         (let ((m (aref *monsters* (1- x))))
           (if (monster-dead m)
                (progn (princ "That monster is alread dead.")
                       (pick-monster))
               m)))))
  
;; -----------------------------------------------------------------------------
;; (random-monster)
;; -----------------------------------------------------------------------------

(defun random-monster ()
   (let ((m (aref *monsters* (random (length *monsters*)))))
      (if (monster-dead m)
         (random-monster)
         m)))

;; -----------------------------------------------------------------------------
;; (player-attack)
;; -----------------------------------------------------------------------------

;; player choose between (s)tab, (d)ouble-swing, (r)oundhouse-swing
;; - stab : choose one monster, powerfull : 2 + (randval strenght / 2)
;; - double-swing : less powerfull, says how powerfull it is, player
;; choose 2 monsters : (randval strenght / 6)
;; - roundhouse-swing : only one point, but randomly aply the attack :
;; randomly : 1+ (randval strengh / 3)
;; to the monsters

;; when theres loops : handle the case where all the monsters could be dead

;; CL-USER> (player-attack)
;; Attack style: [s]tab [d]ouble swing [r]oundhouse:s
;; 
;; Monster #:1
;; You killed the ORC! "! "
;; CL-USER> (player-attack)
;; 
;; Attack style: [s]tab [d]ouble swing [r]oundhouse:d
;; Your double swing has a strength of 1
;; Monster #:1
;; That monster is alread dead.
;; Monster #:2
;; You hit the ORC, knocking off 1 health points! 
;; Monster #:3
;; You hit the ORC, knocking off 1 health points! " health points! "
;; CL-USER> (player-attack)
;; 
;; Attack style: [s]tab [d]ouble swing [r]oundhouse:r
;; You hit the ORC, knocking off 1 health points! You killed the ORC! You hit the ORC, knocking off 1 health points! NIL
;; CL-USER>

(defun player-attack ()
    (fresh-line)
   (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
   (case (read)
    (s (monster-hit (pick-monster)
                      (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
            (princ "Your double swing has a strength of ")
            (princ x)
            (fresh-line)
            (monster-hit (pick-monster) x)
            (unless (monsters-dead)
              (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monsters-dead)
                   (monster-hit (random-monster) 1))))))

;; -----------------------------------------------------------------------------
;; (game-loop)
;; -----------------------------------------------------------------------------

;; - loop until player is dead or all monsters are dead
;; - the player can attack a number of times depending on his agility :
;;   - 1 + (/ (max 0 *player-agility*) 15)
;; - then each monsters attacks one time, dead monsters must be
;;   handled here

;; CL-USER> (game-loop)
;; 
;; You are a valiant knight with a health of 30, an agility of 30, and a strength of 30
;; Your foes:
;;    1. (Health=19) A fierce ORC
;;    2. **dead**
;;    3. **dead**
;;    4. **dead**
;;    5. **dead**
;;    6. **dead**
;;    7. **dead**
;;    8. **dead**
;;    9. **dead**
;;    10. **dead**
;;    11. **dead**
;;    12. **dead**
;; Attack style: [s]tab [d]ouble swing [r]oundhouse:s
;; 
;; Monster #:1
;; You hit the ORC, knocking off 7 health points! 
;; Your foes:

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda(m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

;; -----------------------------------------------------------------------------
;; (orc-battle)
;; -----------------------------------------------------------------------------

;; init the game
;; 
;; loop until player or monsters are dead and print a message
;; corresponding

(defun orc-battle ()
   (init-monsters)
   (init-player)
   (game-loop)
   (when (player-dead)
     (princ "You have been killed. Game Over."))
   (when (monsters-dead)
      (princ "Congratulations! You have vanquished all of your foes.")))

;; -----------------------------------------------------------------------------
;; orc : monster-show
;; -----------------------------------------------------------------------------

;; CL-USER> (monster-show (make-orc))
;; A wicked orc with a level 6 club" club"

(defmethod monster-show ((m orc))
    (princ "A wicked orc with a level ")
    (princ (orc-club-level m))
    (princ " club"))

;; -----------------------------------------------------------------------------
;; orc : monster-attack
;; -----------------------------------------------------------------------------

;; remove (randval orc-club-level)

;; CL-USER> (monster-attack (make-orc))
;; An orc swings his club at you and knocks off 1 of your health points. 25

(defmethod monster-attack ((m orc))
   (let ((x (randval (orc-club-level m))))
         (princ "An orc swings his club at you and knocks off ")
         (princ x)
         (princ " of your health points. ")
         (decf *player-health* x)))

;; -----------------------------------------------------------------------------
;; hydra : define
;; -----------------------------------------------------------------------------

;; no additional fields

(defstruct (hydra (:include monster)))

(push #'make-hydra *monster-builders*)

;; -----------------------------------------------------------------------------
;; hydra : monster-show
;; -----------------------------------------------------------------------------

;; CL-USER> (monster-show (make-hydra))
;; A malicious hydra with 1 heads." heads."

;; heads = health

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

;; -----------------------------------------------------------------------------
;; hydra : monster-hit
;; -----------------------------------------------------------------------------

;; loses heads on hit

;; CL-USER> (monster-hit (make-hydra) 2)
;; The corpse of the fully decapitated and decapacitated
;;  hydra falls to the floor!
;; 
;; CL-USER> (monster-hit (make-hydra) 1)
;; You lop off 1 of the hydra's heads! " of the hydra's heads! "

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated and decapacitated
 hydra falls to the floor!")
      (progn (princ "You lop off ")
             (princ x)
             (princ " of the hydra's heads! "))))

;; -----------------------------------------------------------------------------
;; hydra : monster-attack
;; -----------------------------------------------------------------------------

;; let's assume it's (randval (/ health 2))

;; CL-USER> (monster-attack (make-hydra))
;; A hydra attacks you with 4 of its heads! It also grows back one
;; more head! 5

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

;; -----------------------------------------------------------------------------
;; slime-mold : define
;; -----------------------------------------------------------------------------

;; has an additional attribute : sliminess with a randval of 5

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))

(push #'make-slime-mold *monster-builders*)

;; -----------------------------------------------------------------------------
;; slime : monster-show
;; -----------------------------------------------------------------------------

;; CL-USER> (monster-show (make-slime-mold))
;; A slime mold with a sliminess of 22

(defmethod monster-show ((m slime-mold))
    (princ "A slime mold with a sliminess of ")
    (princ (slime-mold-sliminess m)))

;; -----------------------------------------------------------------------------
;; slime : monster-attack
;; -----------------------------------------------------------------------------

;; half of the time : remove one health point
;; also attack the agility of the player : randval of sliminess

;; CL-USER> (monster-attack (make-slime-mold))
;; A slime mold wraps around your legs and decreases your agility by 5! It also squirts in your face, taking away a health point! -11

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

;; -----------------------------------------------------------------------------
;; brigand : define
;; -----------------------------------------------------------------------------

;; no addtionnal fields

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

;; -----------------------------------------------------------------------------
;; brigand : monster-attack
;; -----------------------------------------------------------------------------

;; choose the max of player health, agility or strenght
;; if equals choose in this order
;; perform an attack that remove 2 in all cases

;; CL-USER> (monster-attack (make-brigand))
;; 
;; A brigand hits you with his slingshot,
;;  taking off 2 health points! 98
;; CL-USER> (setf *player-strength* 100)
;; 100
;; CL-USER> (monster-attack (make-brigand))
;; 
;; A brigand cuts your arm with his whip,
;;  taking off 2 strength points! 98
;; CL-USER> (setf *player-agility* 100)
;; 100
;; CL-USER> (monster-attack (make-brigand))
;; 
;; A brigand catches your leg with his whip,
;;  taking off 2 agility points! 98
;; CL-USER> 

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (princ "A brigand hits you with his slingshot,
 taking off 2 health points! ")
           (decf *player-health* 2))
          ((= x *player-agility*)
           (princ "A brigand catches your leg with his whip,
 taking off 2 agility points! ")
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (princ "A brigand cuts your arm with his whip,
 taking off 2 strength points! ")
           (decf *player-strength* 2)))))


