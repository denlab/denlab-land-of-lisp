;; -----------------------------------------------------------------------------
;; Dice of Doom
;; -----------------------------------------------------------------------------

(use-package :lisp-unit)

(defparameter *num-players* 2)
(defparameter *max-dice* 3)
'(defparameter *board-size* 3)
(defparameter *board-size* 4)
(defparameter *board-hexnum* (* *board-size* *board-size*))


;; The board is represented as :
;; - a list for each case from top left to right bottom
;; - each item of the list contains two values :
;;   - the first is the player : 0 : player 1, 1 : player 2
;;   - second is the number of dice

;; -----------------------------------------------------------------------------
;; (board-array)
;; -----------------------------------------------------------------------------

;; takes a list an return an array
;; we need this because we'll need to access the elements in a fast
;;way

(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

;; -----------------------------------------------------------------------------
;; (game-tree)
;; -----------------------------------------------------------------------------

;; build the tree of all possible moves for a certain configuration
;; of the game

 (defun game-tree (board player spare-dice first-move)
   (list player
          board
         (add-passing-move board
                           player
                           spare-dice
                           first-move
                           (attacking-moves board player spare-dice))))

;; -----------------------------------------------------------------------------
;; (add-passing-move)
;; -----------------------------------------------------------------------------
;;
;; Return the moves passed in param, plus the passing moves for this player
;; 
;; Moves are : A list of 2 items :
;; - the description of the move
;;   - for passing moves this is nil
;; - a new game tree representing the entire universe after the move :
;;   - manage reinforcement (using (add-new-dice)
;;   - switch to the next player
;;
;; Note : don't forget the a player must play at least one time

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice))
                             (mod (1+ player) *num-players*)
                             0
                             t))
            moves)))

;; -----------------------------------------------------------------------------
;; (attacking-moves)
;; -----------------------------------------------------------------------------

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (mapcan (lambda (src)
              (when (eq (player src) cur-player)
                (mapcan (lambda (dst)
                          (when (and (not (eq (player dst) cur-player))
                                     (> (dice src) (dice dst)))
                            (list
                             (list (list src dst)
                                   (game-tree (board-attack board cur-player src dst (dice src))
                                              cur-player
                                              (+ spare-dice (dice dst))
                                              nil)))))
                        (neighbors src))))
            (loop for n below *board-hexnum*
               collect n))))

;; find legal moves for this player and board
;;
;; scan each hexagon of the board and check for legal moves according
;;to the rules :
;; - the neighboor case must be another player
;; - the number of dice of the attack must be greater than the
;;attackee
;; - the description of the move is src -> dst
;;
;; Suggestions :
;; - write a some convience functions :
;;   - (player) to determine the player of a given hexagon
;;   - (dice) same for the number of dices

(defun player (board pos)
  (car (aref board pos)))

(defun dice (board pos)
  (cadr (aref board pos)))


(define-test test-player-dice
  (print "hello")
  (let ((board #((0 1) (1 2) (0 3))))
    (assert-equal 0 (player board 0))
    (assert-equal 1 (player board 1))
    (assert-equal 0 (player board 2))
    (assert-equal 1 (dice board 0))
    (assert-equal 2 (dice board 1))
    (assert-equal 3 (dice board 2))))



;; -----------------------------------------------------------------------------
;; (gen-board)
;; -----------------------------------------------------------------------------

;; initialize a random board (returns an array)

(defun gen-board ()
  (board-array (loop for i below *board-hexnum*
                  collect
                  (list (random *num-players*) (1+ (random *max-dice*))))))

;; -----------------------------------------------------------------------------
;; (player-letter n)
;; -----------------------------------------------------------------------------

;; convert the number of a player to A or B

(defun player-letter (n)
  (code-char (+ 97 n)))

(define-test test-player-letter
  (assert-equal #\a (player-letter 0))
  (assert-equal #\b (player-letter 1)))

;; -----------------------------------------------------------------------------
;; (draw-board)
;; -----------------------------------------------------------------------------

;; Example :
;; > (draw-board #((0 3) (0 3) (1 3) (1 1)))
;;     a-3 a-3
;;   b-3 b-1

(defun draw-board (board-array)
  (loop for y below *board-size*
     do
       (fresh-line)
       (loop repeat (- *board-size* y)
            do (princ "  "))
       (loop
          for x below *board-size*
          for hex = (aref board-array (+ x (* y *board-size*)))
            do
            (format t "~a-~a " (player-letter (car hex)) (cadr hex)))))

(define-test test-draw-board
  (assert-equal "    a-3 a-3 
  b-3 b-1 " (with-output-to-string (*standard-output*)
           (draw-board #((0 3) (0 3) (1 3) (1 1))))))

;; -----------------------------------------------------------------------------
;; (neighbors)
;; -----------------------------------------------------------------------------

'(defparameter *board-size* 2)
'(defparameter *board-hexnum* (* *board-size* *board-size*))

(define-test test-neighbors
             (assert-equal '(1 2 3) (sort (neighbors 0) #'<))
             (assert-equal '(0 3)   (sort (neighbors 1) #'<))
             (assert-equal '(0 3)   (sort (neighbors 2) #'<))
             (assert-equal '(0 1 2) (sort (neighbors 3) #'<)))

(defun neighbors (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
       when (and (>= p 0) (< p *board-hexnum*))
       collect p)))

;; -----------------------------------------------------------------------------
;; (board-attack)
;; -----------------------------------------------------------------------------

;; return a board updated after the attack from src to dst :
;; - leave on dice on dst
;; - add (1- dice) to dst and update the player

;; Example : 
;; > (board-attack #((0 3) (0 3) (1 3) (1 1)) 0 1 3 3)
;; #((0 3) (0 1) (1 3) (0 2))

(define-test test-board-attack
  (assert-equal "#((0 3) (0 1) (1 3) (0 2))"
                (princ-to-string (board-attack #((0 3) (0 3)
                                                 (1 3) (1 1)) 0 1 3 3))))

(defun board-attack (board player src dst dice)
  (board-array (loop for pos
                  for hex across board
                  collect (cond ((eq pos src) (list player 1))
                                ((eq pos dst) (list player (1- dice)))
                                (t hex)))))

;; -----------------------------------------------------------------------------
;; (add-new-dice)
;; -----------------------------------------------------------------------------

(define-test test-add-new-dice
  (assert-equal "#((0 2) (1 3) (0 3) (1 1))"
                (princ-to-string (add-new-dice #((0 1) (1 3) (0 2) (1 1)) 0 2)))
  (assert-equal "#((0 2) (1 3) (0 2) (1 1))"
                (princ-to-string (add-new-dice #((0 1) (1 3) (0 1) (1 1)) 0 3))))

;; first version (non TCO optimized)
'(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n)
             (cond ((null lst) nil)
                   ((zerop n) lst)
                   (t (let ((cur-player (caar lst))
                            (cur-dice (cadar lst)))
                        (if (and (eq cur-player player) (< cur-dice *max-dice*))
                            (cons (list cur-player (1+ cur-dice))
                                  (f (cdr lst) (1- n)))
                            (cons (car lst) (f (cdr lst) n))))))))
    (board-array (f (coerce board 'list) spare-dice))))

;; TCO optimized version
(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n acc)
             (cond ((zerop n) (append (reverse acc) lst))
                   ((null lst) (reverse acc))
                   (t (let ((cur-player (caar lst))
                            (cur-dice (cadar lst)))
                        (if (and (eq cur-player player)
                                 (< cur-dice *max-dice*))
                            (f (cdr lst)
                               (1- n)
                               (cons (list cur-player (1+ cur-dice)) acc))
                            (f (cdr lst) n (cons (car lst) acc))))))))
    (board-array (f (coerce board 'list) spare-dice ()))))


'(load "~/Dropbox/project/denlab-land-of-lisp/graph-util.lisp")

'(graph->png "dice-of-dooms-deps"
            '(
              (board-array)
              (gen-board)
              (player-letter)
              (draw-board)
              (game-tree)
              (add-passing-move)
              (attacking-moves)
              (neighbors)
              (board-attack)
              (add-new-dice)
              (play-vs-human)
              (print-info)
              (handle-human)
              (winners)
              (announce-winner))
            '((board-array)
              (gen-board (board-array))
              (player-letter)
              (draw-board (player-letter))
              (game-tree (add-passing-move) (attacking-moves))
              (add-passing-move (game-tree) (add-new-dice))
              (attacking-moves (game-tree) (board-attack) (neighbors))
              (neighbors)
              (board-attack (board-array))
              (add-new-dice (board-array))
              (play-vs-human (print-info) (play-vs-human) (handle-human) (announce-winner))
              (print-info (player-letter) (draw-board))
              (handle-human)
              (winners)
              (announce-winner (winners))))

;; -----------------------------------------------------------------------------
;; (play-vs-human)
;; -----------------------------------------------------------------------------

(defun play-vs-human (tree)
  (print-info tree)
  (if (caddr tree)
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

;; -----------------------------------------------------------------------------
;; (print-info)
;; -----------------------------------------------------------------------------

(defun test-data-tree ()
  '(0 #((0 1) (1 1) (0 2) (1 1))
    (((2 3)
      (0 #((0 1) (1 1) (0 1) (0 1)) ((NIL (1 #((0 1) (1 1) (0 1) (0 1)) NIL))))))))

(define-test test-print-info
  (assert-equal "current player = a
    a-1 b-1 
  a-2 b-1 " (with-output-to-string (*standard-output*)
              (print-info (test-data-tree)))))

(defun print-info (tree)
  (fresh-line)
   (format t "current player = ~a" (player-letter (car tree)))
   (draw-board (cadr tree)))

;; -----------------------------------------------------------------------------
;; (handle-human)
;; -----------------------------------------------------------------------------

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (loop for move in moves
       for n from 1
       do (let ((action (car move)))
            (fresh-line)
            (format t "~a. " n)
            (if action
                (format t "~a -> ~a" (car action) (cadr action))
                (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))

;; -----------------------------------------------------------------------------
;; (winners)
;; -----------------------------------------------------------------------------

(defun winners (board)
  (let ((count-array (make-array *num-players* :initial-element 0)))
    (loop
       for hex across board
       for i from 0
       do
       (format t "count-array:~a" count-array)
       (let ((player (car hex)))
         (setf (aref count-array player)
               (1+ (aref count-array player)))))
    (let ((max-hex (apply #'max (coerce count-array 'list)))
          (result nil))
      (loop
         for i below *num-players*
         do (if (eq (aref count-array i)
                    max-hex)
                (push i result)))
      (reverse result))))

(defun winners (board)
  (let* ((tally (loop for hex across board
                   collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))

(define-test test-winners
  (assert-equal '(0 1) (winners #((0 1) (1 1) (0 2) (1 1))))
  (assert-equal '(1)   (winners #((0 1) (1 1) (1 2) (1 1)))))

;; -----------------------------------------------------------------------------
;; (announce-winner)
;; -----------------------------------------------------------------------------

(define-test test-announce-winner
  (assert-equal "The game is a tie between (a b)"
                (with-output-to-string (*standard-output*)
                  (announce-winner #((0 1) (1 1) (0 2) (1 1)))))
  (assert-equal "The winner is b"
                (with-output-to-string (*standard-output*)
                  (announce-winner #((0 1) (1 1) (1 2) (1 1))))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w))))))

;; -----------------------------------------------------------------------------
;; AI opponent
;; -----------------------------------------------------------------------------

'(load "~/Dropbox/project/denlab-land-of-lisp/graph-util.lisp")

'(graph->png "dice-of-dooms-computer"
            '(
              (rate-position)
              (get-ratings)
              (handle-computer)
              (play-vs-computer))
            '((rate-position (get-ratings))
              (get-ratings (rate-position))
              (handle-computer (get-ratings))
              (play-vs-computer)))

;; -----------------------------------------------------------------------------
;; (get-ratings)
;; -----------------------------------------------------------------------------

(defun get-ratings (tree player)
  (mapcar (lambda (move)
            (rate-position (cadr move) player))
          (caddr tree)))

;; -----------------------------------------------------------------------------
;; (rate-position)
;; -----------------------------------------------------------------------------

(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if moves
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
               (get-ratings tree player))
        (let ((w (winners (cadr tree))))
          (if (member player w)
              (/ 1 (length w))
              0)))))

;; -----------------------------------------------------------------------------
;; (handle-computer)
;; -----------------------------------------------------------------------------

(defun handle-computer (tree)
  (let ((ratings (get-ratings tree (car tree))))
    (cadr (nth (position (apply #'max ratings) ratings) (caddr tree)))))

;; -----------------------------------------------------------------------------
;; (play-vs-computer)
;; -----------------------------------------------------------------------------

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

;; -----------------------------------------------------------------------------
;; create a function that keep a counter between invocations
;; -----------------------------------------------------------------------------

(let ((line-number 0))
  (defun my-print (x)
    (print line-number)
    (print x)
    (incf line-number)
    nil))

;; -----------------------------------------------------------------------------
;; memoize the neighbors function
;; -----------------------------------------------------------------------------

(define-test test-neighbors2
             (assert-equal '(1 2 3) (sort (neighbors2 0) #'<))
             (assert-equal '(0 3)   (sort (neighbors2 1) #'<))
             (assert-equal '(0 3)   (sort (neighbors2 2) #'<))
             (assert-equal '(0 1 2) (sort (neighbors2 3) #'<)))

(let ((old-neighbors (symbol-function 'neighbors))
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (funcall old-neighbors pos)))))

;; -----------------------------------------------------------------------------
;; memoize (game-tree)
;; -----------------------------------------------------------------------------

(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
        (setf (gethash rest previous) (apply old-game-tree rest)))))

;; -----------------------------------------------------------------------------
;; memoize (rate-position)
;; -----------------------------------------------------------------------------

(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
          (setf (gethash tree tab)
                (funcall old-rate-position tree player))))))

;; =============================================================================
;; LAZYNESS
;; =============================================================================

;; -----------------------------------------------------------------------------
;; [lazy]
;; -----------------------------------------------------------------------------
;;
;; Use closures to store :
;;   - a flag for evaluated or not
;;   - the value of eval
;;

(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))

;; -----------------------------------------------------------------------------
;; force
;; -----------------------------------------------------------------------------

(defun force (lazy-value)
  (funcall lazy-value))

;; -----------------------------------------------------------------------------
;; [lazy-cons] (lazy-car) (lazy-cdr)
;; -----------------------------------------------------------------------------

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

(define-test test-lazy-cons-car-cdr
  (let ((foo (lazy-cons 4 7)))
    (assert-equal 4 (lazy-car foo))
    (assert-equal 7 (lazy-cdr foo))))

;; -----------------------------------------------------------------------------
;; All integers
;; -----------------------------------------------------------------------------
;;
;; build the list of all integer using the lazy stuff

(defparameter *integers*
  (labels ((f (n)
             (lazy-cons n (f (1+ n)))))
    (f 1)))

(define-test test-integers
  (assert-equal 1 (lazy-car *integers*))
  (assert-equal 2 (lazy-car (lazy-cdr *integers*)))
  (assert-equal 3 (lazy-car (lazy-cdr (lazy-cdr *integers*)))))

;; -----------------------------------------------------------------------------
;; for non empty lists
;; -----------------------------------------------------------------------------

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

;; -----------------------------------------------------------------------------
;; (make-lazy)
;; -----------------------------------------------------------------------------
;; convert a list to a lazy one

(defun make-lazy (lst)
  (lazy (when lst
          (cons (car lst) (make-lazy (cdr lst))))))

(define-test test-make-lazy
  (let ((l (make-lazy '(a b c))))
    (assert-equal 'a (lazy-car l))
    (assert-equal 'c (lazy-car (lazy-cdr (lazy-cdr l))))))

;; -----------------------------------------------------------------------------
;; (take-all)
;; -----------------------------------------------------------------------------

(defun take-all (lst)
    (unless (lazy-null lst)
      (cons (lazy-car lst) (take-all (lazy-cdr lst))))

'(define-test test-take-all
  (assert-equal '(a b c)
                (take-all (make-lazy '(a b c)))))

;; -----------------------------------------------------------------------------
;; (take)
;; -----------------------------------------------------------------------------

(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))


'(define-test test-take
  (let ((l (make-lazy '(a b c))))
    (assert-equal '(a) (take 1 l))
    (assert-equal '(a b) (take 2 l))
    (assert-equal '(a b c) (take 3 l))
    (assert-equal '(a b c) (take 4 l)))))

;; -----------------------------------------------------------------------------
;; (lazy-mapcar)
;; -----------------------------------------------------------------------------

(define-test test-lazy-mapcar
  (assert-equal '(2 4 6 8 10 12 14 16 18 20)
                (take 10 (lazy-mapcar (lambda (x) (* 2 x))
                                      *integers*))))

(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fun (lazy-car lst))
                (lazy-mapcar fun (lazy-cdr lst))))))

;; -----------------------------------------------------------------------------
;; (lazy-mapcan)
;; -----------------------------------------------------------------------------

(define-test test-lazy-mapcan 
  (assert-equal '(2 4 6 8 10 12 14 16 18 20)
                (take 10 (lazy-mapcan (lambda (x)
                                        (if (evenp x)
                                            (make-lazy (list x))
                                            (lazy-nil)))
                                      *integers*))))
(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur)
             (if (lazy-null lst-cur)
                 (force (lazy-mapcan fun (lazy-cdr lst)))
                 (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null lst)
            (f (funcall fun (lazy-car lst)))))))

;; -----------------------------------------------------------------------------
;; (lazy-find-if)
;; -----------------------------------------------------------------------------

(define-test test-lazy-find-if
  (assert-equal 7
                (lazy-find-if #'oddp (make-lazy '(2 4 6 7 8 10)))))

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
          x
        (lazy-find-if fun (lazy-cdr lst))))))

;; -----------------------------------------------------------------------------
;; (lazy-nth)
;; -----------------------------------------------------------------------------

(defun lazy-nth (n lst)
  (if (zerop n)
      (lazy-car lst)
    (lazy-nth (1- n) (lazy-cdr lst))))

(define-test test-lazy-nth
  (assert-equal 'e (lazy-nth 4 (make-lazy '(a b c d e f g)))))

;; -----------------------------------------------------------------------------
;; (lazy-nth) lazy version
;; -----------------------------------------------------------------------------

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (lazy-cons (list nil
                       (game-tree (add-new-dice board player
                                                (1- spare-dice))
                                  (mod (1+ player) *num-players*)
                                  0
                                  t))
                 moves)))

;; -----------------------------------------------------------------------------
;; (attacking-moves) lazy version
;; -----------------------------------------------------------------------------

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (lazy-mapcan
     (lambda (src)
       (if (eq (player src) cur-player)
           (lazy-mapcan
            (lambda (dst)
              (if (and (not (eq (player dst)
                                cur-player))
                       (> (dice src) (dice dst)))
                  (make-lazy
                   (list (list (list src dst)
                               (game-tree (board-attack board
                                                        cur-player
                                                        src
                                                        dst
                                                        (dice src))
                                          cur-player
                                          (+ spare-dice (dice dst))
                                          nil))))
                  (lazy-nil)))
            (make-lazy (neighbors src)))
           (lazy-nil)))
     (make-lazy (loop for n below *board-hexnum*
                   collect n)))))

;; -----------------------------------------------------------------------------
;; (handle-human) lazy version
;; -----------------------------------------------------------------------------

(defun handle-human (tree)
    (fresh-line)
    (princ "choose your move:")
    (let ((moves (caddr tree)))
      (labels ((print-moves (moves n)
                     (unless (lazy-null moves)
                       (let* ((move (lazy-car moves))
                           (action (car move)))
                          (fresh-line)
                          (format t "~a. " n)
                          (if action
                            (format t "~a -> ~a" (car action) (cadr
                            action))
                           (princ "end turn")))
                       (print-moves (lazy-cdr moves) (1+ n)))))
            (print-moves moves 1))
      (fresh-line)
     (cadr (lazy-nth (1- (read)) moves))))

;; -----------------------------------------------------------------------------
;; (play-vs-human) lazy version
;; -----------------------------------------------------------------------------

(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

;; -----------------------------------------------------------------------------
;; play the game
;; -----------------------------------------------------------------------------

'(play-vs-human (game-tree (gen-board) 0 0 t))

;; -----------------------------------------------------------------------------
;; (limit-tree-depth)
;; -----------------------------------------------------------------------------

(defun limit-tree-depth (tree depth)
    (list (car tree)
      (cadr tree)
     (if (zerop depth)
         (lazy-nil)
        (lazy-mapcar (lambda (move)
                         (list (car move)
                              (limit-tree-depth (cadr move) (1- depth))))
                     (caddr tree)))))

;; -----------------------------------------------------------------------------
;; (handle-computer) with limit
;; -----------------------------------------------------------------------------

(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*)
                              (car tree))))
    (cadr (lazy-nth (position (apply #'max ratings) ratings)
                    (caddr tree)))))

;; -----------------------------------------------------------------------------
;; (play-vs-computer) with limit
;; -----------------------------------------------------------------------------

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

;; -----------------------------------------------------------------------------
;; (score-board)
;; -----------------------------------------------------------------------------

(defun score-board (board player)
  (loop for hex across board
     for pos from 0
     sum (if (eq (car hex) player)
             (if (threatened pos board)
                 1
                 2)
             -1)))

;; -----------------------------------------------------------------------------
;; (threatened)
;; -----------------------------------------------------------------------------

(defun threatened (pos board)
  (let* ((hex (aref board pos))
         (player (car hex))
         (dice (cadr hex)))
    (loop for n in (neighbors pos)
       do (let* ((nhex (aref board n))
                 (nplayer (car nhex))
                 (ndice (cadr nhex)))
            (when (and (not (eq player nplayer)) (> ndice dice))
              (return t))))))

;; -----------------------------------------------------------------------------
;; (get-ratings)
;; -----------------------------------------------------------------------------

(defun get-ratings (tree player)
  (take-all (lazy-mapcar (lambda (move)
                           (rate-position (cadr move) player))
                         (caddr tree))))

;; -----------------------------------------------------------------------------
;; (rate-position)
;; -----------------------------------------------------------------------------

(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
               (get-ratings tree player))
        (score-board (cadr tree) player))))

;; -----------------------------------------------------------------------------
;; (ab-get-ratings-max)
;; -----------------------------------------------------------------------------

(defun ab-get-ratings-max (tree player upper-limit lower-limit)
  (labels ((f (moves lower-limit)
             (unless (lazy-null moves)
               (let ((x (ab-rate-position (cadr (lazy-car moves))
                                          player
                                          upper-limit
                                          lower-limit)))
                 (if (>= x upper-limit)
                     (list x)
                     (cons x (f (lazy-cdr moves) (max x lower-limit))))))))
    (f (caddr tree) lower-limit)))

;; -----------------------------------------------------------------------------
;; (ab-get-ratings-min)
;; -----------------------------------------------------------------------------

(defun ab-get-ratings-min (tree player upper-limit lower-limit)
  (labels ((f (moves upper-limit)
             (unless (lazy-null moves)
               (let ((x (ab-rate-position (cadr (lazy-car moves))
                                          player
                                          upper-limit
                                          lower-limit)))
                 (if (<= x lower-limit)
                     (list x)
                     (cons x (f (lazy-cdr moves) (min x upper-limit))))))))
    (f (caddr tree) upper-limit)))

;; -----------------------------------------------------------------------------
;; (ab-rate-position)
;; -----------------------------------------------------------------------------

(defun ab-rate-position (tree player upper-limit lower-limit)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
        (if (eq (car tree) player)
            (apply #'max (ab-get-ratings-max tree
                                             player
                                             upper-limit
                                             lower-limit))
            (apply #'min (ab-get-ratings-min tree
                                             player
                                             upper-limit
                                             lower-limit)))
        (score-board (cadr tree) player))))

;; -----------------------------------------------------------------------------
;; (ab-rate-position)
;; -----------------------------------------------------------------------------

(defun handle-computer (tree)
  (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
                                     (car tree)
                                     most-positive-fixnum
                                     most-negative-fixnum)))
    (cadr (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)))))

(defparameter *board-size* 5)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(defparameter *ai-level* 4)

;; -----------------------------------------------------------------------------
;; GUI
;; -----------------------------------------------------------------------------


(load "~/Dropbox/project/denlab-land-of-lisp/webserver.lisp")
(load "~/Dropbox/project/denlab-land-of-lisp/chapter-16-macros.lisp")

(defparameter *board-width* 900)
(defparameter *board-height* 500)
(defparameter *board-scale* 64)
(defparameter *top-offset* 3)
(defparameter *dice-scale* 40)
(defparameter *dot-size* 0.05)


(defun draw-die-svg (x y col)
  (labels ((calc-pt (pt)
             (cons (+ x (* *dice-scale* (car pt)))
                   (+ y (* *dice-scale* (cdr pt)))))
           (f (pol col)
             (polygon (mapcar #'calc-pt pol) col)))
    (f '((0 . -1) (-0.6 . -0.75) (0 . -0.5) (0.6 . -0.75))
       (brightness col 40))
    (f '((0 . -0.5) (-0.6 . -0.75) (-0.6 . 0) (0 . 0.25))
       col)
    (f '((0 . -0.5) (0.6 . -0.75) (0.6 . 0) (0 . 0.25))
       (brightness col -40))
    (mapc (lambda (x y)
            (polygon (mapcar (lambda (xx yy)
                               (calc-pt (cons (+ x (* xx *dot-size*))
                                              (+ y (* yy *dot-size*)))))
                             '(-1 -1 1 1)
                             '(-1 1 1 -1))
                     '(255 255 255)))
          '(-0.05 0.125 0.3 -0.3 -0.125 0.05 0.2 0.2 0.45 0.45 -0.45 -0.2)
          '(-0.875 -0.80 -0.725 -0.775 -0.70 -0.625
            -0.35 -0.05 -0.45 -0.15 -0.45 -0.05))))

(defun draw-tile-svg (x y pos hex xx yy col chosen-tile)
   (loop for z below 2
         do (polygon (mapcar (lambda (pt)
                                (cons (+ xx (* *board-scale* (car pt)))
                                      (+ yy (* *board-scale*
                                               (+ (cdr pt) (* (- 1 z) 0.1))))))
                             '((-1 . -0.2) (0 . -0.5) (1 . -0.2)
                                (1 . 0.2) (0 . 0.5) (-1 . 0.2)))
                     (if (eql pos chosen-tile)
                          (brightness col 100)
                        col)))
   (loop for z below (second hex)
         do (draw-die-svg (+ xx
                              (* *dice-scale*
                                 0.3
                                (if (oddp (+ x y z))
                                     -0.3
                                   0.3)))
                           (- yy (* *dice-scale* z 0.8)) col)))

(defparameter *die-colors* '((255 63 63) (63 63 255)))

(defun draw-board-svg (board chosen-tile legal-tiles)
  (loop for y below *board-size*
     do (loop for x below *board-size*
           for pos = (+ x (* *board-size* y))
           for hex = (aref board pos)
           for xx = (* *board-scale* (+ (* 2 x) (- *board-size* y)))
           for yy = (* *board-scale* (+ (* y 0.7) *top-offset*))
           for col = (brightness (nth (first hex) *die-colors*)
                                 (* -15 (- *board-size* y)))
           do (if (member pos legal-tiles)
                  (tag g ()
                    (tag a ("xlink:href" (make-game-link pos))
                      (draw-tile-svg x y pos hex xx yy col chosen-tile)))
                  (draw-tile-svg x y pos hex xx yy col chosen-tile)))))

(defun make-game-link (pos)
  (format nil "/game.html?chosen=~a" pos))

(defparameter *cur-game-tree* nil)
  (defparameter *from-tile* nil)

(defun dod-request-handler (path header params)
   (if (equal path "game.html")
       (progn (princ "<!doctype html>")
              (tag center ()
                    (princ "Welcome to DICE OF DOOM!")
                    (tag br ())
                   (let ((chosen (assoc 'chosen params)))
                     (when (or (not *cur-game-tree*) (not chosen))
                        (setf chosen nil)
                       (web-initialize))
                      (cond ((lazy-null (caddr *cur-game-tree*))
                              (web-announce-winner (cadr *cur-game-tree*)))
                            ((zerop (car *cur-game-tree*))
                              (web-handle-human
                                  (when chosen
                                        (read-from-string (cdr chosen)))))
                           (t (web-handle-computer))))
                    (tag br ())
                   (draw-dod-page *cur-game-tree* *from-tile*)))
       (princ "Sorry... I don't know that page.")))

(defun web-initialize ()
  (setf *from-tile* nil)
  (setf *cur-game-tree* (game-tree (gen-board) 0 0 t)))

(defun web-announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w)))))
  (tag a (href "game.html")
    (princ " play again")))

(defun web-handle-human (pos)
  (cond ((not pos) (princ "Please choose a hex to move from:"))
        ((eq pos 'pass) (setf *cur-game-tree*
                              (cadr (lazy-car (caddr *cur-game-tree*))))
         (princ "Your reinforcements have been placed.")
         (tag a (href (make-game-link nil))
           (princ "continue")))
        ((not *from-tile*) (setf *from-tile* pos)
         (princ "Now choose a destination:"))
        ((eq pos *from-tile*) (setf *from-tile* nil)
         (princ "Move cancelled."))
        (t (setf *cur-game-tree*
                 (cadr (lazy-find-if (lambda (move)
                                       (equal (car move)
                                              (list *from-tile* pos)))
                                     (caddr *cur-game-tree*))))
           (setf *from-tile* nil)
           (princ "You may now ")
           (tag a (href (make-game-link 'pass))
             (princ "pass"))
           (princ " or make another move:"))))

(defun web-handle-computer ()
  (setf *cur-game-tree* (handle-computer *cur-game-tree*))
  (princ "The computer has moved. ")
  (tag script ()
    (princ
     "window.setTimeout('window.location=\"game.html?chosen=NIL\"',5000)")))

(defun draw-dod-page (tree selected-tile)
  (svg *board-width*
       *board-height*
       (draw-board-svg (cadr tree)
                       selected-tile
                       (take-all (if selected-tile
                                     (lazy-mapcar
                                      (lambda (move)
                                        (when (eql (caar move)
                                                   selected-tile)
                                          (cadar move)))
                                      (caddr tree))
                                     (lazy-mapcar #'caar (caddr tree)))))))

;; -----------------------------------------------------------------------------
;; playing
;; -----------------------------------------------------------------------------


'(serve #'dod-request-handler)

;; -----------------------------------------------------------------------------
;; more fun
;; -----------------------------------------------------------------------------


(defparameter *num-players* 4)
(defparameter *die-colors* '((255 63 63) (63 63 255) (63 255 63)
                             (255 63 255)))

(defparameter *max-dice* 5)
(defparameter *ai-level* 2)

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (lazy-mapcan (lambda (src)
                   (if (eq (player src) cur-player)
                       (lazy-mapcan
                        (lambda (dst)
                          (if (and (not (eq (player dst) cur-player))
                                   (> (dice src) 1))
                              (make-lazy (list (list (list src dst)
                                                     (game-tree (board-attack board cur-player src dst (dice src))
                                                                cur-player
                                                                (+ spare-dice (dice dst))
                                                                nil)
                                                     (game-tree (board-attack-fail board cur-player src dst (dice src))
                                                                cur-player
                                                                (+ spare-dice (dice dst))
                                                                nil))))
                              (lazy-nil)))
                        (make-lazy (neighbors src)))
                       (lazy-nil)))
                 (make-lazy (loop for n below *board-hexnum*
                               collect n)))))

(defun board-attack-fail (board player src dst dice)
  (board-array (loop for pos from 0
                  for hex across board
                  collect (if (eq pos src)
                              (list player 1)
                              hex))))

(defun roll-dice (dice-num)
  (let ((total (loop repeat dice-num
                  sum (1+ (random 6)))))
    (fresh-line)
    (format t "On ~a dice rolled ~a. " dice-num total)
    total))

(defun roll-against (src-dice dst-dice)
  (> (roll-dice src-dice) (roll-dice dst-dice)))

(defun pick-chance-branch (board move)
  (labels ((dice (pos)
             (cadr (aref board pos))))
    (let ((path (car move)))
      (if (or (null path) (roll-against (dice (car path))
                                        (dice (cadr path))))
          (cadr move)
          (caddr move)))))

(defun handle-human (tree)
    (fresh-line)
    (princ "choose your move:")
    (let ((moves (caddr tree)))
      (labels ((print-moves (moves n)
                      (unless (lazy-null moves)
                        (let* ((move (lazy-car moves))
                             (action (car move)))
                          (fresh-line)
                          (format t "~a. " n)
                          (if action
                            (format t "~a -> ~a" (car action) (cadr
                            action))
                          (princ "end turn")))
                        (print-moves (lazy-cdr moves) (1+ n)))))
            (print-moves moves 1))
      (fresh-line)
      (pick-chance-branch (cadr tree) (lazy-nth (1- (read)) moves))))

(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*) (car tree))))
    (pick-chance-branch
     (cadr tree)
     (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)))))

(defparameter *dice-odds* #(#(0.84 0.97 1.0 1.0)
                            #(0.44 0.78 0.94 0.99)
                            #(0.15 0.45 0.74 0.91)
                            #(0.04 0.19 0.46 0.72)
                            #(0.01 0.06 0.22 0.46)))

(defun get-ratings (tree player)
  (let ((board (cadr tree)))
    (labels ((dice (pos)
               (cadr (aref board pos))))
      (take-all (lazy-mapcar
                 (lambda (move)
                   (let ((path (car move)))
                     (if path
                         (let* ((src (car path))
                                (dst (cadr path))
                                (odds (aref (aref *dice-odds*
                                                  (1- (dice dst)))
                                            (- (dice src) 2))))
                           (+ (* odds (rate-position (cadr move) player))
                              (* (- 1 odds) (rate-position (caddr move)
                                                           player))))
                         (rate-position (cadr move) player))))
                 (caddr tree))))))

(defun limit-tree-depth (tree depth)
  (list (car tree)
        (cadr tree)
        (if (zerop depth)
            (lazy-nil)
            (lazy-mapcar (lambda (move)
                           (cons (car move)
                                 (mapcar (lambda (x)
                                           (limit-tree-depth x (1- depth)))
                                         (cdr move))))
                         (caddr tree)))))

(defun get-connected (board player pos)
  (labels ((check-pos (pos visited)
             (if (and (eq (car (aref board pos)) player)
                      (not (member pos visited)))
                 (check-neighbors (neighbors pos) (cons pos visited))
                 visited))
           (check-neighbors (lst visited)
             (if lst
                 (check-neighbors (cdr lst) (check-pos (car lst) visited))
                 visited)))
    (check-pos pos '())))

(defun largest-cluster-size (board player)
  (labels ((f (pos visited best)
             (if (< pos *board-hexnum*)
                 (if (and (eq (car (aref board pos)) player)
                          (not (member pos visited)))
                     (let* ((cluster (get-connected board player pos))
                            (size (length cluster)))
                       (if (> size best)
                           (f (1+ pos) (append cluster visited) size)
                           (f (1+ pos) (append cluster visited) best)))
                     (f (1+ pos) visited best))
                 best)))
    (f 0 '() 0)))

(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n)
             (cond ((zerop n) lst)
                   ((null lst) nil)
                   (t (let ((cur-player (caar lst))
                            (cur-dice (cadar lst)))
                        (if (and (eq cur-player player) (< cur-dice *max-dice*))
                            (cons (list cur-player (1+ cur-dice))
                                  (f (cdr lst) (1- n)))
                            (cons (car lst) (f (cdr lst) n))))))))
    (board-array (f (coerce board 'list)
                    (largest-cluster-size board player)))))

'(serve #'dod-request-handler)
