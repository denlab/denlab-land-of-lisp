(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

; ----------------------------------------------------------------------------
; (make-edge-list)
; ----------------------------------------------------------------------------

; composed of (random-node) and (edge-pair)

;> (make-edge-list)
;((16 . 20) (20 . 16) (9 . 3) (3 . 9) (25 . 18) (18 . 25) (30 . 29)
; (29 . 30) (26 . 13) (13 . 26) (12 . 25) (25 . 12) (26 . 22) (22 . 26)
; (30 . 29) (29 . 30) (3 . 14) (14 . 3) (28 . 6) (6 . 28) (4 . 8) (8 . 4)
; (27 . 8) (8 . 27) (3 . 30) (30 . 3) (25 . 16) (16 . 25) (5 . 21) (21 . 5)
; (11 . 24) (24 . 11) (14 . 1) (1 . 14) (25 . 11) (11 . 25) (21 . 9) (9 . 21)
; (12 . 22) (22 . 12) (21 . 11) (11 . 21) (11 . 17) (17 . 11) (30 . 21) (21 . 30)
; (3 . 11) (11 . 3) (24 . 23) (23 . 24) (1 . 24) (24 . 1) (21 . 19) (19 . 21) (25 . 29)
; (29 . 25) (1 . 26) (26 . 1) (28 . 24) (24 . 28) (20 . 15) (15 . 20)
; (28 . 25) (25 . 28)
; (2 . 11) (11 . 2) (11 . 24) (24 . 11) (29 . 24) (24 . 29)
;(18 . 28) (28 . 18) (14 . 15)
; (15 . 14) (16 . 10) (10 . 16) (3 . 26) (26 . 3) (18 . 9) (9 . 18) (5 . 12)
; (12 . 5) (11 . 18) (18 . 11) (20 . 17) (17 . 20) (25 . 3) (3 . 25))

(defun random-node ()
    (1+ (random *node-num*)))

(defun edge-pair (a b)
    (unless (eql a b)
      (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))))

; ----------------------------------------------------------------------------
; (direct-edges)
; ----------------------------------------------------------------------------

;> (direct-edges 'a '((a . b) (b . a) (a . c) (c . a) (c . d) (d . c)))
;((A . B) (A . C))

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

; ----------------------------------------------------------------------------
; (get-connected)
; ----------------------------------------------------------------------------

;> (get-connected 'b '((a . b) (b . a) (a . c) (c . a) (c . d) (d . c) (x . y) (y . x)))
;(D C A B)

(defun get-connected (node edge-list)
   (let ((visited nil))
      (labels ((traverse (node)
                 (unless (member node visited)
                  (push node visited)
                  (mapc (lambda (edge)
                           (traverse (cdr edge)))
                         (direct-edges node edge-list)))))
        (traverse node))
      visited))

; ----------------------------------------------------------------------------
; (find-islands)
; ----------------------------------------------------------------------------

;> (find-islands '(a b c d e) '((a . b) (b . a) (c . d) (d . c)))
;((E) (D C) (B A))

 (defun find-islands (nodes edge-list)
    (let ((islands nil))
     (labels ((find-island (nodes)
                 (let* ((connected (get-connected (car nodes) edge-list))
                        (unconnected (set-difference nodes connected)))
                   (push connected islands)
                  (when unconnected
                     (find-island unconnected)))))
        (find-island nodes))
      islands))

; ----------------------------------------------------------------------------
; (connect-with-bridges)
; ----------------------------------------------------------------------------

;> (connect-with-bridges '((E) (D C) (B A)))
;((E . D) (D . E) (D . B) (B . D))

(defun connect-with-bridges (islands)
   (when (cdr islands)
      (append (edge-pair (caar islands) (caadr islands))
              (connect-with-bridges (cdr islands)))))

; ----------------------------------------------------------------------------
; (connect-all-islands)
; ----------------------------------------------------------------------------

;> (connect-all-islands '(a b c d e) '((a . b) (b . a) (c . d) (d . c)))
;((E . D) (D . E) (D . B) (B . D) (A . B) (B . A) (C . D) (D . C))

(defun connect-all-islands (nodes edge-list)
    (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

; ----------------------------------------------------------------------------
; (add-cops)
; ----------------------------------------------------------------------------

;> (add-cops '((1 (2)) (2 (1) (3)) (3 (2))) '((2 . 3) (3 . 2)))
;((1 (2)) (2 (1) (3 COPS)) (3 (2 COPS)))

(defun add-cops (edge-alist edges-with-cops)
   (mapcar (lambda (x)
              (let ((node1 (car x))
                    (node1-edges (cdr x)))
                (cons node1
                     (mapcar (lambda (edge)
                                (let ((node2 (car edge)))
                                 (if (intersection (edge-pair node1 node2)
                                                    edges-with-cops
                                                    :test #'equal)
                                      (list node2 'cops)
                                    edge)))
                              node1-edges))))
            edge-alist))

; ----------------------------------------------------------------------------
; (edges-to-alist)
; ----------------------------------------------------------------------------

; hint : use (remove-duplicates :test ...)
; hint : use (direct-edges)

;> (edges-to-alist '((1 . 2) (2 . 1) (2 . 3) (3 . 2)))
;((1 (2)) (2 (1) (3)) (3 (2)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))

; ----------------------------------------------------------------------------
; (make-city-edges)
; ----------------------------------------------------------------------------
;> (make-city-edges)
;((24 (4)) (4 (24) (23)) (28 (20) (5)) (10 (9)) (13 (11) (2)) (17 (8) (19))
;(25 (11 COPS)

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num* collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (edge-with-cops (remove-if-not (lambda (x) (zerop (random *cop-odds*)))
                                        edge-list)))
    (add-cops (edges-to-alist edge-list) edge-with-cops)))

; ----------------------------------------------------------------------------
; (neighbors)
; ----------------------------------------------------------------------------

;> (neighbors '2 '((1 (2)) (2 (1) (3)) (3 (2))))
;(1 3)
;> (neighbors '1 '((1 (2)) (2 (1) (3)) (3 (2))))
;(2)

(defun neighbors (node edge-alist)
  (mapcar #'car
          (cdr (assoc node edge-alist))))

; ----------------------------------------------------------------------------
; (within-one)
; ----------------------------------------------------------------------------

;> (within-one '1 '2 '((1 (2)) (2 (1) (3)) (3 (2))))
;(2)
;> (within-one '1 '3 '((1 (2)) (2 (1) (3)) (3 (2))))
;NIL

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

; ----------------------------------------------------------------------------
; (within-two)
; ----------------------------------------------------------------------------

;(within-two  '1 '2 '((1 (2)) (2 (1) (3)) (3 (2 4)) (4 (3))))
;(2)
;(within-two  '1 '3 '((1 (2)) (2 (1) (3)) (3 (2 4)) (4 (3))))
;(3)
;(within-two  '1 '4 '((1 (2)) (2 (1) (3)) (3 (2 4)) (4 (3))))
;NIL

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x) (within-one x b edge-alist))
            (neighbors a edge-alist))))

; ----------------------------------------------------------------------------
; (make-city-nodes)
; ----------------------------------------------------------------------------

; rules : 1 node with wumpus, 1 or 2 nodes from wumpus : blood!
;         *worm-num* glow-worms, from 1 node to glow-worms : lights!
;         sirens! in cops nodes

;> (make-city-nodes (make-city-edges) )
;((1 BLOOD! SIRENS!) (2 BLOOD! SIRENS!) (3 SIRENS!) (4 WUMPUS SIRENS!)
;(5 BLOOD!) (6 LIGHTS! SIRENS!) (7 BLOOD!) (8 BLOOD! LIGHTS!) (9
;BLOOD! LIGHTS! SIRENS!) (10) (11 BLOOD! SIRENS!) (12) (13 BLOOD! SIRENS!) (14) (15) (16 GLOW-WORM) (17) (18 LIGHTS! SIRENS!) (19) (20 LIGHTS!) (21 BLOOD!) (22 BLOOD!) (23 BLOOD! GLOW-WORM SIRENS!) (24 LIGHTS!) (25 GLOW-WORM) (26) (27 LIGHTS!) (28 SIRENS!) (29) (30))


(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                       collect (random-node))))
    (loop for n from 1 to *node-num*
       collect (append (list n)
                       (cond ((eql n wumpus) '(wumpus)))
                       (cond ((member n glow-worms)
                              '(glow-worm))
                             ((some (lambda (worm)
                                      (within-one n worm edge-alist))
                                    glow-worms)
                              '(lights!)))
                       (when (some #'cdr (cdr (assoc n edge-alist)))
                         '(sirens!))))))

; ----------------------------------------------------------------------------
; (find-empty-node)
; ----------------------------------------------------------------------------

;> (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
;((1) (2) (3) (4) (5 GLOW-WORM) (6) (7) (8) (9) (10) (11) (12) (13) (14) (15) (16) (17) (18) (19) (20 GLOW-WORM) (21 GLOW-WORM) (22) (23 WUMPUS) (24) (25) (26) (27) (28) (29) (30))
;> (find-empty-node) 
;7

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node)
      x)))

; ----------------------------------------------------------------------------
; (draw-city)
; ----------------------------------------------------------------------------

; will draw a graph 
; hint : no arg
; take as global args : *congestion-city-edges*
;                       *congestion-city-nodes*

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

; ----------------------------------------------------------------------------
; (new-game)
; ----------------------------------------------------------------------------

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city))

; ----------------------------------------------------------------------------
; (known-city-nodes)
; ----------------------------------------------------------------------------

; hint : use mapcan

; > (new-game)
; (push '28 *visited-nodes* )
; > *player-pos*
; 17
; > *visited-nodes*
; (28 17)
; > (known-city-nodes) 
; ((17 *) (9 ?) (21 ?) (4 ?) (12 ?) (19 ?) (28 BLOOD! LIGHTS! SIRENS!) (6 ?) (8 ?) (18 ?))


