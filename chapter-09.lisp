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

