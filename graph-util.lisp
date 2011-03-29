
; ----------------------------------------------------------------------------
; graphiz stuff
; ----------------------------------------------------------------------------

; ----------------------------------------------------------------------------
; (dot-name)
; ----------------------------------------------------------------------------

;> (dot-name 'foo!)
;"FOO_"

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

; ----------------------------------------------------------------------------
; (dot-label)
; ----------------------------------------------------------------------------

(defparameter *max-label-length* 30)

;> (dot-label '(living-room (YOU ARE IN THE LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH.)))
;"(LIVING-ROOM (YOU ARE IN TH..."

(defun dot-label (exp)
    (if exp
     (let ((s (write-to-string exp :pretty nil)))
       (if (> (length s) *max-label-length*)
           (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

; ----------------------------------------------------------------------------
; (nodes->dot)
; ----------------------------------------------------------------------------

;>  (nodes->dot *wizard-nodes*)
;LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
;GARDEN[label="(GARDEN (YOU ARE IN A BEAUT..."];
;ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];

(defun nodes->dot (nodes)
   (mapc (lambda (node)
            (fresh-line)
           (princ (dot-name (car node)))
            (princ "[label=\"")
           (princ (dot-label node))
            (princ "\"];"))
          nodes))

; ----------------------------------------------------------------------------
; (edges->dot)
; ----------------------------------------------------------------------------

;> (edges->dot *wizard-edges*)
;  LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
;  LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
;  GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
;  ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

; ----------------------------------------------------------------------------
; (graph->dot)
; ----------------------------------------------------------------------------

;> (graph->dot *wizard-nodes* *wizard-edges*)
;digraph{
;LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
;GARDEN[label="(GARDEN (YOU ARE IN A BEAUT..."];
;ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];
;LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
;LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
;GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
;ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];}

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
    (princ "}"))

; ----------------------------------------------------------------------------
; (dot->png)
; ----------------------------------------------------------------------------

 (defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                     fname
                     :direction :output
                     :if-exists :supersede)
      (funcall thunk))
    (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

; ----------------------------------------------------------------------------
; (graph->png)
; ----------------------------------------------------------------------------

;; A directed graph with node description
;; 
;; (graph->png "graph-with-descr"
;;            '((node-1 node-1-descr) (node-2 node-2-descr))
;;            '((node-1 (node-2 edge-1->2-descr))))

;; A directed graph without node description
;; 
;; (graph->png "graph-without-descr"
;;            '((node-1) (node-2))
;;            '((node-1 (node-2))))



(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))

; ----------------------------------------------------------------------------
; (uedges->dot)
; ----------------------------------------------------------------------------

;(uedges->dot *wizard-edges* )
;GARDEN--LIVING_ROOM[label="(EAST DOOR)"];
;ATTIC--LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];

(defun uedges->dot (edges)
   (maplist (lambda (lst)
               (mapc (lambda (edge)
                      (unless (assoc (car edge) (cdr lst))
                         (fresh-line)
                         (princ (dot-name (caar lst)))
                         (princ "--")
                         (princ (dot-name (car edge)))
                         (princ "[label=\"")
                         (princ (dot-label (cdr edge)))
                         (princ "\"];")))
                     (cdar lst)))
             edges))

; ----------------------------------------------------------------------------
; (ugraph->dot)
; ----------------------------------------------------------------------------

;> (ugraph->dot *wizard-nodes* *wizard-edges* )
;graph{
;LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
;GARDEN[label="(GARDEN (YOU ARE IN A BEAUT..."];
;ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];
;GARDEN--LIVING_ROOM[label="(EAST DOOR)"];
;ATTIC--LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];}

 (defun ugraph->dot (nodes edges)
   (princ "graph{")
    (nodes->dot nodes)
    (uedges->dot edges)
    (princ "}"))

; ----------------------------------------------------------------------------
; (ugraph->png)
; ----------------------------------------------------------------------------

 (defun ugraph->png (fname nodes edges)
    (dot->png fname
              (lambda ()
                (ugraph->dot nodes edges))))
