;; -----------------------------------------------------------------------------
;; Chapter 18 - Macros 
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; let1
;; -----------------------------------------------------------------------------

;; A macro that allow to define a lexical var (like with let) but for
;; only one var, so with less parentheses ;
;;
;; (let1 foo (+ 2 3)
;;    (* foo foo))

(let ((foo (+ 2 3)))
  (* foo foo))

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

;; -----------------------------------------------------------------------------
;; (my-length)
;; -----------------------------------------------------------------------------
;;
;; Write a function that calculate the length of a function.
;; Use reccursion with proprer TCO

(use-package :lisp-unit)

(defun my-length (l)
  (labels ((f (l acc)
             (if l
                 (f (cdr l) (1+ acc))
                 acc)))
    (f l 0)))

(compile 'my-length)

(define-test test-my-length
  (let ((big-list (loop for i below 100000 collect i)))
    (assert-equal 0  (my-length ()))
    (assert-equal 1 (my-length '(a)))
    (assert-equal 2 (my-length '(a b)))
    '(assert-equal 100000 (my-length big-list))))

;; -----------------------------------------------------------------------------
;; [split]
;; -----------------------------------------------------------------------------
;;
;; Define a macro that splits lists, like this :

(define-test test-split
  (assert-equal "This can be split into 2 and (3)."
                (with-output-to-string (*standard-output*)
                  (split '(2 3)
                         (format t "This can be split into ~a and ~a." head tail)
                         (format t "This cannot be split."))))
  (assert-equal "This cannot be split."
                (with-output-to-string (*standard-output*)
                  (split '()
                         (format t "This can be split into ~a and ~a." head tail)
                         (format t "This cannot be split.")))))

(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
             (tail (cdr ,val)))
         ,yes)
       ,no))

;; -----------------------------------------------------------------------------
;; cleanup (my-length)
;; -----------------------------------------------------------------------------

;; old
'(defun my-length (l)
  (labels ((f (l acc)
             (if l
                 (f (cdr l) (1+ acc))
                 acc)))
    (f l 0)))

(defun my-length (l)
  (labels ((f (l acc)
             (split l
                    (f tail (1+ acc))
                    acc)))
    (f l 0)))

(compile 'my-length)

;; -----------------------------------------------------------------------------
;; fix the repeated expression bug in the macro
;; -----------------------------------------------------------------------------

(defmacro split (val yes no)
  `(let1 v ,val
     (if v
         (let ((head (car v))
               (tail (cdr v)))
           ,yes)
         ,no)))

;; -----------------------------------------------------------------------------
;; fix the macro variable capture bug 
;; -----------------------------------------------------------------------------

(define-test test-split-var-capture-bug
  (assert-equal 4
                (let1 v 2
                  (split nil
                         (* 2 v)
                         (* 2 v)))))

(defmacro split (val yes no)
  (let1 g (gensym)
    `(let1 ,g ,val
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))


;; -----------------------------------------------------------------------------
;; (pairs)
;; -----------------------------------------------------------------------------
;;
;; will be used for the recurse macro

;; my first try
'(defun pairs (lst)
  (if lst
      (cons (cons (car lst) (cadr lst))
            (pairs (cddr lst)))
      nil))

;; use split, and be TCO friendly

(defun pairs (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cddr lst) (cons (cons (car lst) (cadr lst))
                                     acc))
                 (reverse acc))))
    (f lst nil)))

(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail))
                                            acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))

(define-test test-pairs
  (assert-equal '((a . b) (c . d) (e . f)) (pairs '(a b c d e f)))
  (assert-equal '((a . b)) (pairs '(a b c ))))

;; -----------------------------------------------------------------------------
;; [recurse]
;; -----------------------------------------------------------------------------

(define-test test-recurse
  (assert-equal "2
1
lift-off!"
   (with-output-to-string (*standard-output*)
     (recurse (n 2)
       (fresh-line)
       (if (zerop n)
           (princ "lift-off!")
           (progn (princ n)
                  (self (1- n))))))))

(macroexpand '(recurse (n 2)
               (fresh-line)
               (if (zerop n)
                   (princ "lift-off!")
                   (progn (princ n)
                          (self (1- n))))))

(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
    `(labels ((self ,(mapcar #'car p) ,@body))
       (self ,@(mapcar #'cdr p)))))

;; -----------------------------------------------------------------------------
;; improve my-length with recurse
;; -----------------------------------------------------------------------------

(defun my-length (l)
  (recurse (lst l acc 0)
    (split lst
           (self tail (1+ acc))
           acc)))
;; -----------------------------------------------------------------------------
;; SVG DSL
;; -----------------------------------------------------------------------------

(use-package :lisp-unit)

;; -----------------------------------------------------------------------------
;; (print-tag)
;; -----------------------------------------------------------------------------
;;
;; use the string-downcase function to lowercase a string

(define-test test-print-tag
  (assert-equal "<mytag color=\"BLUE\" height=\"9\">"
                (with-output-to-string (*standard-output*)
                  (print-tag 'mytag '((color . blue) (height . 9)) nil)))
  (assert-equal "</mytag>"
                (with-output-to-string (*standard-output*)
                  (print-tag 'mytag nil t))))

(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
        alst)
  (princ #\>))

;; -----------------------------------------------------------------------------
;; [tag]
;; -----------------------------------------------------------------------------

(define-test test-tag
  (assert-equal
   "<mytag color=\"BLUE\" height=\"9\"></mytag>"
   (with-output-to-string (*standard-output*)
     (tag mytag (color 'blue height (+ 4 5)))))
  (assert-equal
   "<html meta=\"GOOD-STUFF\" meta2=\"VERY-GOOD-STUFF\"><body script=\"JS\" printable=\"YES\"></body></html>"
   (with-output-to-string (*standard-output*)
     (tag html (meta 'good-stuff meta2 'very-good-stuff)
       (tag body (script 'js printable 'yes))))))

(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                                     (pairs atts)))
                     nil)
          ,@body
          (print-tag ',name nil t)))

;; -----------------------------------------------------------------------------
;; (brightness)
;; -----------------------------------------------------------------------------

(define-test test-brightness
  (assert-equal '(155 23 0)
                (brightness '(255 123 63) -100))
  (assert-equal '(255 123 100)
                (brightness '(156 23 0) 100)))

(defun brightness (col amt)
  (mapcar (lambda (x)
        (min 255 (max 0 (+ x amt))))
          col))

;; -----------------------------------------------------------------------------
;; (svg-style)
;; -----------------------------------------------------------------------------

(define-test test-svg-style
  (assert-equal "fill:rgb(156,64,24);stroke:rgb(56,0,0)"
                (svg-style '(156 64 24))))

(defun svg-style (color)
    (format nil
           "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
            (append color
                    (brightness color -100))))

;; -----------------------------------------------------------------------------
;; (circle)
;; -----------------------------------------------------------------------------

(defun circle (center radius color)
  (tag circle (cx (car center)
                  cy (cdr center)
                  r radius
                  style (svg-style color))))

(define-test test-circle
  (assert-equal "<circle cx=\"50\" cy=\"50\" r=\"50\" style=\"fill:rgb(255,0,0);stroke:rgb(155,0,0)\"></circle>"
                (with-output-to-string (*standard-output*)
                  (circle '(50 . 50) 50 '(255 0 0)))))
;; -----------------------------------------------------------------------------
;; [svg]
;; -----------------------------------------------------------------------------

(defmacro svg (&body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
                   "xmlns:xlink" "http://www.w3.org/1999/xlink")
     ,@body))

(svg (circle '(50 . 50) 50 '(255 0 0)))

(define-test test-svg
  (assert-equal "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><circle cx=\"50\" cy=\"50\" r=\"50\" style=\"fill:rgb(255,0,0);stroke:rgb(155,0,0)\"></circle><circle cx=\"100\" cy=\"100\" r=\"50\" style=\"fill:rgb(0,0,255);stroke:rgb(0,0,155)\"></circle></svg>"
                (with-output-to-string (*standard-output*)
                  (svg (circle '(50 . 50) 50 '(255 0 0))
                       (circle '(100 . 100) 50 '(0 0 255))))))

(macroexpand '(svg (circle '(50 . 50) 50 '(255 0 0))))

(tag svg (xmlns "http://www.w3.org/2000/svg"
                xmlns "http://www.w3.org/1999/xlink")
  (circle '(50 . 50) 50 '(255 0 0)))

;; -----------------------------------------------------------------------------
;; (polygon)
;; -----------------------------------------------------------------------------

(define-test test-polygon
  (assert-equal "<polygon points=\"1,2 3,4 \" style=\"fill:rgb(5,5,5);stroke:rgb(0,0,0)\"></polygon>"
                (with-output-to-string (*standard-output*)
                  (polygon '((1 . 2) (3 . 4)) '(5 5 5)))))


(defun polygon (points color)
  (tag polygon (points (format nil
                               "~{~a,~a ~}"
                               (mapcan (lambda (tp)
                                         (list (car tp) (cdr tp)))
                                       points))
                       style (svg-style color))))

;; -----------------------------------------------------------------------------
;; (random-walk)
;; -----------------------------------------------------------------------------
;; CL-USER> (random-walk 100 10)
;; (100 101 102 103 102 101 102 101 102 103)
;; without loop

(defun random-walk (value length)
  (unless (zerop length)
    (cons value
          (random-walk (if (zerop (random 2))
                           (1- value)
                           (1+ value))
                       (1- length)))))

;; -----------------------------------------------------------------------------
;; Drawing a graph
;; -----------------------------------------------------------------------------

;; - Draw a picture of 10 polygon starting from (0 . 200) to (400 . 200)
;; with 400 random walks in between
;;
;; with random colors
;; write to a file

(with-open-file (*standard-output* "random_walk.svg"
                                   :direction :output
                                   :if-exists :supersede)
  (svg (loop repeat 10
          do (polygon (append '((0 . 200))
                              (loop for x
                                 for y in (random-walk 100 400)
                                 collect (cons x y))
                              '((400 . 200)))
                      (loop repeat 3
                         collect (random 256))))))


