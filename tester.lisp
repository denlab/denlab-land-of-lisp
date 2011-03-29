;; -----------------------------------------------------------------------------
;; Test framework
;; 
;; from : http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html
;; -----------------------------------------------------------------------------

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))

'(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

'(test-+)
;; --------------------------------------------------------------------------------

