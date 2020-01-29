(in-package #:com-tzj-expr-compiler)

;;; some sample expressions for verification/testing
;;; note that the equation execute (comp expression) '() = (list (evaluate expression))
;;; must (and will) hold for any such expression
;;;
;;; Also note the read versions for the following expressions are as follows:
;;;
;;; #.(Add #.(Val 1) #.(Val 2))
;;;
;;; #. (Add #.(Add #.(Val 1) #.(Val 2)) #.(Add #.(Val 3) #.(Val 4)))
;;;
;;; and so on.

(defvar *e1* (Add (Val 1) (Val 2))) ; 
(defvar *e2* (Add (Add (Val 1) (Val 2)) (Add (Val 3) (Val 4))))
(defvar *e3* (Mul (Sub (Val 10) (Val 2)) (Add (Val 1) (Val 2))))
(defvar *e4* (Div (Add (Sub (Val 9) (Val 8)) (Val 100)) (Val 2)))
(defvar *e5* (Div (Add (Val 1) (Val 3)) (Sub (Mul (Val 2) (Val 1)) (Div (Val 4) (Val 2)))))

(defun test ()
  (let ((expressions (list *e1* *e2* *e3* *e4* *e5*)))
    (loop for expression in expressions
       do (assert (= (evaluate expression) (car (execute (comp expression) '()))))
	 (format t "~a has the value: ~a~%" expression (execute (comp expression) '())))))

(defun main()
  (let* ((expression (read))
	 (eval-result (evaluate expression))
	 (exec-result (execute (comp expression) '())))
    (assert (= eval-result (car exec-result)))
    (format t "For the exrpression ~a~%result of eval = ~a~%result of exec = ~a~%"
	    expression
	    eval-result
	    exec-result)))
