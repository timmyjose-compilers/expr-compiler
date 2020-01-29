(in-package #:com-tzj-expr-compiler)

;;; data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
(adt:defdata Expr
             (Val integer)
             (Add t t)
	     (Sub t t)
	     (Mul t t)
	     (Div t t))

;;; data Op = OPPUSH Int | OPADD | OPSUB | OPMUL | OPDIV
(adt:defdata Op
             (OPPUSH integer)
             OPADD
	     OPSUB
	     OPMUL
	     OPDIV)

(defun evaluate (expression)
  "Evaluates the given mathematical expression directly, returns the result."
  (adt:match Expr expression
    ((Val n) n)
    ((Add e1 e2) (+ (evaluate e1) (evaluate e2)))
    ((Sub e1 e2) (- (evaluate e1) (evaluate e2)))
    ((Mul e1 e2) (* (evaluate e1) (evaluate e2)))
    ((Div e1 e2) (let ((e2val (evaluate e2)))
		   (if (zerop e2val)
		       0
		       (/ (evaluate e1) e2val))))))

(defun comp (expression)
  "Compiles the expression into code, returns the stack of instructions (list of Op)."
  (adt:match Expr expression
             ((Val n) (list (OPPUSH n)))
             ((Add e1 e2) (append (comp e1)
                                  (comp e2)
                                  (list OPADD)))
	     ((Sub e1 e2) (append (comp e1)
				  (comp e2)
				  (list OPSUB)))
	     ((Mul e1 e2) (append (comp e1)
				  (comp e2)
				  (list OPMUL)))
	     ((Div e1 e2) (append (comp e1)
				  (comp e2)
				  (list OPDIV)))))

;;; Due to Peter Seibel, "Practical Common Lisp"
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for name in names collecting `(,name (gensym)))
     ,@body))

(defmacro execute-operation (operation instructions accumulator)
  (with-gensyms (op1 op2)
    `(let ((,op2 (pop ,accumulator))
	   (,op1 (pop ,accumulator)))
       (if (eq ,operation #'/)
	   (if (zerop ,op2)
	       '(0)
	       (execute (cdr ,instructions) (push (funcall ,operation ,op1 ,op2) ,accumulator)))
	   (execute (cdr ,instructions) (push (funcall ,operation ,op1 ,op2) ,accumulator))))))

(defun execute (instructions accumulator)
  "Executes the instructions (list of Op) and stores the result in the accumulator."
  (cond
    ((null instructions) accumulator)
    (t (let ((instr (car instructions)))
         (adt:match Op instr
	   ((OPPUSH n) (execute (cdr instructions) (push n accumulator)))
	   ((OPADD) (execute-operation #'+ instructions accumulator))
	   ((OPSUB) (execute-operation #'- instructions accumulator))
	   ((OPMUL) (execute-operation #'* instructions accumulator))
	   ((OPDIV) (execute-operation #'/ instructions accumulator)))))))
