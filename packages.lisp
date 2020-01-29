(in-package #:common-lisp-user)

(defpackage #:com-tzj-expr-compiler
  (:use #:common-lisp
        #:cl-algebraic-data-type)
  (:export #:eval #:exec))
