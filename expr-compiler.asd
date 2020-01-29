(defsystem "expr-compiler"
  :description "A basic compiler for very basic mathematical operations, based on Graham Hutton's Haskell example (Chapter 16,
                \"Programming in Haskell (2nd Edition)\")"
  :author "Timmy Jose <zoltan.jose@gmail.com>"
  :version "0.0.1"
  :depends-on ("cl-algebraic-data-type")
  :components ((:file "packages")
               (:file "src/compiler" :depends-on ("packages"))
               (:file "src/main" :depends-on ("packages"))))