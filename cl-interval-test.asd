(cl:eval-when (:load-toplevel :execute)
  (asdf:load-system :checkl))

(defsystem :cl-interval-test
  :depends-on (#:cl-interval #:checkl #:alexandria #:s-dot)

  :pathname "t"
  :serial t
  :components ((:file "package")
               (checkl:test-values "results.ms"
                                   :package :interval-test)
               (checkl:tests "tree-test")
               (checkl:tests "sdot")))

