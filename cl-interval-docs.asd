(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cl-gendoc))

(defsystem :cl-interval-docs
  :depends-on (:cl-gendoc :cl-interval)

  :serial t
  :pathname "doc"

  :components
  ((:file "generate")))

(gendoc:define-gendoc-load-op :cl-interval-docs :interval.docs 'generate)
