(asdf:defsystem :cl-interval
  :description "Intervals, interval trees"
  :author "Ryan Pavlik"
  :license "NewBSD, LLGPL"

  :serial t

  :components
  ((:file "package")
   (:file "interval")
   (:file "tree")))
