(defpackage :interval
  (:use #:cl)
  (:shadow cl:find cl:delete)
  (:export

   ;; Interval
   #:interval #:interval-start #:interval-end
   #:make-interval
   #:interval< #:interval=

   ;; Interval tree
   #:tree #:make-tree
   #:tree-root #:tree-beforep #:tree-equalp
   #:insert #:delete #:find #:find-all
   #:tree-dump #:sdot-tree #:tree-validate

   ;; Node
   #:node #:node-level #:node-left #:node-right #:node-value
   #:node-max-end))
