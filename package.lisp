(defpackage :interval
  (:use #:cl)
  (:export

   ;; Interval tree
   #:tree #:make-tree
   #:tree-root #:tree-beforep #:tree-equalp
   #:tree-insert #:tree-delete #:tree-find
   #:tree-dump #:sdot-tree #:tree-validate

   ;; AA node
   #:node #:node-level #:node-left #:node-right #:node-value))
