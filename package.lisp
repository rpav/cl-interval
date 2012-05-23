(defpackage :interval
  (:use #:cl)
  (:export

   ;; AA tree
   #:aa-tree #:make-aa-tree
   #:aa-tree-root #:aa-tree-beforep #:aa-tree-equalp
   #:aa-tree-insert #:aa-tree-delete #:aa-tree-find
   #:aa-tree-dump #:sdot-aa-tree #:aa-tree-validate

   ;; AA node
   #:aa-node #:node-level #:node-left #:node-right #:node-value))
