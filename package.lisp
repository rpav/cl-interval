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
   #:tree-beforep #:tree-equalp #:tree-value-before-p
   #:insert #:delete #:find #:find-all #:find-any
   #:tree-dump #:sdot-tree #:tree-validate))
