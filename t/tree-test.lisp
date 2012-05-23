(in-package :interval-test)

(defvar *aa*)

(defmethod checkl:result-translate ((result interval:tree))
  (interval:tree-dump result))

(defmethod checkl:result-translate ((result interval:node))
  (interval:node-value result))

(check (:name :make-tree)
  (setf *aa* (make-tree :beforep #'<
                           :equalp #'=))
  (results *aa*))

(check (:name :basic-insertion)
  (run :make-tree)
  (dotimes (i 100)
    (interval:tree-insert *aa* i))
  (results *aa*))

(check (:name :basic-find)
  (run :basic-insertion)
  (results
   (tree-find *aa* 1)
   (tree-find *aa* 7)
   (tree-find *aa* 15)
   (tree-find *aa* 37)
   (tree-find *aa* 92)
   (tree-find *aa* 100)))

(check (:name :validate)
  (results
   (tree-validate *aa*)))

(check (:name :delete)
  (run :basic-insertion)
  (dotimes (i 500)
    (let ((n (random 100)))
      (:say "delete ~A" n)
      (tree-delete *aa* n)
      (tree-validate *aa*)))
  (results))

(check ()
 (time
  (progn
    (run :make-tree)
    (dotimes (i 100000)
      (tree-insert *aa* i))
    (dotimes (i 100000)
      (tree-find *aa* i))
    (dotimes (i 100000)
      (tree-delete *aa* i)))))
