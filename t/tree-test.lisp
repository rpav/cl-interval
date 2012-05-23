(in-package :interval-test)

(defvar *aa*)

(defmethod checkl:result-translate ((result interval:aa-tree))
  (interval:aa-tree-dump result))

(defmethod checkl:result-translate ((result interval:aa-node))
  (interval:node-value result))

(check (:name :make-aa-tree)
  (setf *aa* (make-aa-tree :beforep #'<
                           :equalp #'=))
  (results *aa*))

(check (:name :basic-insertion)
  (run :make-aa-tree)
  (dotimes (i 100)
    (interval:aa-tree-insert *aa* i))
  (results *aa*))

(check (:name :basic-find)
  (run :basic-insertion)
  (results
   (aa-tree-find *aa* 1)
   (aa-tree-find *aa* 7)
   (aa-tree-find *aa* 15)
   (aa-tree-find *aa* 37)
   (aa-tree-find *aa* 92)
   (aa-tree-find *aa* 100)))

(check (:name :validate)
  (results
   (aa-tree-validate *aa*)))

(check (:name :delete)
  (run :basic-insertion)
  (dotimes (i 500)
    (let ((n (random 100)))
      (:say "delete ~A" n)
      (aa-tree-delete *aa* n)
      (aa-tree-validate *aa*)))
  (results))

(check ()
 (time
  (progn
    (run :make-aa-tree)
    (dotimes (i 100000)
      (aa-tree-insert *aa* i))
    (dotimes (i 100000)
      (aa-tree-find *aa* i))
    (dotimes (i 100000)
      (aa-tree-delete *aa* i)))))
