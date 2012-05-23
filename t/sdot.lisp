(in-package :interval-test)

(defun sdot-aa-tree (tree &optional (filename "tree.dot"))
  (when tree
    (with-open-file (stream filename :direction :output
                                     :if-exists :supersede)
      (s-dot:s-dot->dot stream
                        `(s-dot::graph () ,@(sdot-node (aa-tree-root tree)
                                                       (gensym "ROOT")))))))

(defun sdot-node (node name)
  (when node
    (alexandria:with-gensyms (left right)
      (let ((name (string name))
            (left (string left))
            (right (string right)))
        `((s-dot::node ((s-dot::id ,name)
                        (s-dot::label
                         ,(format nil "L:~A V:~A"
                                  (node-level node) (node-value node)))))
          ,@(sdot-node (node-left node) left)
          ,@(sdot-node (node-right node) right)
          ,@(when (node-left node)
              `((s-dot::edge ((s-dot::from ,name) (s-dot::to ,left)
                              (s-dot::label "L")))))
          ,@(when (node-right node)
              `((s-dot::edge ((s-dot::from ,name) (s-dot::to ,right)
                              (s-dot::label "R"))))))))))

(check ()
  (sdot-aa-tree *aa*))
