(in-package :interval-test)

(defun sdot-aa-tree (tree &optional (filename "tree.dot"))
  (when tree
    (with-open-file (stream filename :direction :output
                                     :if-exists :supersede)
      (s-dot:s-dot->dot stream
                        `(s-dot::graph () ,@(sdot-node (interval::tree-root tree)
                                                       (gensym "ROOT")))))))

(defun sdot-node (node name)
  (when node
    (alexandria:with-gensyms (left right)
      (let ((name (string name))
            (left (string left))
            (right (string right)))
        `((s-dot::node ((s-dot::id ,name)
                        (s-dot::label
                         ,(format nil "[L:~A] ~A-~A Max: ~A"
                                  (interval::node-level node)
                                  (interval:interval-start
                                   (interval::node-value node))
                                  (interval:interval-end
                                   (interval::node-value node))
                                  (interval::node-max-end node)))))
          ,@(sdot-node (interval::node-left node) left)
          ,@(sdot-node (interval::node-right node) right)
          ,@(when (interval::node-left node)
              `((s-dot::edge ((s-dot::from ,name) (s-dot::to ,left)
                              (s-dot::label "L")))))
          ,@(when (interval::node-right node)
              `((s-dot::edge ((s-dot::from ,name) (s-dot::to ,right)
                              (s-dot::label "R"))))))))))

(check ()
  (sdot-aa-tree *aa*))
