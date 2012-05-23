(in-package :interval)

(defstruct (tree (:constructor %make-tree)
                 (:copier %copy-tree))
  (root nil :type (or null node))
  (beforep nil :type function)
  (equalp nil :type function))

(defun tree-insert (tree value)
  (declare (type tree tree))
  (if (tree-root tree)
      (setf (tree-root tree)
            (node-insert (tree-root tree) value
                         (tree-beforep tree)))
      (setf (tree-root tree)
            (make-node :level 1 :value value)))
  tree)

(defun tree-delete (tree value)
  (declare (type tree tree))
  (multiple-value-bind (node foundp)
      (node-delete (tree-root tree) value
                   (tree-beforep tree)
                   (tree-equalp tree))
    (when foundp (setf (tree-root tree) node))
    (values (and foundp value) foundp)))

(defun tree-find (tree value)
  (declare (type tree tree))
  (multiple-value-bind (node foundp)
      (node-find (tree-root tree) value
                 (tree-beforep tree)
                 (tree-equalp tree))
    (values (and foundp node) foundp)))

(defun tree-validate (tree)
  (node-validate (tree-root tree)))

(defun tree-dump (tree)
  (when tree (node-dump (tree-root tree))))

(defstruct (node (:conc-name node-))
  (level 0 :type (unsigned-byte 16))
  (left nil :type (or null node))
  (right nil :type (or null node))
  (value nil :type t))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "L: ~A, V: ~A"
            (node-level object) (node-value object))))

(defun node-dump (node)
  (when node
    (list (node-level node) (node-value node)
          (node-dump (node-left node))
          (node-dump (node-right node)))))

(declaim (inline node-left-level-p node-two-rights-level-p))
(defun node-left-level-p (node)
  (declare (type node node))
  (and node (node-left node)
       (= (node-level node)
          (node-level (node-left node)))))

(defun node-two-rights-level-p (node)
  (declare (type node node))
  (and node (node-right node) (node-right (node-right node))
       (= (node-level node)
          (node-level (node-right (node-right node))))))

(defun node-skew (node)
  (declare (type (or null node) node))
  (if (and node (node-left-level-p node))
      (prog1
          (node-left node)
        (psetf (node-right (node-left node)) node
               (node-left node) (node-right (node-left node))))
      node))

(defun node-split (node)
  (declare (type (or null node) node))
  (if (and node (node-two-rights-level-p node))
      (prog2
          (incf (node-level (node-right node)))
          (node-right node)
        (psetf (node-left (node-right node)) node
               (node-right node) (node-left (node-right node))))
      node))

(defun node-insert (node value beforep)
  (declare (type (or null node) node)
           (type function beforep))
  (cond
    ((null node)
     (return-from node-insert
       (make-node :level 1 :value value)))
    ((funcall beforep value (node-value node))
     (setf (node-left node) (node-insert (node-left node) value beforep)))
    (t
     (setf (node-right node) (node-insert (node-right node) value beforep))))

  (node-split (node-skew node)))

(declaim (inline node-leaf-p))
(defun node-leaf-p (node)
  (and (null (node-left node))
       (null (node-right node))))

(defun node-leftmost-child (node)
  (if (and node (node-left node))
      (node-leftmost-child (node-left node))
      node))

(defun node-rightmost-child (node)
  (if (and node (node-right node))
      (node-rightmost-child (node-right node))
      node))

(defun node-successor (node)
  (when node
    (node-leftmost-child (node-right node))))

(defun node-predecessor (node)
  (when node
    (node-rightmost-child (node-left node))))

(defun node-decrease-level (node)
  (let* ((left-level (if (node-left node) (node-level (node-left node)) 0))
         (right-level (if (node-right node) (node-level (node-right node)) 0))
         (target-level (1+ (min left-level right-level))))
    (when (< target-level (node-level node))
      (setf (node-level node) target-level)
      (when (< target-level right-level)
        (setf (node-level (node-right node)) target-level)))
    node))

(defmacro if-node-fun ((node function) then &optional else)
  `(multiple-value-bind (node0 foundp)
       (funcall ,function ,node value beforep equalp)
     (declare (ignorable node0 foundp))
     (if foundp ,then ,else)))

(defun node-delete (node value beforep equalp)
  (declare (type (or null node) node)
           (type function beforep))
  (let (node-found-p)
    (cond
      ((null node)
       (return-from node-delete
         (values nil nil)))
      ((funcall beforep value (node-value node))
       (if-node-fun ((node-left node) #'node-delete)
         (progn
           (setf node-found-p foundp)
           (setf (node-left node) node0))))
      ((funcall beforep (node-value node) value)
       (if-node-fun ((node-right node) #'node-delete)
         (progn
           (setf node-found-p foundp)
           (setf (node-right node) node0))))
      ((funcall equalp (node-value node) value)
       (setf node-found-p t)
       (cond
         ((node-leaf-p node)
          (return-from node-delete (values nil t)))
         ((null (node-left node))
          (let ((successor (node-successor node)))
            (setf (node-right node) (node-delete (node-right node)
                                                 (node-value successor)
                                                 beforep equalp))
            (setf (node-value node) (node-value successor))))
         (t
          (let ((predecessor (node-predecessor node)))
            (setf (node-left node) (node-delete (node-left node)
                                                (node-value predecessor)
                                                beforep equalp))
            (setf (node-value node) (node-value predecessor))))))
      (t
       (if-node-fun ((node-left node) #'node-delete)
         (setf node-found-p t)
         (if-node-fun ((node-right node) #'node-delete)
           (setf node-found-p t)
           (return-from node-delete (values nil nil))))))
    (if (node-leaf-p node)
        (values node node-found-p)
        (let* ((node (node-skew (node-decrease-level node)))
               (right (node-skew (node-right node))))
          (setf (node-right node) right)
          (setf (node-right right) (node-skew (node-right right)))
          (setf node (node-split node))
          (setf (node-right node) (node-split (node-right node)))
          (values node node-found-p)))))

(defun node-validate (node &optional prev)
  (flet ((leaf-invariants (node)
           (= 1 (node-level node)))
         (level-1-invariants (node)
           (assert (null (node-left node))))
         (level->1-invariants (node)
           (assert (and (node-left node)
                        (node-right node))))
         (left-invariants (node)
           (assert (= (1+ (node-level (node-left node)))
                      (node-level node))))
         (right-invariants (node prev)
           (assert (or (= (node-level (node-right node))
                          (node-level node))
                       (= (1+ (node-level (node-right node)))
                          (node-level node))))
           (when prev
             (assert (< (node-level prev) (node-level (node-right node)))))))
    (cond
      ((null node) (return-from node-validate))
      ((node-leaf-p node) (leaf-invariants node))
      ((= 1 (node-level node))
       (level-1-invariants node)
       (right-invariants node prev))
      ((> (node-level node) 1)
       (level->1-invariants node)
       (left-invariants node)
       (right-invariants node prev)))
    (node-validate (node-left node))
    (node-validate (node-right node))
    t))

(defun node-find (node value beforep equalp)
  (declare (type (or null node) node)
           (type function beforep equalp))
  (cond
    ((null node) (return-from node-find (values nil nil)))
    ((funcall beforep value (node-value node))
     (if-node-fun ((node-left node) #'node-find)
       (values node0 foundp)))
    ((funcall beforep (node-value node) value)
     (if-node-fun ((node-right node) #'node-find)
       (values node0 foundp)))
    ((funcall equalp (node-value node) value)
     (values node t))
    (t
     (if-node-fun ((node-left node) #'node-find)
       (values node0 t)
       (if-node-fun ((node-right node) #'node-find)
         (values node0 t)
         (values nil nil))))))
