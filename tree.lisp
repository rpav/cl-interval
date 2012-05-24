(in-package :interval)

(defstruct (tree (:constructor %make-tree)
                 (:copier %copy-tree))
  (root nil :type (or null node))
  (beforep nil :type function)
  (equalp nil :type function)
  (value-before-p nil :type function))

(defun make-tree (&key (interval-before-p 'interval<)
                  (interval-equal-p 'interval=)
                  (value-before-p '<=))
  (%make-tree :beforep (coerce interval-before-p 'function)
              :equalp (coerce interval-equal-p 'function)
              :value-before-p (coerce value-before-p 'function)))

(defun insert (tree interval)
  "=> interval, inserted-p

Insert `INTERVAL` into `TREE`, if an equivalent interval (by
interval-equal-p) is not already in `TREE`, returning `INTERVAL`.
Otherwise, return the existing interval.

`INSERTED-P` is `T` if there was no existing interval, or `NIL` if
the existing interval was returned."
  (declare (type tree tree))
  (multiple-value-bind (node foundp)
      (node-insert tree (tree-root tree) interval)
    (if foundp
        (values (node-value node) nil)
        (progn
          (setf (tree-root tree) node)
          (values interval t)))))

(defun delete (tree interval)
  "=> INTERVAL, deleted-p

Delete an interval that is interval-equal-p to `INTERVAL` from `TREE`.

`INTERVAL` may be any type of interval, or a cons in the form `(START
. END)`."
  (declare (type tree tree)
           (type (or cons interval) interval))
  (let ((interval (etypecase interval
                    (interval interval)
                    (cons (make-interval :start (car interval)
                                         :end (cdr interval))))))
    (multiple-value-bind (node foundp)
        (node-delete tree (tree-root tree) interval)
      (when foundp (setf (tree-root tree) node))
      (values interval foundp))))

(defun find (tree interval)
  "=> interval-in-tree or NIL

Find a specific interval that is :interval-equal-p to `INTERVAL` in
`TREE` and return it, or NIL.

`INTERVAL` may be any type of interval, or a cons in the form `(START
. END)`."
  (declare (type tree tree)
           (type (or cons interval) interval))
  (let ((interval (etypecase interval
                    (interval interval)
                    (cons (make-interval :start (car interval)
                                         :end (cdr interval))))))
    (multiple-value-bind (node foundp)
        (node-find tree (tree-root tree) interval)
      (and foundp (node-value node)))))

(defun find-all (tree interval)
  "=> list-of-intervals or NIL

Find all intervals intersecting `INTERVAL` in `TREE`.  `INTERVAL` does
not have to be matched exactly in `TREE`.

Alternatively, `INTERVAL` may be either a cons of `(START . END)`, or
a single value, which will be used as both the start and the
end (effectively finding intervals at a point)."
  (let ((interval (typecase interval
                    (interval interval)
                    (cons
                     (make-interval :start (car interval)
                                    :end (cdr interval)))
                    (t (make-interval :start interval :end interval)))))
    (mapcar #'node-value
            (node-find-all tree (tree-root tree) interval))))

(defun tree-validate (tree)
  (node-validate (tree-root tree)))

(defun tree-dump (tree)
  (when tree (node-dump (tree-root tree))))

(defstruct (node (:conc-name node-))
  (level 0 :type (unsigned-byte 16))
  (left nil :type (or null node))
  (right nil :type (or null node))
  (value nil :type interval)
  (max-end nil :type t))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "L: ~A, V: ~A"
            (node-level object) (node-value object))))

(defun node-dump (node)
  (when node
    (list (node-level node)
          (cons (interval-start (node-value node))
                (interval-end (node-value node)))
          (node-max-end node)
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

(defun node-skew (tree node)
  (declare (type (or null node) node))
  (if (and node (node-left-level-p node))
      (let ((left-node (node-left node)))
        (psetf (node-right (node-left node)) node
               (node-left node) (node-right (node-left node)))
        (node-update-max tree node)
        (node-update-max tree left-node)
        left-node)
      node))

(defun node-split (tree node)
  (declare (type (or null node) node))
  (if (and node (node-two-rights-level-p node))
      (let ((right-node (node-right node)))
        (incf (node-level right-node))
        (psetf (node-left (node-right node)) node
               (node-right node) (node-left (node-right node)))
        (node-update-max tree node)
        (node-update-max tree right-node)
        right-node)
      node))

(defun node-update-max (tree node)
  (let ((value-before-p (tree-value-before-p tree))
        (node-end (interval-end (node-value node)))
        (left-max (and (node-left node) (node-max-end (node-left node))))
        (right-max (and (node-right node) (node-max-end (node-right node)))))
    (labels ((v< (v1 v2) (funcall value-before-p v1 v2))
             (vmax (&rest values)
               (reduce (lambda (a b) (if (v< a b) b a)) values)))
      (cond
        ((and left-max right-max)
         (setf (node-max-end node) (vmax node-end left-max right-max)))
        (left-max
         (setf (node-max-end node) (vmax node-end left-max)))
        (right-max
         (setf (node-max-end node) (vmax node-end right-max)))
        (t (setf (node-max-end node) (interval-end (node-value node))))))
    node))

(defmacro if-node-fun ((node function) then &optional else)
  `(multiple-value-bind (node0 foundp)
       (funcall ,function tree ,node value)
     (declare (ignorable node0 foundp))
     (if foundp ,then ,else)))

(defun node-insert (tree node value)
  (declare (type tree tree)
           (type (or null node) node))
  (let ((beforep (tree-beforep tree))
        (equalp (tree-equalp tree)))
    (cond
      ((null node)
       (return-from node-insert
         (values
          (make-node :level 1 :value value :max-end (interval-end value))
          nil)))
      ((funcall beforep value (node-value node))
       (if-node-fun ((node-left node) #'node-insert)
         (return-from node-insert (values node0 t))
         (setf (node-left node) node0)))
      ((funcall equalp value (node-value node))
       (return-from node-insert (values node t)))
      (t
       (if-node-fun ((node-right node) #'node-insert)
         (return-from node-insert (values node0 t))
         (setf (node-right node) node0))))
    (node-update-max tree node)
    (values (node-split tree (node-skew tree node)) nil)))

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

(defun node-delete (tree node value)
  (declare (type tree tree)
           (type (or null node) node)
           (type interval value))
  (let ((beforep (tree-beforep tree))
        (equalp (tree-equalp tree)))
   (let (node-found-p)
     (cond
       ((null node)
        (return-from node-delete
          (values nil nil)))
       ((funcall beforep value (node-value node))
        (if-node-fun ((node-left node) #'node-delete)
          (progn
            (setf node-found-p foundp)
            (setf (node-left node) node0)
            (node-update-max tree node))))
       ((funcall beforep (node-value node) value)
        (if-node-fun ((node-right node) #'node-delete)
          (progn
            (setf node-found-p foundp)
            (setf (node-right node) node0)
            (node-update-max tree node))))
       ((funcall equalp (node-value node) value)
        (setf node-found-p t)
        (cond
          ((node-leaf-p node)
           (return-from node-delete (values nil t)))
          ((null (node-left node))
           (let ((successor (node-successor node)))
             (setf (node-right node) (node-delete tree
                                                  (node-right node)
                                                  (node-value successor)))
             (setf (node-value node) (node-value successor))))
          (t
           (let ((predecessor (node-predecessor node)))
             (setf (node-left node) (node-delete tree (node-left node)
                                                 (node-value predecessor)))
             (setf (node-value node) (node-value predecessor))))))
       (t
        (if-node-fun ((node-left node) #'node-delete)
          (setf node-found-p t)
          (if-node-fun ((node-right node) #'node-delete)
            (setf node-found-p t)
            (return-from node-delete (values nil nil))))))
     (if (node-leaf-p node)
         (values node node-found-p)
         (let* ((node (node-skew tree (node-decrease-level node)))
                (right (node-skew tree (node-right node))))
           (setf (node-right node) right)
           (setf (node-right right) (node-skew tree (node-right right)))
           (setf node (node-split tree node))
           (setf (node-right node) (node-split tree (node-right node)))
           (values node node-found-p))))))

(defun node-validate (node &optional prev)
  (flet ((leaf-invariants (node)
           (assert (= 1 (node-level node)))
           (assert (= (node-max-end node)
                      (interval-end (node-value node)))))
         (level-1-invariants (node)
           (assert (null (node-left node))))
         (level->1-invariants (node)
           (assert (and (node-left node)
                        (node-right node))))
         (left-invariants (node)
           (assert (= (1+ (node-level (node-left node)))
                      (node-level node)))
           (assert (>= (node-max-end node)
                       (node-max-end (node-left node)))))
         (right-invariants (node prev)
           (assert (or (= (node-level (node-right node))
                          (node-level node))
                       (= (1+ (node-level (node-right node)))
                          (node-level node))))
           (assert (>= (node-max-end node)
                       (node-max-end (node-right node))))
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

(defun node-find (tree node value)
  (declare (type tree tree)
           (type (or null node) node))
  (let ((beforep (tree-beforep tree))
        (equalp (tree-equalp tree)))
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
           (values nil nil)))))))

(defun node-find-all (tree node interval)
  (when node
    (let ((v< (tree-value-before-p tree)))
      (concatenate 'list
        (when (funcall v< (interval-start interval) (node-max-end node))
          (node-find-all tree (node-left node) interval))
        (when (interval-intersects v< (node-value node) interval)
          (list node))
        (when (funcall v< (interval-start (node-value node))
                          (interval-start interval))
          (node-find-all tree (node-right node) interval))))))
