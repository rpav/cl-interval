(in-package :interval)

(defstruct (tree (:constructor %make-tree)
                 (:copier %copy-tree))
  (root nil :type (or null node))
  (beforep nil :type function :read-only t)
  (equalp nil :type function :read-only t)
  (value-before-p nil :type function :read-only t))

(setf (documentation 'tree-beforep 'function)
      "=> FUNCTION
Return the function used to compare *intervals*.  It should return
whether one interval `START` comes before another interval `START`.")

(setf (documentation 'tree-equalp 'function)
      "=> FUNCTION
Return the function used to compare *intervals*.  It should return
whether one interval is equal to another.  It should not be an identity
comparison (i.e., not `EQ`).")

(setf (documentation 'tree-value-before-p 'function)
      "=> FUNCTION
Return the function used to compare *values* (i.e., start and end).
For closed intervals, use a less-than-or-equal function.  For open
intervals, use a less-than function.  Half-open intervals are currently
not supported.")

(defun make-tree (&key (interval-before-p 'interval<)
                  (interval-equal-p 'interval=)
                  (value-before-p '<=))
  "=> INTERVAL:TREE

Make an interval tree given the specified functions.  By default,
these are simple numeric comparisons.

`INTERVAL-BEFORE-P` should take two intervals, `A` and `B`, and test
whether the *start* of `A` comes before the *start* of `B`.  This is
used solely for tree placement.  `A` and `B` might be equal, but the
test may be a less-than-not-equal test.

`INTERVAL-EQUAL-P` should take two intervals, `A` and `B`, and test
whether they are equal (e.g., the starts and ends are the same).  This
should *not* be an identity test (i.e., not `EQ`).

`VALUE-BEFORE-P` should take two *values*, `A` and `B`, which are
used as start or end values, and compare whether `A` comes before `B`.
For closed intervals, use a less-than-or-equal function.  For open
intervals, use a less-than function.  Half-open intervals are currently
not supported."
  (%make-tree :beforep (coerce interval-before-p 'function)
              :equalp (coerce interval-equal-p 'function)
              :value-before-p (coerce value-before-p 'function)))

(defun coerce-interval-designator (designator allow-single-value-interval)
  (typecase designator
    (interval designator)
    ((cons atom atom)
     (make-interval :start (car designator) :end (cdr designator)))
    (t
     (if allow-single-value-interval
         (make-interval :start designator :end designator)
         (error "Expected an INTERVAL or a CONS of start and end value, but got: ~S"
                designator)))))

(defun insert (tree interval)
  "=> interval, inserted-p

Insert `INTERVAL` into `TREE`, if an equivalent interval (by
interval-equal-p) is not already in `TREE`, returning `INTERVAL`.
Otherwise, return the existing interval.

`INSERTED-P` is `T` if there was no existing interval, or `NIL` if
the existing interval was returned.

`INTERVAL` may alternatively be a cons in the form `(START . END)`.
In this case, a simple interval is created and inserted."
  (declare (type tree tree)
           (type (or cons interval) interval))
  (let ((interval (coerce-interval-designator interval nil)))
    (multiple-value-bind (node foundp)
        (node-insert tree (tree-root tree) interval)
      (if foundp
          (values (node-value node) nil)
          (progn
            (setf (tree-root tree) node)
            (values interval t))))))

(defun delete (tree interval)
  "=> INTERVAL, deleted-p

Delete an interval that is interval-equal-p to `INTERVAL` from `TREE`.

`INTERVAL` may be any type of interval, or a cons in the form `(START
. END)`."
  (declare (type tree tree)
           (type (or cons interval) interval))
  (let ((interval (coerce-interval-designator interval nil)))
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
  (let ((interval (coerce-interval-designator interval nil)))
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
  (let ((interval (coerce-interval-designator interval t)))
    (mapcar #'node-value (node-find-all tree (tree-root tree) interval))))

(defun find-any (tree interval)
  "=> interval-in-tree or NIL

Find any one interval in `TREE` intersecting `INTERVAL`.

Alternatively, `INTERVAL` may be either a cons of `(START . END)`, or
a single value, which will be used as both the start and the
end (effectively finding intervals at a point)."
  (let ((interval (coerce-interval-designator interval t)))
    (node-find-any tree (tree-root tree) interval)))

(defun tree-validate (tree)
  "=> T
Tests `TREE` for AA-tree and interval-tree invariants, to make sure the
tree is valid.  It returns `T`, or raises an error if the invariants are
not met."
  (node-validate (tree-root tree)))

(defun tree-dump (tree)
  "=> list
Return a tree dumped into a list form.  This is currently only useful for
testing."
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
          (list (interval-start (node-value node))
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
          (setf (node-left node) node0
		node-found-p t)
          (if-node-fun ((node-right node) #'node-delete)
            (setf (node-rightt node) node0
		  node-found-p t)
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

(defun node-in-range-p (v<-fun node interval)
  (funcall v<-fun (interval-start interval) (node-max-end node)))

(defun node-strictly-greater-p (v<-fun node interval)
  (funcall v<-fun (interval-end interval) (interval-start (node-value node))))

(defun node-find-all (tree node interval)
  (when node
    (let* ((v< (tree-value-before-p tree))
           (strictly-greater-p (node-strictly-greater-p v< node interval))
           (in-range-p (node-in-range-p v< node interval)))
      (let ((current
              (when (interval-intersects v< (node-value node) interval)
                node))
            (left
              (when in-range-p
                (node-find-all tree (node-left node) interval)))
            (right
              (when (and in-range-p (not strictly-greater-p))
                (node-find-all tree (node-right node) interval))))
        (cond ((and left right)
               (append left (and current (list current)) right))
              (left
               (if current
                   (append left (list current))
                   left))
              (right
               (if current
                   (cons current right)
                   right))
              (current
               (list current)))))))

(defun node-find-any (tree node interval)
  (when node
    (let* ((v< (tree-value-before-p tree))
           (strictly-greater-p (node-strictly-greater-p v< node interval))
           (in-range-p (node-in-range-p v< node interval)))
      (or (when (interval-intersects v< (node-value node) interval)
            node)
          (when in-range-p
            (node-find-any tree (node-left node) interval))
          (when (and in-range-p (not strictly-greater-p))
            (node-find-any tree (node-right node) interval))))))
