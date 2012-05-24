# cl-interval

This is a simple, efficient implementation of intervals and interval
trees for Common Lisp.  This is useful if you wish to have a lot of
`start-end` pairs, and find all overlapping intervals at a point, or
within an interval:

```lisp
(defvar *tree* (interval:make-tree))

(interval:insert *tree* '(1 . 1))
(interval:insert *tree* '(1 . 3))
(interval:insert *tree* '(2 . 4))
(interval:insert *tree* '(5 . 9))

(interval:find *tree* '(1 . 1)) ;; => #<1-1>
(interval:find *tree* '(1 . 2)) ;; => NIL

(interval:find-all *tree* '(1 . 1))
   ;; => (#<1-1> #<1-3>)

;;; Equivalent to:
(interval:find-all *tree* 1)
   ;; => (#<1-1> #<1-3>)

(interval:find-all *tree* '(1 . 2))
   ;; => (#<1-1> #<1-3> #<2-4>)
```

The implementation for this is based on an augmented AA tree as
discussed in the following:

* Cormen, Thomas H.; Leiserson, Charles E., Rivest, Ronald
  L. (1990). Introduction to Algorithms (1st ed.). MIT Press and
  McGraw-Hill. ISBN 0-262-03141-8.

A short summary of this method can (at the time of writing) be found
[on wikipedia](http://en.wikipedia.org/wiki/Interval_tree#Augmented_tree).

# Extending Intervals

Plain numeric intervals are not terribly useful on their own.  Thus,
intervals are represented in the structure `interval:interval`, which
you can extend as you please:

```lisp
(defstruct (my-interval (:include interval:interval))
  my-data)

(defun my-interval< (i1 i2)
  (char< (interval:interval-start i1) (interval:interval-start i2)))

(defun my-interval= (i1 i2)
  (and (char= (interval:interval-start i1) (interval:interval-start i2))
       (char= (interval:interval-end i1) (interval:interval-end i2))))

(setf *mytree* (interval:make-tree :interval-before-p #'my-interval<
                                   :interval-equal-p #'my-interval=
                                   :value-before-p #'char<=))

(interval:insert *mytree* (make-my-interval :start #\a :end #\c))
(interval:insert *mytree* (make-my-interval :start #\b :end #\m))

(interval:find *mytree* '(#\a . #\c))
  ;; => (#<a-c>)

(interval:find-all *mytree* #\b)  
  ;; => (#<a-c> #<b-m>)

(setf (my-interval-my-data (interval:find *mytree* '(#\a . #\c)))
      "something useful for later retrieval")

(mapcar #'my-interval-my-data
        (interval:find-all *mytree* #\b))
  ;; => ("something useful for later retrieval" NIL)
```

As you can see in this example, we've made a new interval which
compares alphabetically, and where we can attach data.

One thing of note is that `INTERVAL:INTERVAL-START` and
`INTERVAL:INTERVAL-END` are used instead of `MY-INTERVAL-START` and
`MY-INTERVAL-END`.  This is not strictly necessary; however, you will
not be able to search or delete based on "plain" intervals, or use the
`(START . END)` or single-value syntax.
