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

[The full documentation can be found here.](http://rpav.github.com/cl-interval)
