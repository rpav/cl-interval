(in-package :interval-test)

(defvar *aa*)

(defmethod checkl:result-translate ((result interval:tree))
  (interval:tree-dump result))

(defmethod checkl:result-translate ((node interval::node))
  (list (interval:interval-start (interval::node-value node))
        (interval:interval-end (interval::node-value node))))

(defmethod checkl:result-translate ((result interval:interval))
  (list (interval:interval-start result)
        (interval:interval-end result)))

(defun mi (list)
  (mapcar #'checkl:result-translate list))

(check (:name :make-tree)
  (setf *aa* (interval:make-tree))
  (results *aa*))

(check (:name :basic-insertion)
  (run :make-tree)
  (dotimes (i 100)
    (interval:insert *aa* (interval:make-interval :start i :end (+ i i))))
  (results *aa*))

(check (:name :find-all)
  (run :basic-insertion)
  (results
   (mi (interval:find-all *aa* 1))
   (mi (interval:find-all *aa* 2))
   (mi (interval:find-all *aa* 4))
   (mi (interval:find-all *aa* 50))
   (mi (interval:find-all *aa* 80))
   (mi (interval:find-all *aa* 90))))

(check (:name :find-all-2)
  (run :basic-insertion)
  (results
   (mi (interval:find-all *aa* '(0 . 5)))
   (mi (interval:find-all *aa* '(10 . 20)))
   (mi (interval:find-all *aa* '(20 . 25)))))

(check (:name :basic-find)
  (run :basic-insertion)
  (results
   (interval:find *aa* '(1 . 2))
   (interval:find *aa* '(7 . 14))
   (interval:find *aa* '(15 . 16))
   (interval:find *aa* '(15 . 30))
   (interval:find *aa* '(92 . 184))
   (interval:find *aa* '(100 . 200))))

(check (:name :validate)
  (results
   (interval:tree-validate *aa*)))

(check (:name :delete)
  (run :basic-insertion)
  (dotimes (i 500)
    (let ((n (random 100)))
      (interval:delete *aa* (cons n (* 2 n)))
      (interval:tree-validate *aa*)))
  (results))

(check (:name :same-starts)
  (run :make-tree)
  (dotimes (i 20)
    (interval:insert *aa* (interval:make-interval :start 0 :end i))
    (interval:tree-validate *aa*))
  (results
   *aa*
   (mi (interval:find-all *aa* 0))
   (mi (interval:find-all *aa* 1))
   (mi (interval:find-all *aa* 2))
   (mi (interval:find-all *aa* 9))
   (mi (interval:find-all *aa* 10))
   (mi (interval:find-all *aa* 15))
   (mi (interval:find-all *aa* 19))
   (mi (interval:find-all *aa* 20))))

(check ()
 (time
  (progn
    (run :make-tree)
    (dotimes (i 100000)
      (interval:insert *aa* (cons i i)))
    (dotimes (i 100000)
      (interval:find *aa* (cons i i)))
    (dotimes (i 100000)
      (interval:delete *aa* (cons i i))))))
