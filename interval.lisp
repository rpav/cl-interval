(in-package :interval)

(declaim (inline interval-start interval-end make-interval))
(defstruct interval
  start end)

(setf (documentation 'interval-start 'function)
      "=> start
Return the `START` value of the interval.")

(setf (documentation 'interval-end 'function)
      "=> end
Return the `END` value of the interval.")

(setf (documentation 'make-interval 'function)
      "=> INTERVAL
Returns a simple interval with specified `START` and `END`.")

(defmethod print-object ((i interval) stream)
  (format stream "#<~A-~A>"
          (interval-start i)
          (interval-end i)))

(declaim (inline interval< interval=))
(defun interval< (i1 i2)
  "=> boolean
A simple example (also used in tests) which compares interval starts
numerically by `#'<`."
  (declare (type interval i1 i2))
  (< (interval-start i1)
     (interval-start i2)))

(defun interval= (i1 i2)
  "=> boolean
A simple example (also used in tests) which compares interval equality
numerically by `#'=`."
  (declare (type interval i1 i2))
  (and (= (interval-start i1) (interval-start i2))
       (= (interval-end i1) (interval-end i2))))

(defun interval-intersects (v<-fun i1 i2)
  (declare (type function v<-fun)
           (type interval i1 i2))
  (flet ((v< (a b) (funcall v<-fun a b)))
    (and (v< (interval-start i1) (interval-end i2))
         (v< (interval-start i2) (interval-end i1)))))
