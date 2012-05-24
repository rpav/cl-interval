(in-package :interval)

(defstruct interval
  start end)

(defmethod print-object ((i interval) stream)
  (format stream "~A-~A"
          (interval-start i)
          (interval-end i)))

(declaim (inline interval< interval=))
(defun interval< (i1 i2)
  (declare (type interval i1 i2))
  (< (interval-start i1)
     (interval-start i2)))

(defun interval= (i1 i2)
  (declare (type interval i1 i2))
  (and (= (interval-start i1) (interval-start i2))
       (= (interval-end i1) (interval-end i2))))

(defun interval-intersects (v<-fun i1 i2)
  (declare (type function v<-fun)
           (type interval i1 i2))
  (flet ((v< (a b) (funcall v<-fun a b)))
    (and (v< (interval-start i1) (interval-end i2))
         (v< (interval-start i2) (interval-end i1)))))
