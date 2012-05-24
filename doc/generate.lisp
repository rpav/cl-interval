(defpackage :interval.docs
  (:use #:cl #:gendoc)
  (:export #:generate))

(in-package :interval.docs)

(defun generate ()
  (gendoc (:output-system :cl-interval-docs
           :output-filename "index.html"
           :css "simple.css")
    (:mdf "intro.md")
    (:apiref :interval)))

