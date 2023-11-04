(defpackage cl-gdsfeel/geom
  (:use #:cl
	#:clem)
  (:shadow :structure)
  (:export
   :<point>
   :p
   :as-point
   )
  )

(in-package :cl-gdsfeel/geom)


(defclass <point> ()
  ((x :type double-float :initarg :x :initform 0.0d0 :accessor x)
   (y :type double-float :initarg :y :initform 0.0d0 :accessor y)))


(defmethod print-object ((object <point>) stream)
  (print-unreadable-object (object stream :type nil)
    (format stream "[~d, ~d]" (x object) (y object))))


(defun p (x y)
  (make-instance '<point> :x (coerce x 'double-float)
			  :y (coerce y 'double-float)))


(defmethod p+ ((p1 <point>) (p2 <point>))
  (p (+ (x p1) (x p2))
     (+ (y p1) (y p2))))


(defgeneric as-point (object))


(defmethod as-point ((object sequence))
  (p (first object) (second object)))


(defmethod as-point ((object cons))
  (p (car object) (cdr object)))


