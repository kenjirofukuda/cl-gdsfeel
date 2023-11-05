(defpackage :cl-gdsfeel/viewport
  (:use #:cl
	#:alexandria
	#:cl-geometry2
	#:cl-gdsfeel/geom
	)
  (:export #:<viewport>
	   #:port-width
	   #:port-height
	   #:w-scale
	   #:w-center-x
	   #:w-center-y
	   #:transform
	   #:basic-transform))

(in-package :cl-gdsfeel/viewport)

(defclass <viewport> ()
  ((port-width :type double-float :initform 0.0d0 :accessor port-width)
   (port-height :type double-float :initform 0.0d0 :accessor port-height)
   (w-scale :type double-float :initform 1.0d0 :accessor w-scale)
   (w-center-x :type double-float :initform 1.0d0 :accessor w-center-x)
   (w-center-y :type double-float :initform 1.0d0 :accessor w-center-y)
   (_transform :type (or <transform> null) :initform nil :accessor transform)
   (_basic-transform :type (or <transform> null) :initform nil :accessor basic-transform)
   (_damage-count :type integer :initform 0)
   (_transform-stack :type list :initform nil :accessor transform-stack)
   )  
  )

(defgeneric (setf bounds) (value viewport))

(defmethod (setf bounds) (w-bounds (viewport <viewport>))
  (let* ((h-ratio (/ (port-width viewport) ) )))
  )


(defmethod bounds ((viewport <viewport>) (w-bounds <bounding-box>) ) )


(defun fit-xxyy (viewport xmin xmax ymin ymax)
  
  )
