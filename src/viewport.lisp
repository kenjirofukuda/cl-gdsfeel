(defpackage :cl-gdsfeel/viewport
  (:use #:cl
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
  ((port-width :type double-float :initform 0.0d0 :initarg :width :accessor port-width)
   (port-height :type double-float :initform 0.0d0 :initarg :height :accessor port-height)
   (w-scale :type double-float :initform 1.0d0 :accessor w-scale)
   (w-center-x :type double-float :initform 1.0d0 :accessor w-center-x)
   (w-center-y :type double-float :initform 1.0d0 :accessor w-center-y)
   (_transform :type (or <transform> null) :initform nil :accessor transform)
   (_basic-transform :type (or <transform> null) :initform nil :accessor basic-transform)
   (_damage-count :type integer :initform 0 :accessor damage-count)
   (_transform-stack :type list :initform nil :accessor transform-stack)
   )  
  )


(defun set-bounds (vp w-bounds)
  (let* ((h-ratio (/ (port-width vp) (bbox-width w-bounds)))
	 (v-ratio (/ (port-height vp) (bbox-height w-bounds)))
	 (ratio (if (< h-ratio v-ratio) h-ratio v-ratio))
	 (new-center (make-point (bbox-mid-x w-bounds) (bbox-mid-y w-bounds))))
    (setf (w-center-x vp) (x new-center))
    (setf (w-center-y vp) (y new-center))
    (setf (w-scale vp) ratio)
    (incf (damage-count vp))))


(defmethod bounds ((viewport <viewport>) (w-bounds <bounding-box>) ) )


(defun fit-xxyy (viewport xmin xmax ymin ymax)
  (list viewport xmin xmax ymin ymax)
  )

(defvar *r* nil)
(defvar *vp* nil)

(defun setup ()
  (setq *r* (make-instance '<bounding-box> :x-min 10
					   :x-max 200
					   :y-min 3000
					   :y-max 4000)) 

  (setq *vp* (make-instance '<viewport> :width 640
					:height 480))
  (set-bounds *vp* *r*)
  )
(setup)
