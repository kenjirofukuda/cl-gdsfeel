(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("clem" "cl-geometry2")))

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
	   #:w-center
	   #:basic-transform
	   #:world->device
	   #:device->world
	   #:pop-transform
	   #:push-transform
	   #:final-transform
	   #:get-bounds
	   #:set-bounds
	   #:damage-transform))

(in-package :cl-gdsfeel/viewport)

(defclass <viewport> ()
  ((port-width :type double-float :initform 0.0d0 :initarg :width :accessor port-width)
   (port-height :type double-float :initform 0.0d0 :initarg :height :accessor port-height)
   (w-scale :type double-float :initform 1.0d0 :accessor w-scale)
   (w-center-x :type double-float :initform 0.0d0 :accessor w-center-x)
   (w-center-y :type double-float :initform 0.0d0 :accessor w-center-y)
   (_transform :type (or <transform> null) :initform nil :accessor _transform)
   (_basic-transform :type (or <transform> null) :initform nil :accessor _basic-transform)
   (_transform-stack :type list :initform nil :accessor _transform-stack)))


(defun damage-transform (vp)
  (setf (_basic-transform vp) nil)
  (setf (_transform vp) nil))


(defmethod (setf w-scale) ((vp <viewport>) value)
  (setf (slot-value vp 'w-scale) value)
  (damage-transform vp)
  value)


(defmethod w-center ((vp <viewport>))
  (p (w-center-x vp) (w-center-y vp)))


(defmethod (setf w-center) (value (vp <viewport>))
  (let ((pt (as-point value)))
    (setf (slot-value vp 'w-center-x) (x pt))
    (setf (slot-value vp 'w-center-y) (y pt))
    (damage-transform vp)
    pt))


(defun set-bounds (vp w-bounds)
  (let* ((h-ratio (/ (port-width vp) (bbox-width w-bounds)))
	 (v-ratio (/ (port-height vp) (bbox-height w-bounds)))
	 (ratio (if (< h-ratio v-ratio) h-ratio v-ratio))
	 (new-center (make-point (bbox-mid-x w-bounds) (bbox-mid-y w-bounds))))
    (setf (w-center-x vp) (x new-center))
    (setf (w-center-y vp) (y new-center))
    (setf (w-scale vp) ratio)
    (damage-transform vp)))


(defun get-bounds (vp)
  (let ((inv (clem:invert-matrix (final-transform vp))))
    (multiple-value-bind (x1 y1)
	(clem:transform-coord 0 0 inv)
      (multiple-value-bind (x2 y2)
	  (clem:transform-coord (port-width vp) (port-height vp) inv)
	(make-bbox x1 y1 x2 y2)))))


(defun lookup-basic-transform (vp)
  (let* ((tx1 (clem:make-affine-transformation
	       :x-shift (/ (port-width vp) 2)
	       :y-shift (/ (port-height vp) 2)))
	 (tx2 (clem:make-affine-transformation
	       :x-scale (w-scale vp)
	       :y-scale (w-scale vp)))
	 (tx3 (clem:make-affine-transformation
	       :x-shift (- (w-center-x vp))
	       :y-shift (- (w-center-y vp)))))
    (clem:m* tx1 tx2 tx3)))


(defun basic-transform (vp)
  (unless (_basic-transform vp)
    (setf (_basic-transform vp) (lookup-basic-transform vp)))
  (_basic-transform vp))


(defun lookup-final-transform (vp)
  (let ((tx (basic-transform vp)))
    (dolist (m (_transform-stack vp))
      (setf tx (clem:m* tx m)))
    tx))


(defun final-transform (vp)
  (unless (_transform vp)
    (setf (_transform vp) (lookup-final-transform vp)))
  (_transform vp))


(defun push-transform (vp tx)
  (push tx (_transform-stack vp))
  (damage-transform vp)
  tx)


(defun pop-transform (vp)
  (if (null (_transform-stack vp))
      nil
      (let ((result (car (_transform-stack vp))))
	(setf (_transform-stack vp) (cdr (_transform-stack vp)))
	(damage-transform vp)
	result)))


(defmethod bounds ((viewport <viewport>) (w-bounds <bounding-box>) ) )


(defmethod world->device ((vp <viewport>) (pt <point>))
  (multiple-value-bind (xd yd)
      (clem:transform-coord (x pt) (y pt) (final-transform vp))
    (p xd yd)))


(defmethod world->device ((vp <viewport>) (bbox <bounding-box>))
  (let ((origin (world->device vp (bbox-origin bbox)))
	(corner (world->device vp (bbox-corner bbox))))
    (2point->bbox origin corner)))


(defmethod device->world ((vp <viewport>) (pt <point>))
  (let ((inv (clem:invert-matrix (final-transform vp))))
    (multiple-value-bind (xw yw)
	(clem:transform-coord (x pt) (y pt) inv)
      (p xw yw))))


(defvar *r* nil)
(defvar *vp* nil)

(defun setup ()
  (setq *r* (make-bbox 0 0 640 480)) 
  (setq *vp* (make-instance '<viewport> :width 640
					:height 480))
  (set-bounds *vp* *r*))

(setup)
