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
	   #:world->device2
	   #:device->world2
	   #:final-transform
	   #:get-bounds
	   #:set-bounds
	   #:damage-transform
	   #:port-center-x
	   #:port-center-y
	   #:set-port-center
	   #:reset-port-center
	   #:with-transform
	   #:device-pixel-convertor
	   #:whell-zoom
	   #:port-stack-empty-p
	   #:device-size))

(in-package :cl-gdsfeel/viewport)

(defclass <viewport> ()
  ((port-width :type integer :initform 0 :initarg :width :accessor port-width)
   (port-height :type integer :initform 0 :initarg :height :accessor port-height)
   (port-center-x :type integer :initform 0 :accessor port-center-x)
   (port-center-y :type integer :initform 0 :accessor port-center-y)
   (w-scale :type double-float :initform 1.0d0 :accessor w-scale)
   (w-center-x :type double-float :initform 0.0d0 :accessor w-center-x)
   (w-center-y :type double-float :initform 0.0d0 :accessor w-center-y)
   (_transform :type (or mat3 null) :initform nil :accessor _transform)
   (_basic-transform :type (or mat3 null) :initform nil :accessor _basic-transform)
   (_transform-stack :type list :initform nil :accessor _transform-stack)
   (device-pixel-convertor :type symbol :initform 'identity :accessor device-pixel-convertor)))


(defun set-port-center (vp pt)
  (setf (port-center-x vp) (truncate (x pt)))
  (setf (port-center-y vp) (truncate (y pt)))
  (damage-transform vp))


(defun reset-port-center (vp)
  (setf (port-center-x vp) (truncate (/ (port-width vp) 2)))
  (setf (port-center-y vp) (truncate (/ (port-height vp) 2)))
  (damage-transform vp))


(defun reset-world (vp)
  (setf (w-scale vp) 1.0d0)
  (setf (w-center vp) (p 0.0d0 0.0d0)))


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


(defun fitting-ratio (vp w-bounds)
  (let ((h-ratio (/ (port-width vp) (bbox-width w-bounds)))
	(v-ratio (/ (port-height vp) (bbox-height w-bounds))))
    (* (min h-ratio v-ratio) 0.95)))


(defun set-bounds (vp w-bounds)
  (reset-port-center vp)
  (setf (w-center vp) (bbox-mid w-bounds))
  (setf (w-scale vp) (fitting-ratio vp w-bounds))
  (damage-transform vp))


(defun get-bounds (vp)
  (let* ((tx (final-transform vp))
	 (min-pt (invert-point2 tx (p 0 0)))
	 (max-pt (invert-point2 tx (p (port-width vp) (port-height vp)))))
    (2point->bbox min-pt max-pt)))


(defun lookup-basic-transform (vp)
  (let* ((tx1 (clem:make-affine-transformation
	       :x-shift (port-center-x vp)
	       :y-shift (port-center-y vp)))
	 (tx2 (clem:make-affine-transformation
	       :x-scale (w-scale vp)
	       :y-scale (w-scale vp)))
	 (tx3 (clem:make-affine-transformation
	       :x-shift (- (w-center-x vp))
	       :y-shift (- (w-center-y vp)))))
    (clem:m* tx1 tx2 tx3)))


(defun lookup-basic-transform2 (vp)
  (let* ((tx1 (make-mat3))
	 (tx2 (make-mat3))
	 (tx3 (make-mat3)))
    (set-affine-parameters (mat3-m tx1) :x-shift (port-center-x vp)
					:y-shift (port-center-y vp))
    (set-affine-parameters (mat3-m tx2) :x-scale (w-scale vp)
					:y-scale (w-scale vp))
    (set-affine-parameters (mat3-m tx3) :x-shift (- (port-center-x vp))
					:y-shift (- (port-center-y vp)))
    (mat3* (mat3* tx1 tx2) tx3)))



(defun basic-transform (vp)
  (unless (_basic-transform vp)
    (setf (_basic-transform vp) (lookup-basic-transform2 vp)))
  (_basic-transform vp))


(defun lookup-final-transform (vp)
  (let ((tx (basic-transform vp)))
    (dolist (m (reverse (_transform-stack vp)))
      (setf tx (clem:m* tx m)))
    tx))


(defun lookup-final-transform2 (vp)
  (let ((tx (basic-transform vp)))
    (dolist (m (reverse (_transform-stack vp)))
      (setf tx (mat3* tx m)))
    tx))


(defun final-transform (vp)
  (unless (_transform vp)
    (setf (_transform vp) (lookup-final-transform2 vp)))
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

(defun port-stack-empty-p (vp)
  (zerop (length (_transform-stack vp))))


(defun call-with-transform (vp tx func)
  (push-transform vp tx) 
  (unwind-protect
       (progn
	 (funcall func))
    (pop-transform vp)))


(defmacro with-transform (vp tx &body body)
  `(call-with-transform ,vp ,tx #'(lambda () ,@body)))


(defmethod bounds ((viewport <viewport>) (w-bounds <bounding-box>) ) )


(defun device-size (vp size)
  (let ((p1 (world->device2 vp (p size size)))
	(p2 (world->device2 vp (p 0 0))))
    (distance (x p1) (y p1) (x p2) (y p2))))


(defmethod world->device ((vp <viewport>) (pt <point>))
  (multiple-value-bind (xd yd)
      (clem:transform-coord (x pt) (y pt) (final-transform vp))
    (p (funcall (device-pixel-convertor vp) xd)
       (funcall (device-pixel-convertor vp) yd))))


(defmethod world->device2 ((vp <viewport>) (pt <point>))
  (let ((dest (transform-point2 (final-transform vp) pt)))
    (p (funcall (device-pixel-convertor vp) (x dest))
       (funcall (device-pixel-convertor vp) (y dest)))))


(defmethod world->device ((vp <viewport>) (bbox <bounding-box>))
  (let ((origin (world->device vp (bbox-origin bbox)))
	(corner (world->device vp (bbox-corner bbox))))
    (2point->bbox origin corner)))


(defmethod world->device2 ((vp <viewport>) (bbox <bounding-box>))
  (let ((origin (world->device2 vp (bbox-origin bbox)))
	(corner (world->device2 vp (bbox-corner bbox))))
    (2point->bbox origin corner)))


(defmethod device->world ((vp <viewport>) (pt <point>))
  (let ((inv (clem:invert-matrix (final-transform vp))))
    (multiple-value-bind (xw yw)
	(clem:transform-coord (x pt) (y pt) inv))))

    
(defmethod device->world2 ((vp <viewport>) (pt <point>))
  (invert-point2 (final-transform vp) pt))

(defun whell-zoom (vp port-pt direction)
  (setf (port-center-x vp) (x port-pt))
  (setf (port-center-y vp) (y port-pt))
  (let* ((mat (final-transform vp))
	 (world-center (invert-point2 mat port-pt)))
    (setf (w-center vp) world-center)
    (setf (w-scale vp) (* (w-scale vp) (+ 1.0 (* 0.125 direction)))))
  (damage-transform vp))


(defvar *r* nil)
(defvar *vp* nil)

(defun setup ()
  (setq *r* (make-bbox 0 0 640 480)) 
  (setq *vp* (make-instance '<viewport> :width 640
					:height 480))
  (set-bounds *vp* *r*))

(setup)
