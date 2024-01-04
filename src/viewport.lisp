(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("3d-matrices" "cl-geometry2")))

(defpackage :cl-gdsfeel/viewport
  (:use #:cl
	#:cl-geometry2
	#:3d-vectors
	#:3d-matrices
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
	   #:device-size
	   #:view-move-ratio
	   #:view-move-left
	   #:view-move-right
	   #:view-move-up
	   #:view-move-down
	   #:view-zoom-double
	   #:view-zoom-half))

(in-package :cl-gdsfeel/viewport)


(declaim (inline bbox-width bbox-height))


(defclass <viewport> ()
  ((port-width :type integer :initform 0 :initarg :width :accessor port-width)
   (port-height :type integer :initform 0 :initarg :height :accessor port-height)
   (port-center-x :type integer :initform 0 :accessor port-center-x)
   (port-center-y :type integer :initform 0 :accessor port-center-y)
   (w-scale :type double-float :initform 1.0d0 :accessor w-scale)
   (w-center-x :type double-float :initform 0.0d0 :accessor w-center-x)
   (w-center-y :type double-float :initform 0.0d0 :accessor w-center-y)
   (view-move-ratio :type double-float :initform 0.25d0 :accessor view-move-ratio)
   (_transform :type (or mat3 null) :initform nil :accessor _transform)
   (_basic-transform :type (or mat3 null) :initform nil :accessor _basic-transform)
   (_transform-stack :type list :initform nil :accessor _transform-stack)
   (device-pixel-convertor :type symbol :initform 'identity :accessor device-pixel-convertor)))


(defun set-port-center (vp pt)
  (setf (port-center-x vp) (truncate (vx2 pt)))
  (setf (port-center-y vp) (truncate (vx2 pt)))
  (damage-transform vp))


(defun reset-port-center (vp)
  (setf (port-center-x vp) (truncate (/ (port-width vp) 2)))
  (setf (port-center-y vp) (truncate (/ (port-height vp) 2)))
  (damage-transform vp))


(defun reset-world (vp)
  (setf (w-scale vp) 1.0d0)
  (setf (w-center vp) (vec2 0.0 0.0)))


(defun damage-transform (vp)
  (setf (_basic-transform vp) nil)
  (setf (_transform vp) nil))


(defmethod (setf w-scale) ((vp <viewport>) value)
  (setf (slot-value vp 'w-scale) value)
  (damage-transform vp)
  value)


(defmethod w-center ((vp <viewport>))
  (vec2 (w-center-x vp) (w-center-y vp)))


(defmethod (setf w-center) (value (vp <viewport>))
  (let ((pt (as-point value)))
    (setf (slot-value vp 'w-center-x) (vx2 pt))
    (setf (slot-value vp 'w-center-y) (vy2 pt))
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
	 (min-pt (invert-point tx (vec2 0.0 0.0)))
	 (max-pt (invert-point tx (vec2 (port-width vp) (port-height vp)))))
    (2point->bbox min-pt max-pt)))


(defun lookup-basic-transform (vp)
  (let ((tx1 (m3translation (vec (port-center-x vp)
				 (port-center-y vp))))
	(tx2 (m3scaling (vec (w-scale vp)
			     (w-scale vp))))
	(tx3  (m3translation (vec (- (w-center-x vp))
				  (- (w-center-y vp))))))
    (m* tx1 tx2 tx3)))


(defun basic-transform (vp)
  (unless (_basic-transform vp)
    (setf (_basic-transform vp) (lookup-basic-transform vp)))
  (_basic-transform vp))


(defun lookup-final-transform (vp)
  (let ((tx (basic-transform vp)))
    (dolist (m (reverse (_transform-stack vp)))
      (setf tx (m* tx m)))
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
  (let ((p1 (world->device vp (vec2 size size)))
	(p2 (world->device vp (vec2 0 0))))
    (distance (vx2 p1) (vy2 p1) (vx2 p2) (vy2 p2))))


(defmethod world->device ((vp <viewport>) (pt vec2))
  (let ((dest (transform-point (final-transform vp) pt)))
    (vec2 (funcall (device-pixel-convertor vp) (vx2 dest))
	  (funcall (device-pixel-convertor vp) (vy2 dest)))))


(defmethod world->device ((vp <viewport>) (bbox <bounding-box>))
  (let ((origin (world->device vp (bbox-origin bbox)))
	(corner (world->device vp (bbox-corner bbox))))
    (2point->bbox origin corner)))


(defmethod device->world ((vp <viewport>) (pt vec2))
  (invert-point (final-transform vp) pt))


(defun whell-zoom (vp port-pt direction)
  (setf (port-center-x vp) (vx2 port-pt))
  (setf (port-center-y vp) (vy2 port-pt))
  (let* ((mat (final-transform vp))
	 (world-center (invert-point mat port-pt)))
    (setf (w-center vp) world-center)
    (setf (w-scale vp) (* (w-scale vp) (+ 1.0 (* 0.125 direction)))))
  (damage-transform vp))

(defun view-move-fraction (vp x-frac y-frac)
  (let* ((w-box (get-bounds vp))
	 (x-offset (* x-frac (bbox-width w-box)))
	 (y-offset (* y-frac (bbox-height w-box))))
    (setf (w-center vp) (vec2 (+ (w-center-x vp) x-offset)
			      (+ (w-center-y vp) y-offset)))))

(defun view-move-left (vp)
  (with-slots ((ratio view-move-ratio)) vp
    (view-move-fraction vp (- ratio) 0.0d0)))

(defun view-move-right (vp)
  (with-slots ((ratio view-move-ratio)) vp
    (view-move-fraction vp ratio 0.0d0)))

(defun view-move-up (vp)
  (with-slots ((ratio view-move-ratio)) vp
    (view-move-fraction vp 0.0d0 ratio)))

(defun view-move-down (vp)
  (with-slots ((ratio view-move-ratio)) vp
    (view-move-fraction vp 0.0d0 (- ratio))))

(defun view-zoom-double (vp)
  (setf (w-scale vp) (* (w-scale vp) 2.0d0)))

(defun view-zoom-half (vp)
  (setf (w-scale vp) (* (w-scale vp) 0.5d0)))
