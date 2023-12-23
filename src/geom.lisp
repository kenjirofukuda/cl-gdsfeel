(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("3d-matrices" "cl-geometry2")))

(defpackage cl-gdsfeel/geom
  (:use #:cl
	#:cl-user
	#:sb-alien
	#:cl-geometry2
	#:3d-vectors
	#:3d-matrices)
  (:export #:as-point
	   #:<transform>
	   #:<bounding-box>
	   #:bbox-height
	   #:bbox-width
	   #:bbox-mid-x
	   #:bbox-mid-y
	   #:make-bbox
	   #:xxyy->bbox
	   #:xyxy->bbox
	   #:bbox-origin
	   #:bbox-corner
	   #:bbox-points
	   #:points->bbox
	   #:2point->bbox
	   #:bbox-mid
	   #:transform-point
	   #:invert-point
	   #:m3translation
	   #:m3scaling
	   #:m3rotation))

(in-package cl-gdsfeel/geom)

(declaim (inline bbox-width bbox-height))

(declaim (inline dotpairp))


(defun dotpairp (x)
  (and (listp x) (atom (car x)) (not (listp (cdr x)))))


(defgeneric as-point (object))


(defmethod as-point ((object sequence))
  (declare (inline dotpairp))
  (if (dotpairp object)
      (vec2 (3d-matrices::ensure-float (car object))
	    (3d-matrices::ensure-float (cdr object)))
      (vec2 (3d-matrices::ensure-float (first object))
	    (3d-matrices::ensure-float (second object)))))


(defmethod as-point ((object vec2))
  object)


(defclass <bounding-box> (bounding-box) ())


(defun xxyy->bbox (xmin xmax ymin ymax)
  (assert (and (<= xmin xmax) (<= ymin ymax)))
  (make-instance '<bounding-box> :x-min xmin
				 :x-max xmax
				 :y-min ymin
				 :y-max ymax))


(defun xyxy->bbox (xmin ymin xmax ymax)
  (assert (and (<= xmin xmax) (<= ymin ymax)))
  (make-instance '<bounding-box> :x-min xmin
				 :x-max xmax
				 :y-min ymin
				 :y-max ymax))

(defun negative-bbox (&optional (unit 9999))
  (make-instance '<bounding-box> :x-min unit
				 :x-max (- unit)
				 :y-min unit
				 :y-max (- unit)))


(defun points->bbox (points)
  (let* ((unit 9999)
	 (xmin unit)
	 (xmax (- unit))
	 (ymin unit)
	 (ymax (- unit)))
    (dolist (each points)
      (let* ((pt (as-point each)))
	(setq xmin (min xmin (vx2 pt)))
	(setq xmax (max xmax (vx2 pt)))
	(setq ymin (min ymin (vy2 pt)))
	(setq ymax (max ymax (vy2 pt)))))
    (make-bbox xmin ymin xmax ymax)))


(defun 2point->bbox (p1 p2)
  (points->bbox (list p1 p2)))


(defun make-bbox (xmin ymin xmax ymax)
  (xyxy->bbox xmin ymin xmax ymax))


(defmethod bbox-width ((bbox <bounding-box>))
  (with-slots (x-min x-max) bbox
    (- x-max x-min)))


(defmethod bbox-height ((bbox <bounding-box>))
  (with-slots (y-min y-max) bbox
    (- y-max y-min)))


(defmethod bbox-mid-x ((bbox <bounding-box>))
  (with-slots (x-min x-max) bbox
    (/ (+ x-max x-min) 2)))


(defmethod bbox-mid-y ((bbox <bounding-box>))
  (with-slots (y-min y-max) bbox
    (/ (+ y-max y-min) 2)))


(defmethod bbox-mid ((bbox <bounding-box>))
  (vec2 (bbox-mid-x bbox) (bbox-mid-y bbox)))


(defmethod bbox-origin ((bbox <bounding-box>))
  (with-slots (x-min y-min) bbox
    (vec2 x-min y-min)))


(defmethod bbox-corner ((bbox <bounding-box>))
  (with-slots (x-max y-max) bbox
    (vec2 x-max y-max)))


(defmethod bbox-points ((bbox <bounding-box>))
  (with-slots (x-min y-min x-max y-max)  bbox
    (list (vec2 x-min y-min)
	  (vec2 x-min y-max)
	  (vec2 x-max y-max)
	  (vec2 x-max y-min))))


(declaim (ftype (function (mat3 vec2) vec2) transform-point))
(defun transform-point (tx pt)
  (let ((m (m* tx (m3translation pt))))
    ;;(describe m)
    (vec2 (mcref3 m 0 2) (mcref3 m 1 2))))


(declaim (ftype (function (mat3 vec2) vec2) invert-point))
(defun invert-point (tx pt)
  (let* ((x1 (- (vx2 pt) (mcref3 tx 0 2)))
	 (y1 (- (vy2 pt) (mcref3 tx 1 2)))
	 (a00 (mcref3 tx 0 0))
	 (a01 (mcref3 tx 0 1))
	 (a10 (mcref3 tx 1 0))
	 (a11 (mcref3 tx 1 1))
	 (det (- (* a00 a11) (* a01 a10)))
	 (detx 0)
	 (dety 0))
    (when (zerop det)
      (return-from invert-point (vec2 0 0)))
    (setq det (/ 1.0d0 det))
    (setq detx (- (* x1 a11) (* a01 y1)))
    (setq dety (- (* a00 y1) (* x1 a10)))
    (vec2 (* detx det) (* detY det))))


;;(in-package #:org.shirakumo.flare.matrix)
(declaim (ftype (function (vec2) mat3) m3translation))
(3d-matrices::define-ofun m3translation (v)
  (mat 1 0 (vx2 v)
       0 1 (vy2 v)
       0 0 1))


(declaim (ftype (function (vec2) mat3) m3scaling))
(3d-matrices::define-ofun m3scaling (v)
  (mat (vx2 v) 0 0
       0 (vy2 v) 0
       0 0       1))


(declaim (inline %m3rotation))
(defun %m3rotation (arr angle)
  (declare (type (simple-array single-float (9)) arr))
  (declare (optimize speed (safety 0)))
  ;; https://joombig.com/sqlc/3D-Rotation-Algorithm-about-arbitrary-axis-with-CC-code-tutorials-advance
  (let* ((angle (3d-matrices::ensure-float angle))
         (c (cos angle))
         (s (sin angle)))
    (macrolet ((%mat (&rest els)
                 `(progn ,@(loop for el in els
                                 for i from 0
                                 collect `(setf (aref arr ,i) (3d-matrices::ensure-float ,el))))))
      (%mat c (- s) 0
            s c 0
            0 0 1))))


(declaim (ftype (function (real) mat3) m3rotation))
(3d-matrices::define-ofun m3rotation (angle)
  (let ((mat (mat3)))
    (%m3rotation (marr3 mat) angle)
    mat))
