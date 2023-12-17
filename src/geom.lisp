(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("3d-matrices" "cl-geometry2")))

(defpackage cl-gdsfeel/geom
  (:use #:cl
	#:cl-user
	#:sb-alien
	#:cl-geometry2
	#:3d-vectors
	#:3d-matrices
	)
  (:export #:<point>
	   #:p
	   #:as-point
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
	   #:transform-point2
	   #:invert-point2
	   #:mat3*
	   #:my/make-mat3
	   #:my/mat3-m
	   #:my/mat3-mult
	   #:set-affine-parameters
	   #:m3translation
	   #:m3scaling
	   #:m3rotation)
  )

(in-package cl-gdsfeel/geom)

(declaim (inline bbox-width bbox-height))

(defclass <point> ()
  ((x :type double-float :reader x :initarg :x :initform 0.0d0)
   (y :type double-float :reader y :initarg :y :initform 0.0d0)))


(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :type nil)
    (format stream "~a @ ~a" (x object) (y object))))


(defun p (x y)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type double-float x y))
					;(assert (and (numberp x) (numberp y)))
  (make-instance '<point> :x x :y y))


(defmethod p+ ((p1 <point>) (p2 <point>))
  (p (+ (x p1) (x p2))
     (+ (y p1) (y p2))))


(declaim (inline dotpairp))


(defun dotpairp (x)
  (and (listp x) (atom (car x)) (not (listp (cdr x)))))


(defgeneric as-point (object))


(defmethod as-point ((object sequence))
  (declare (inline dotpairp))
  (if (dotpairp object)
      (p (car object) (cdr object))
      (p (first object) (second object))))


(defmethod as-point ((object <point>))
  (p (x object) (y object)))


;; (defclass <transform> (clem:affine-transformation) ())


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
	(setq xmin (min xmin (x pt)))
	(setq xmax (max xmax (x pt)))
	(setq ymin (min ymin (y pt)))
	(setq ymax (max ymax (y pt)))))
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
  (p (bbox-mid-x bbox) (bbox-mid-y bbox)))


(defmethod bbox-origin ((bbox <bounding-box>))
  (with-slots (x-min y-min) bbox
    (p x-min y-min)))


(defmethod bbox-corner ((bbox <bounding-box>))
  (with-slots (x-max y-max) bbox
    (p x-max y-max)))


(defmethod bbox-points ((bbox <bounding-box>))
  (with-slots (x-min y-min x-max y-max)  bbox
    (list (p x-min y-min)
	  (p x-min y-max)
	  (p x-max y-max)
	  (p x-max y-min))))


;; (defun transform-point (tx pt)
;;   (multiple-value-bind (xd yd)
;;       (clem:transform-coord (x pt) (y pt) tx)
;;     (p xd yd)))

;;(declaim (ftype (function (mat3 <point>) <point>) transform-point))
(defun transform-point (tx pt)
  (let ((m (m* tx (m3translation (vec (x pt) (y pt))))))
    ;;(describe m)
    (p (mcref3 m 0 2) (mcref3 m 1 2))
    )
  )


(defun transform-point2 (tx pt)
  (let ((m (make-my/mat3)))
    (setf (aref (my/mat3-m m) 0 0) (coerce (x pt) 'double-float))
    (setf (aref (my/mat3-m m) 0 1) (coerce (y pt) 'double-float))
    (setf (aref (my/mat3-m m) 0 2) 1.0d0)
    (let ((cm (my/mat3-mult tx m)))
					;(print (my/mat3-m cm))
      (p (aref (my/mat3-m cm) 0 0) (aref (my/mat3-m cm) 0 1)))))

;;(declaim (ftype (function (mat3 <point>) <point>) invert-point))
(defun invert-point (tx pt)
  (let* ((x1 (- (x pt) (mcref3 tx 0 2)))
	 (y1 (- (y pt) (mcref3 tx 1 2)))
	 (a00 (mcref3 tx 0 0))
	 (a01 (mcref3 tx 0 1))
	 (a10 (mcref3 tx 1 0))
	 (a11 (mcref3 tx 1 1))
	 (det (- (* a00 a11) (* a01 a10)))
	 (detx 0)
	 (dety 0))
    (when (zerop det)
      (return-from invert-point (p 0 0)))
    (setq det (/ 1.0d0 det))
    (setq detx (- (* x1 a11) (* a01 y1)))
    (setq dety (- (* a00 y1) (* x1 a10)))
    (p (* detx det) (* detY det))))


(defun invert-point2 (tx pt)
  (let* ((x1 (- (x pt) (aref (my/mat3-m tx) 2 0)))
	 (y1 (- (y pt) (aref (my/mat3-m tx) 2 1)))
	 (a00 (aref (my/mat3-m tx) 0 0))
	 (a01 (aref (my/mat3-m tx) 1 0))
	 (a10 (aref (my/mat3-m tx) 0 1))
	 (a11 (aref (my/mat3-m tx) 1 1))
	 (det (- (* a00 a11) (* a01 a10)))
	 (detx 0)
	 (dety 0))
    (when (zerop det)
      (return-from invert-point2 (p 0 0)))
    (setq det (/ 1.0d0 det))
    (setq detx (- (* x1 a11) (* a01 y1)))
    (setq dety (- (* a00 y1) (* x1 a10)))
    (p (* detx det) (* detY det))))


(defun sample-bounding-box ()
  (let ((b 
	  (make-instance '<bounding-box> :x-min 10/3
					 :x-max 200/7
					 :y-min 3000/11
					 :y-max 40000/13)))
    (describe b)
    ;;    (print (bbox-width b))
    ;;    (print (bbox-height b))
    b
    ))
(defvar *foo* nil)
(setq *foo* (sample-bounding-box))

(sb-alien:load-shared-object "libblas.so.3")

(declaim (inline dgemm))

(sb-alien:define-alien-routine ("dgemm_" dgemm) void
  (transa c-string)
  (transb c-string)
  (m int :copy)
  (n int :copy)
  (k int :copy)
  (alpha double :copy)
  (a (* double))
  (lda int :copy)
  (b (* double))
  (ldb int :copy)
  (beta double :copy)
  (c (* double))
  (ldc int :copy))

(defun pointer (array)
  (sap-alien (sb-sys:vector-sap (sb-ext:array-storage-vector array)) (* double)))

(defstruct my/mat3
  (m (make-array '(3 3) :element-type 'double-float
			:initial-contents '((1.0d0 0.0d0 0.0d0) (0.0d0 1.0d0 0.0d0) (0.0d0 0.0d0 1.0d0)))))


(defun set-affine-parameters (mat
                              &key
                                (y-shift 0d0)
                                (x-shift 0d0)
                                (theta 0d0)
                                (y-scale 1d0)
                                (x-scale 1d0)
                                (y-shear 0d0)
                                (x-shear 0d0))
  (setf (aref mat 0 0) (- (* (cos theta) x-scale)
                          (* (sin theta) y-scale y-shear)))
  (setf (aref mat 1 0) (- (* (cos theta) x-scale x-shear)
                          (* (sin theta) y-scale)))
  (setf (aref mat 2 0) (coerce x-shift 'double-float))

  (setf (aref mat 0 1) (+ (* (sin theta) x-scale)
                          (* (cos theta) y-scale y-shear)))
  (setf (aref mat 1 1) (+ (* (sin theta) x-scale x-shear)
                          (* (cos theta) y-scale)))
  (setf (aref mat 2 1) (coerce y-shift 'double-float))
  
  (setf (aref mat 0 2) 0d0)
  (setf (aref mat 1 2) 0d0)
  (setf (aref mat 2 2) 1d0)
  mat)



(defun my/mat3* (a b)
  (let* ((c (make-my/mat3))
	 (n 3))
    (sb-sys:with-pinned-objects (a b c)
      (dgemm "n" "n" n n n 1d0 (pointer (my/mat3-m a)) n (pointer (my/mat3-m b)) n 0d0 (pointer (my/mat3-m c)) n))
    c))



;;(declaim (ftype (function (my/mat3 my/mat3) my/mat3) my/mat3-mult))
(defun my/mat3-mult (a b)
  ;;(declare (optimize (speed 3)))
  (let* ((c (make-my/mat3))
	 (n 3))
    (dotimes (i n)
      (dotimes (j n)
	(setf (aref (my/mat3-m c) j i) 0.0d0)
	(dotimes (k n)
	  (incf (aref (my/mat3-m c) j i) (* (aref (my/mat3-m a) k i) (aref (my/mat3-m b) j k))))))
    c))


;;(declaim (ftype (function (my/mat3 my/mat3) my/mat3) my/mat3-mult))
(defun my/mat3-mult-sub (a b)
  ;;(declare (optimize (speed 3)))
  (let* ((c (make-my/mat3))
	 (n 3))
    (dotimes (i n)
      (dotimes (j n)
	(setf (aref (my/mat3-m c) j i) 0.0d0)
	(dotimes (k n)
	  (incf (aref (my/mat3-m c) j i) (* (aref (my/mat3-m a) k i) (aref (my/mat3-m b) j k))))))
    c))


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
