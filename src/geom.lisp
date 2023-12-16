(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("clem" "cl-geometry2")))

(defpackage cl-gdsfeel/geom
  (:use #:cl
	#:cl-user
	#:clem
	#:sb-alien
	#:cl-geometry2
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
	   #:mat3
	   #:make-mat3
	   #:mat3-m
	   #:set-affine-parameters)
  )

(in-package cl-gdsfeel/geom)


(defclass <point> (point)
  ())


(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream :type nil)
    (format stream "~a @ ~a" (x object) (y object))))


(defun p (x y)
  (assert (and (numberp x) (numberp y)))
  (make-point x y '<point>))


(defmethod p+ ((p1 <point>) (p2 <point>))
  (p (+ (x p1) (x p2))
     (+ (y p1) (y p2))))


(defun dotpairp (x)
  (and (listp x) (atom (car x)) (not (listp (cdr x)))))


(defgeneric as-point (object))


(defmethod as-point ((object sequence))
  (if (dotpairp object)
      (p (car object) (cdr object))
      (p (first object) (second object))))


(defmethod as-point ((object point))
  (p (x object) (y object)))


(defclass <transform> (clem:affine-transformation) ())


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


(defun transform-point (tx pt)
  (multiple-value-bind (xd yd)
      (clem:transform-coord (x pt) (y pt) tx)
    (p xd yd)))


(defun transform-point2 (tx pt)
  (let ((m (make-mat3)))
    (setf (aref (mat3-m m) 0 0) (coerce (x pt) 'double-float))
    (setf (aref (mat3-m m) 0 1) (coerce (y pt) 'double-float))
    (setf (aref (mat3-m m) 0 2) 1.0d0)
    (let ((cm (mat3* tx m)))
					;(print (mat3-m cm))
      (p (aref (mat3-m cm) 0 0) (aref (mat3-m cm) 0 1)))))


(defun invert-point (tx pt)
  (let* ((x1 (- (x pt) (mref tx 0 2)))
	 (y1 (- (y pt) (mref tx 1 2)))
	 (a00 (mref tx 0 0))
	 (a01 (mref tx 0 1))
	 (a10 (mref tx 1 0))
	 (a11 (mref tx 1 1))
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
  (let* ((x1 (- (x pt) (aref (mat3-m tx) 2 0)))
	 (y1 (- (y pt) (aref (mat3-m tx) 2 1)))
	 (a00 (aref (mat3-m tx) 0 0))
	 (a01 (aref (mat3-m tx) 1 0))
	 (a10 (aref (mat3-m tx) 0 1))
	 (a11 (aref (mat3-m tx) 1 1))
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

(defstruct mat3
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



(defun mat3* (a b)
  (let* ((c (make-mat3))
	 (n 3))
    (sb-sys:with-pinned-objects (a b c)
      (dgemm "n" "n" n n n 1d0 (pointer (mat3-m a)) n (pointer (mat3-m b)) n 0d0 (pointer (mat3-m c)) n))
    c))

