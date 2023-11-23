(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("clem" "cl-geometry2")))

(defpackage cl-gdsfeel/geom
  (:use #:cl
	#:clem
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
	   #:transform-point)
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
