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
	   #:bbox-width)
  )

(in-package cl-gdsfeel/geom)

(defclass <point> (point)
  ())


(defun p (x y)
  (make-point x y '<point>))


(defmethod p+ ((p1 <point>) (p2 <point>))
  (p (+ (x p1) (x p2))
     (+ (y p1) (y p2))))


(defgeneric as-point (object))


(defmethod as-point ((object sequence))
  (p (first object) (second object)))


(defmethod as-point ((object cons))
  (p (car object) (cdr object)))

(defclass <transform> (clem:affine-transformation) ())


(defclass <bounding-box> (bounding-box) ())



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

(sample-bounding-box)
