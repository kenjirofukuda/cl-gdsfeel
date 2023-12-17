(defpackage cl-gdsfeel/model
  (:use #:cl
	#:alexandria
	#:local-time
	#:3d-vectors
	#:3d-matrices
	#:cl-geometry2
	#:cl-gdsfeel/geom)
  (:shadow :structure)
  (:export
   :alloc-typed-vector
   :vector-last

   :<tree-node>
   :children
   :resolved
   :resolved-children
   :data-bbox
   
   :<named-container>
   :name
   :children
   :last-accessed
   :last-modified
   :child-names
   :child-named
   :leaf-children
   :no-leaf-children

   :<library>
   :user-unit
   :meter-unit
   :add-structure

   :<structure>
   :library
   :elements
   :add-element
   :structures
   :refnames
   :leaf-p
   :depth
   :referenced-structures

   :<strans>
   :mag
   :angle
   :abs-mag-p
   :abs-angle-p
   :reflected-p
   :transform-effective-p

   :<primitive>
   :<element>
   
   :structure
   :primitive-p
   :datatype
   :layer
   :xy
   :coords
   :coords-2a

   :<path>
   :path-width
   :pathtype
   :outline-coords

   :<boundary>

   :<text>
   :contents

   :<reference>
   :<sref>
   :refname

   :<aref>
   :x-step
   :y-step
   :row-count
   :column-count
   
   :points
   :calc-transform
   :used-layer-numbers
   :depth-info
   :ref-transform
   :ref-transform2
   :repeated-transform
   :repeated-transform2
   :ref-structure
   :structure-named
   :calc-bbox)
  )


(in-package :cl-gdsfeel/model)

(defun alloc-typed-vector (type)
  (make-array
   0
   :element-type type
   :initial-contents #()
   :fill-pointer 0
   :adjustable t))

(defun vector-last (v)
  (elt v (1- (length v))))


(defclass <tree-node> ()
  ((cached-bbox :initform nil :accessor cached-bbox)))


(defgeneric data-bbox (object))


(defmethod data-bbox ((object <tree-node>))
  (unless (cached-bbox object)
    (setf (cached-bbox object) (calc-bbox object)))
  (cached-bbox object))


(defmethod parent ((tree-node <tree-node>))
  nil)


(defgeneric children (tree-node)
  (:documentation "generic children accessor"))


(defgeneric resolved-children (tree-node)
  (:documentation "generic resolved-children accessor"))


(defclass <strans> ()
  ((mag :type double-float :initform 1.0d0 :accessor mag)
   (angle :type double-float :initform 0.0d0 :accessor angle)
   (abs-mag-p :type boolean :accessor abs-mag-p)
   (abs-angle-p :type boolean :accessor abs-angle-p)
   (reflected-p :type boolean :accessor reflected-p)))


(defclass <element> (<tree-node>)
  ((structure :type <structure> :accessor structure)
   (xy :type list :initform nil :accessor xy)))


(defclass <primitive> (<element>)
  ((datatype :type integer :initform -1 :accessor datatype)
   (layer    :type integer :initform -1 :accessor layer)))


(defclass <boundary> (<primitive>) ())


(defclass <path> (<primitive>)
  ((width    :type single-float :initform 0.0 :accessor path-width)
   (pathtype :type integer :initform 0 :accessor pathtype)))


(defclass <text> (<primitive> <strans>)
  ((contents :type string :initform "" :accessor contents)))


(defclass <reference> (<element> <strans>)
  ((_ref-transform
    :type (or affine-transformation null)
    :initform nil)
   (_ref-transform2
    :type (or my/mat3 null)
    :initform nil)))


(defclass <sref> (<reference>)
  ((refname   :type string       :initform "" :accessor refname)
   (_ref-structure :type (or null <structure>) :initform nil)))


(defclass <aref> (<sref>)
  ((x-step :type integer :initform 0 :accessor x-step)
   (y-step :type integer :initform 0 :accessor y-step)
   (row-count :type integer :initform 0 :accessor row-count)
   (column-count :type integer :initform 0 :accessor column-count)
   (_repeated-transform
    :type (or affine-transformation null)
    :initform nil)
   (_repeated-transform2
    :type (or mat3 null)
    :initform nil)))


(defclass <named-container> (<tree-node>)
  ((name
    :type string :initform "" :accessor name)
   (last-modified
    :type timestamp :accessor last-modified)
   (last-accessed
    :type timestamp :accessor last-accessed)))


(defclass <structure> (<named-container>)
  ((library
    :type <library>
    :accessor library)
   (elements
    :type list
    :initform nil
    :accessor elements)))

(defclass <library> (<named-container>)
  ((user-unit
    :type double-float
    :accessor user-unit)
   (meter-unit
    :type double-float
    :accessor meter-unit)
   (structures
    :type list
    :initform nil
    :accessor structures)
   (_structure-map
    :type hash-table
    :initform (make-hash-table))
   ))

(defconstant +radians-per-degree+  0.017453292519943295d0)


;; NOTE: don't remove still use for PATHS:
(defmethod coords-2a ((element <element>))
  (aops:reshape (coerce (xy element) 'vector) '(t 2)))


;; NOTE: don't remove still use in PATHS:
(defmethod coords ((element <element>))
  (let* ((coords (coords-2a element))
	 (dims (array-dimensions coords))
	 (nrows (car dims))
	 (result '()))
    (loop for i from 0 below nrows
	  do (push (cons  
		    (aref coords i 0)
		    (aref coords i 1))
		   result))
    (reverse result)))


(defmethod points ((element <element>))
  (mapcar #'as-point (coords element)))


(defun path-outline-coords (coords path-width pathtype)
  (when (zerop path-width)
    (return-from path-outline-coords coords))
  (let* ((path (paths:make-simple-path coords))
	 (outline (paths:stroke-path
		   path
		   path-width
		   :caps (case pathtype
			   (0
			    :butt)
			   (1
			    :round)
			   (2
			    :square)
			   (t
			    :butt))
		   :joint :miter
		   :inner-joint :miter)))
    (coerce (paths::path-knots (if (listp outline) (car outline) outline)) 'list)))


(defun outline-coords (path)
  (path-outline-coords (coords path) (path-width path) (pathtype path)))


(defgeneric calc-bbox (object))


(defmethod calc-bbox ((element <element>))
  (points->bbox (points element)))


(defmethod calc-bbox ((element <path>))
  (points->bbox (outline-coords element)))


(defmethod calc-bbox ((element <sref>))
  (let* ((ref-bbox (data-bbox (resolved element)))
	 (tx (ref-transform element)))
    (points->bbox (mapcar (lambda (each) (transform-point tx each))
			  (bbox-points ref-bbox)))))


(defmethod calc-bbox ((element <aref>))
  (let* ((ref-points (bbox-points (data-bbox (resolved element))))
	 (txs (repeated-transform element))
	 (all-points (flatten (mapcar (lambda (tx)
					(mapcar (lambda (pt) (transform-point tx pt))
						ref-points))
				      txs))))
    (points->bbox all-points)))


(defmethod ref-transform ((element <reference>))
  (when (null (slot-value element '_ref-transform))
    (setf (slot-value element '_ref-transform) (lookup-affine-transform element)))
  (slot-value element '_ref-transform))


(defmethod ref-transform2 ((element <reference>))
  (when (null (slot-value element '_ref-transform2))
    (setf (slot-value element '_ref-transform2) (lookup-affine-transform2 element)))
  (slot-value element '_ref-transform2))


(defmethod repeated-transform ((element <aref>))
  (when (null (slot-value element '_repeated-transform))
    (setf (slot-value element '_repeated-transform) (lookup-repeated-transform element)))
  (slot-value element '_repeated-transform))


(defmethod repeated-transform2 ((element <aref>))
  (when (null (slot-value element '_repeated-transform2))
    (setf (slot-value element '_repeated-transform2) (lookup-repeated-transform2 element)))
  (slot-value element '_repeated-transform2))


(defmethod calc-bbox ((structure <structure>))
  (let ((bbox nil)
	(xmins '())
	(ymins '())
	(xmaxs '())
	(ymaxs '()))    
    (loop for each in (children structure) 
	  do (setq bbox (data-bbox each))
	     (push (x-min bbox) xmins)
	     (push (y-min bbox) ymins)
	     (push (x-max bbox) xmaxs)
	     (push (y-max bbox) ymaxs))
    (make-bbox
     (apply #'min xmins)
     (apply #'min ymins)
     (apply #'max xmaxs)
     (apply #'max ymaxs))))


(defmethod primitive-p ((element <element>)) t)


(defmethod primitive-p ((element <reference>)) nil)


(defun non-zerop (v) (not (zerop v)))


(defmethod transform-effective-p ((strans <strans>))
  (or (non-zerop (mag strans))
      (non-zerop (angle strans))
      (reflected-p strans)
      (abs-mag-p strans)
      (abs-angle-p strans)))


;; (defmethod lookup-affine-transform ((reference <reference>))
;;   (let ((m (clem:make-affine-transformation :x-shift (first (xy reference))
;; 					    :y-shift (second (xy reference))
;; 					    :x-scale (mag reference)
;; 					    :y-scale (mag reference)
;; 					    :theta (* +radians-per-degree+ (angle reference)))))
;;     (when (reflected-p reference)
;;       (setf (mref m 0 1) (- (mref m 0 1)))
;;       (setf (mref m 1 1) (- (mref m 1 1))))
;;     m))


(defmethod lookup-affine-transform ((reference <reference>))
  (let ((mat (mtranslation (vec (first (xy reference))
				(second (xy reference))
				0.0))))
    (nmscale mat (vec (mag reference)
		      (mag reference)
		      1.0))
    (nmrotate mat +vz+ (* +radians-per-degree+ (angle reference)))
    (when (reflected-p reference)
      (setf (mcref4 mat 0 1) (- (mcref4 mat 0 1)))
      (setf (mcref4 mat 1 1) (- (mcref4 mat 1 1)))
      )
    mat
    )
  )


(defmethod lookup-affine-transform2 ((reference <reference>))
  (let* ((m (make-my/mat3))
	 (x (first (xy reference)))
	 (y (second (xy reference)))
	 (scale (mag reference))
	 (theta (* +radians-per-degree+ (angle reference)))
	 (rad-cos (* scale (cos theta)))
	 (rad-sin (* scale (sin theta))))
    (setf (aref (my/mat3-m m) 0 0) rad-cos)
    (setf (aref (my/mat3-m m) 0 1) rad-sin)
    (setf (aref (my/mat3-m m) 1 0) (- rad-sin))
    (setf (aref (my/mat3-m m) 1 1) rad-cos)
    (setf (aref (my/mat3-m m) 2 0) x)
    (setf (aref (my/mat3-m m) 2 1) y)
    (when (reflected-p reference)
      (setf (aref (my/mat3-m m) 1 0) (- (aref (my/mat3-m m) 1 0)))
      (setf (aref (my/mat3-m m) 1 1) (- (aref (my/mat3-m m) 1 1))))
    m))


(defmethod lookup-offsets ((aref <aref>))
  (flatten (loop for x-index below (column-count aref)
		 collect (loop
			   for y-index below (row-count aref)
			   collect (p (* x-index (x-step aref)) (* y-index (y-step aref)))))))


(defmethod lookup-repeated-transform ((aref <aref>))
  (let ((tx (ref-transform aref))
	(offsets (lookup-offsets aref)))
    (mapcar (lambda (offset)
	      (let ((otx (mtranslation (vec (x offset) 
					    (y offset)
					    0.0))))
		(m* tx otx)))
	    offsets)))


(defmethod lookup-repeated-transform2 ((aref <aref>))
  (let ((tx (ref-transform2 aref))
	(offsets (lookup-offsets aref)))
    (mapcar (lambda (offset)
	      (let ((otx (make-my/mat3)))
		(setf (aref (my/mat3-m otx) 2 0) (x offset))
		(setf (aref (my/mat3-m otx) 2 1) (y offset))
		(my/mat3-mult tx otx)))
	    offsets)))


(defmethod child-names ((container <named-container>))
  (map 'vector #'name (children container)))


(defmethod child-named ((container <named-container>) name)
  (find-if
   (lambda (x) (equalp (name x) name))
   (children container)))


(defmethod structure-named ((container <library>) name)
  (gethash name (slot-value container '_structure-map)))


(defmethod leaf-p ((structure <structure>))
  (zerop (length (reference-elements structure))))


(defmethod leaf-p ((element <element>))
  (primitive-p element))


(defmethod leaf-children ((container <named-container>))
  (remove-if-not #'leaf-p (children container)))


(defmethod no-leaf-children ((container <named-container>))
  (remove-if #'leaf-p (children container)))


(defmethod reference-elements ((structure <structure>))
  (remove-if #'primitive-p (children structure)))


(defmethod refnames ((structure <structure>))
  (remove-duplicates
   (map 'vector #'refname (reference-elements structure))
   :test 'equalp)) 


(defun as-sorted-uniq (lst)
  (sort (remove-duplicates lst) #'<))


(defmethod used-layer-numbers ((structure <structure>))
  (as-sorted-uniq 
   (mapcar (lambda (each) (layer each))
	   (remove-if-not #'leaf-p (elements structure)))))


(defmethod used-layer-numbers ((library <library>))
  (as-sorted-uniq (flatten (mapcar (lambda (s)
				     (used-layer-numbers s))
				   (structures library)))))


;; (defmethod print-tree ((library <library>))
;;   (dolist (each (no-leaf-children library))
;;     ))


(defmethod resolved ((structure <structure>))
  structure)


(defmethod resolved ((element <element>))
  nil)


(defmethod lookup-ref-structure ((sref <sref>))
  (child-named (library (structure sref)) (refname sref)))


(defmethod ref-structure ((sref <sref>))
  (when (null (slot-value sref '_ref-structure))
    (setf (slot-value sref '_ref-structure) (lookup-ref-structure sref)))
  (slot-value sref '_ref-structure))


(defmethod resolved ((sref <sref>))
  (ref-structure sref))


(defmethod children ((container <element>))
  nil)


(defmethod children ((container <structure>))
  (elements container))


(defmethod children ((container <library>))
  (structures container))


(defmethod resolved-children ((tree-node <tree-node>))
  (children tree-node))


(defmethod resolved-children ((structure <structure>))
  (map 'list
       (lambda (x) (child-named (library structure) x))
       (refnames structure)))


(defclass <walker> ()
  ((depth :type integer :initform 0 :accessor depth)))


(defmethod walk ((walker <walker>) (structure <structure>) proc)
  (funcall proc)
  (dolist (each (resolved-children structure))
    (incf (depth walker))
    (walk walker each proc)
    (decf (depth walker))))
  

(defmethod depth ((structure <structure>))
  (let ((walker (make-instance '<walker>))
        (max-depth 0))
    (walk walker structure
          #'(lambda ()
              (setq max-depth (max max-depth (depth walker)))))
    max-depth))


(defun depth-info (library)
  (sort (mapcar (lambda (s)
		  (cons (name s) (depth s)))
		(structures library))
	(lambda (a b) (> (cdr a) (cdr b)))))


(defmethod referenced-structures ((structure <structure>))
  (remove-if-not
   (lambda (each)
     (some   
      (lambda (el)
        (equalp (name structure) (refname el)))
      (no-leaf-children each)))
   (structures (library structure))))


(defmethod resolved-children ((sref <sref>))
  (resolved-children (resolved sref)))


(defmethod print-object ((object <named-container>) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "\"~a\"" (name object))))


(defmethod print-object ((object <sref>) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "\"~a\"" (refname object))))


(defmethod print-object ((object <text>) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "\"~a\"" (contents object))))


(defmethod add-element ((structure <structure>) (element <element>))
  ;;  (vector-push-extend element (slot-value structure 'elements))
  (push element (slot-value structure 'elements))
  element)


(defmethod add-structure ((library <library>) (structure <structure>))
  (push structure (structures library))
  ;; FIXME
  (setf (gethash (read-from-string (name structure)) (slot-value library '_structure-map)) structure)
  structure)


(defun fixture-library ()
  (let* ((library (make-instance '<library>))
         (structure (add-structure library (make-instance '<structure>))))
    (dolist (class '(<path> <boundary> <text> <sref> <aref>))
      (add-element structure (make-instance class)))
    library))

