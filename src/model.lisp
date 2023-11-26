(defpackage cl-gdsfeel/model
  (:use #:cl
	#:alexandria
	#:local-time
	#:clem
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
   :lookup-affine-transform
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
   :lookup-offsets
   :lookup-repeated-transform)
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

(defclass <reference> (<element> <strans>) ())

(defclass <sref> (<reference>)
  ((refname   :type string       :initform "" :accessor refname)))

(defclass <aref> (<sref>)
  ((x-step :type integer :initform 0 :accessor x-step)
   (y-step :type integer :initform 0 :accessor y-step)
   (row-count :type integer :initform 0 :accessor row-count)
   (column-count :type integer :initform 0 :accessor column-count)))


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
    :accessor elements)
   ;; (elements
   ;;  :type vector
   ;;  :initform (alloc-typed-vector '<element>)
   ;;  :accessor elements)
   ))

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
   ;; (structures
   ;;  :type vector
   ;;  :initform (alloc-typed-vector '<structure>)
   ;;  :accessor structures)

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
	 (tx (lookup-affine-transform element)))
    (points->bbox (mapcar (lambda (each) (transform-point tx each))
			  (bbox-points ref-bbox)))))


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


(defmethod lookup-affine-transform ((reference <reference>))
  (let ((m (clem:make-affine-transformation :x-shift (first (xy reference))
					    :y-shift (second (xy reference))
					    :x-scale (mag reference)
					    :y-scale (mag reference)
					    :theta (* +radians-per-degree+ (angle reference)))))
    (when (reflected-p reference)
      (setf (mref m 0 1) (- (mref m 0 1)))
      (setf (mref m 1 1) (- (mref m 1 1))))
    m))


(defmethod lookup-offsets ((aref <aref>))
  (flatten (loop for x-index below (column-count aref)
		 collect (loop
			   for y-index below (row-count aref)
			   collect (p (* x-index (x-step aref)) (* y-index (y-step aref)))))))


(defmethod lookup-repeated-transform ((aref <aref>))
  (let ((tx (lookup-affine-transform aref))
	(offsets (lookup-offsets aref)))
    (mapcar (lambda (offset)
	      (let ((otx (clem:make-affine-transformation :x-shift (x offset)
							  :y-shift (y offset))))
		(clem:m* tx otx)))
	    offsets)))


(defmethod child-names ((container <named-container>))
  (map 'vector #'name (children container)))


(defmethod child-named ((container <named-container>) name)
  (find-if
   (lambda (x) (equalp (name x) name))
   (children container)))


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


(defmethod resolved ((sref <sref>))
  (child-named (library (structure sref)) (refname sref)))


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
  ;;  (vector-push-extend structure (structures library))
  (push structure (structures library))
  structure)


(defun fixture-library ()
  (let* ((library (make-instance '<library>))
         (structure (add-structure library (make-instance '<structure>))))
    (dolist (class '(<path> <boundary> <text> <sref> <aref>))
      (add-element structure (make-instance class)))
    library))

