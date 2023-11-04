(defpackage cl-gdsfeel/model
  (:use #:cl
	#:local-time
	#:clem
	#:cl-gdsfeel/geom)
  (:shadow :structure)
  (:export
   :alloc-typed-vector
   :vector-last

   :<tree-node>
   :children
   :resolved
   :resolved-children
   :data-bounds
   
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

   :<boundary>

   :<text>
   :contents

   :<reference>
   :<sref>
   :refname

   :<aref>
   )
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
  ((cached-data-bounds :initform nil :accessor cached-data-bounds)))

(defgeneric data-bounds (object))

(defmethod data-bounds ((object <tree-node>))
  (unless (cached-data-bounds object)
    (setf (cached-data-bounds object) (calc-bounds-2a object)))
  (cached-data-bounds object))


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

(defclass <aref> (<sref>) ())


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

(defmethod coords-2a ((element <element>))
  (aops:reshape (coerce (xy element) 'vector) '(t 2)))

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

(defgeneric calc-bounds-2a (object))


(defmethod calc-bounds-2a ((element <element>))
  (let* ((coords (coords-2a element))
	 (min (aops:margin
	       (lambda (col)
		 (reduce #'min col))
	       coords 0))
	 (max (aops:margin
	       (lambda (col)
		 (reduce #'max col))
	       coords 0)))
    (aops:combine (vector min max))))

;; (defmethod calc-bounds-2a ((element <sref>))
;;   (let* ((s-local-bounds (resolved element))
;; 	 (off-x (first (xy element)))
;; 	 (off-y (first (xy element)))))
;;   )


(defmethod calc-bounds-2a ((structure <structure>))
  (let ((bounds nil)
	(xmins '())
	(ymins '())
	(xmaxs '())
	(ymaxs '()))    
    (loop for each in (children structure) 
	  do (setq bounds (data-bounds each))
	     (push (aref bounds 0 0) xmins)
	     (push (aref bounds 0 1) ymins)
	     (push (aref bounds 1 0) xmaxs)
	     (push (aref bounds 1 1) ymaxs))
    (make-array '(2 2) :initial-contents
		(list 
		 (list  (apply #'min xmins)
			(apply #'min ymins))
		 (list  (apply #'max xmaxs)
			(apply #'max ymaxs))))))



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


;; (defmethod print-tree ((library <library>))
;;   (dolist (each (no-leaf-children library))
;;     ))


(defmethod resolved ((structure <structure>))
  structure)


(defmethod resolved ((element <element>))
  nil)


(defmethod resolved ((sref <sref>))
  (child-named (structure sref) (refname sref)))


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

