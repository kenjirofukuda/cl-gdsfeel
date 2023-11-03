(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "cd-context-plus" "cl-vectors" "bt-semaphore")))


(defpackage :cl-gdsfeel/iup-gui
  (:use #:cl
	#:alexandria
	#:cl-gdsfeel/model
	#:cl-gdsfeel/stream)
  (:shadowing-import-from :cl-gdsfeel/model :structure)
  (:export #:main
	   #:entry-point))

(in-package :cl-gdsfeel/iup-gui)

(define-constant +default-window-title+ "GdsFeel" :test #'string=)
(defparameter *canvas* nil)

(defvar *inform* nil)
(defvar *structure* nil)
(defvar *element* nil)

(defun main-window ()
  (cd:use-context-plus t)
  (iup:with-iup ()
    (let* (
	   (item-open (iup:item :title (format nil "&Open...~CCtrl+O" #\Tab)
				:image "IUP_FileOpen"
				:action 'open-stream-format-dialog))
	   (item-layout (iup:item :title (format nil "&Layout ~CCtrl+L" #\Tab)
				  :action (lambda (handle)
					    (declare (ignore handle))
					    (iup:show (iup:layout-dialog nil)))))
	   (item-exit (iup:item :title "E&xit"
				:action (lambda (handle)
					  (declare (ignore handle))
					  iup:+close+)))
	   (file-menu (iup:menu (list 
				 item-open
				 item-layout
				 (iup:separator)
				 item-exit)))
	   (sub-menu-file (iup:sub-menu file-menu :title "&File"))
	   (menu (iup:menu (list
			    sub-menu-file)))
	   (struclist
	     (iup:list :expand :yes 
		       :maxsize "x800"
		       :scrollbar :yes
		       :action 'struclist-action-cb
		       :handlename "struclist"))
	   (elementlist
	     (iup:list :expand :yes 
		       :maxsize "x800"
		       :scrollbar :yes
		       :action 'elementlist-action-cb
		       :handlename "elementlist"))
	   (canvas
	     (iup:canvas :expand :yes
			 :maxsize "x800"
			 :map_cb 'canvas-map
			 :unmap_cb 'canvas-unmap
			 :action 'canvas-redraw			 
			 :handlename "canvas"))
	   (hbox
	     (iup:hbox (list
			(iup:sbox struclist :handlename "sbox1")	
			(iup:sbox elementlist :handlename "sbox2")	
			canvas)
		       :handlename "contents"
		       :expandchildren :no))
	   (statusbar
	     (iup:hbox (list (iup:frame (iup:label :expand :horizontal)))))
	   (vbox
             (iup:vbox (list hbox statusbar)
		       :gap "5"
		       :margin "5x5"))
	   (dialog
             (iup:dialog vbox :title +default-window-title+
			      :size "halfxhalf"
			      :menu "menu"
			      :handlename "dialog"
			      :shrink :no
			      :resize_cb 'dialog-resize-cb)))
      (setf (iup:handle "menu") menu)
      (iup:show dialog)
      (iup:main-loop))))


(defun dialog-resize-cb (handle width height)
  (declare (ignore handle))
  (print (cons width height))
  (mapcar (lambda (handle-name)
	    (setf (iup:attribute (iup:handle handle-name) :maxsize)
		  (format nil "x~d" (- height 50))) )
	  (list
	   "struclist"
	   "elementlist"
	   "canvas"
	   "sbox1"
	   "sbox2"
	   "contents"))
  iup:+default+)

(defun display-structure-names (library slist)
  (setf (iup:attribute (iup:handle "elementlist") 1) nil)
  (setf (iup:attribute slist 1) nil)
  (loop for each in (coerce (child-names library) 'list)
	for i from 1
	do (setf (iup:attribute slist i) each))
  (iup:refresh-children (iup:handle "dialog"))
  iup:+default+)

(defun initial-directory ()
  (and *inform*
       (slot-value *inform* 'path)
       (uiop:pathname-parent-directory-pathname (slot-value *inform* 'path))))

(defun open-stream-format-dialog (handle)
  (declare (ignore handle))
  (let ((dialog (iup:file-dialog
		 :extfilter "GDS file|*.gds;*.gds2;*.sf|All Files|*.*|"
		 :directory (initial-directory))))
    (unwind-protect
         (progn
           (iup:popup dialog iup:+center+ iup:+center+)
	   (let ((selected-path (iup:attribute dialog :value)))	     
             (iup:message +default-window-title+
			  (format nil "Selected ~A" selected-path))
	     (when selected-path
	       (handle-inform (pathname selected-path))
	       (display-structure-names
		(library *inform*) (iup:handle "struclist")))))
      (iup:destroy dialog)))
  iup:+default+)


(defun struclist-action-cb (self text item state)
  (declare (ignore self item))
  (when (zerop state)
    (return-from struclist-action-cb iup:+default+))
  ;; (print (list text item state))
  (activate-structure (child-named (library *inform*) text))
  iup:+default+)


(defun activate-structure (structure)
  (setq *structure* structure)
  (setf (iup:attribute (iup:handle "elementlist") 1) nil)
  (loop for each in (coerce (elements structure) 'list)
	for i from 1
	do (setf (iup:attribute (iup:handle "elementlist") i)
		 (format nil "~s" each)))
  (iup:redraw (iup:handle "canvas") t))


(defun elementlist-action-cb (self text item state)
  (declare (ignore self text))
  (when (zerop state)
    (return-from elementlist-action-cb iup:+default+))
  ;; (print (list text item state))
  (activate-element (elt (children *structure*) (1- item)))
  iup:+default+)


(defun activate-element (element)
  (setq *element* element)
  (iup:redraw (iup:handle "canvas") t))


(defgeneric ad/stroke (element canvas)
  (:documentation "hoge"))


(defun ad/stroke-coords (coords canvas path-mode)
  (cd:with-vertices (canvas path-mode)
    (mapcar
     (lambda (p) (wd:vertex canvas (car p) (cdr p)))
     coords)))


(defun ad/path-outline-coords (coords path-width pathtype)
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


(defmethod ad/stroke (element canvas)
  (ad/stroke-coords (coords element)
		    canvas
		    :path-mode-open-lines))

(defmethod ad/stroke ((element <boundary>) canvas)
  (ad/stroke-coords (coords element)
		    canvas
		    :path-mode-closed-lines))

(defmethod ad/stroke ((element <path>) canvas)
  ;; stroke path center
  (ad/stroke-coords (coords element)
		    canvas
		    :path-mode-open-lines)
  ;; stroke path outline
  (ad/stroke-coords (ad/path-outline-coords
		     (coords element)
		     (path-width element)
		     (pathtype element)) 
		    canvas
		    :path-mode-closed-lines))


(defun ad/stroke-ref (element canvas) 
  (let ((origin (car (coords element))))
    (wd:mark canvas (car origin) (cdr origin))))


(defmethod ad/stroke ((element <sref>) canvas)
  (setf (cd:mark-type canvas) :mark-hollow-diamond)
  (ad/stroke-ref element canvas))


(defmethod ad/stroke ((element <aref>) canvas)
  (setf (cd:mark-type canvas) :mark-x)
  (ad/stroke-ref element canvas))


(defun ad/coords (element)
  (aops:reshape (coerce (xy element) 'vector) '(t 2)))


(defun ad/bounds (element)
  (let* ((coords (ad/coords element))
	 (min (aops:margin
	       (lambda (col)
		 (reduce #'min col))
	       coords 0))
	 (max (aops:margin
	       (lambda (col)
		 (reduce #'max col))
	       coords 0)))
    (aops:combine (vector min max))))


(defun ad/structure-bounds (structure)
  (let ((bounds nil)
	(xmins '())
	(ymins '())
	(xmaxs '())
	(ymaxs '()))    
    (loop for each in (children structure) 
	  do (setq bounds (ad/bounds each))
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


(defun ad/extent (element)
  (let ((bounds (ad/bounds element)))
    (cons 
     (- (aref bounds 1 0)
	(aref bounds 0 0))
     (- (aref bounds 1 1)
	(aref bounds 0 1)))))


(defun stroke-structure (structure canvas)
  (loop for each in (coerce (children structure) 'list)
	do (setf (cd:foreground canvas)
		 (if (null *element*)
		     cd:+black+
		     (if (eq *element* each) cd:+red+ cd:+black+)))
	   (ad/stroke each canvas)))


(defun draw-structure (structure canvas)
  ;; (print "draw-structure>>")
  (multiple-value-bind (canvas-w canvas-h w-mm h-mm)
      (cd:size canvas)
    (declare (ignore w-mm h-mm))
    (setf (cd:background canvas) cd:+white+)
    (cd:clear canvas)
    (when (null structure)
      (return-from draw-structure))
    (setf (wd:window canvas)
	  (apply #'values
		 (min-max-bounds-to-fit-canvas
		  (ad/structure-bounds structure) canvas-w canvas-h)))
    (setf (wd:viewport canvas)
	  (values 0 (1- canvas-w) 0 (1- canvas-h)))
    (setf (cd:foreground canvas) cd:+black+)
    (stroke-structure structure canvas)))

(defun min-max-bounds-to-fit-canvas (bounds width height)
  (let* ((xmin (aref bounds 0 0))
	 (ymin (aref bounds 0 1))
	 (xmax (aref bounds 1 0))
	 (ymax (aref bounds 1 1))
	 (w-width (- xmax xmin))
	 (w-height (- ymax ymin))
	 (x-ratio (/ width  w-width))
	 (y-ratio (/ height  w-height))
	 (scaling (min x-ratio y-ratio))
	 (half-width (/  (/ width scaling) 2.0))
	 (half-height (/  (/ height scaling) 2.0))
	 (x-center (+ xmin (/ w-width 2)))
	 (y-center (+ ymin (/ w-height 2))))
    (list
     (- x-center half-width)
     (+ x-center half-width)
     (- y-center half-height)
     (+ y-center half-height))))


(defun canvas-redraw (handle x y)
  (declare (ignore handle x y))
  (cd:activate *canvas*)
  (draw-structure *structure* *canvas*)
  (cd:flush *canvas*)
  iup:+default+)


(defun canvas-map (handle)
  (setf *canvas* (cd:create-canvas
		  (iup-cd:context-iup-dbuffer-rgb) handle))
  iup:+default+)


(defun canvas-unmap (handle)
  (declare (ignore handle))
  (cd:kill *canvas*)
  iup:+default+)


(defun handle-inform (gds-file)
  (let ((inform-result nil))
    (setq inform-result
	  (make-instance '<inform>
			 :path gds-file))        
    (run inform-result)
    (setq *inform* inform-result)))


(defun message (message)
  (iup:message +default-window-title+ message))


(defun entry-point ()
  #-sbcl (main-window)
  #+sbcl
  (sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (main-window)))


(defun main (&rest args)
  (declare (ignore args))
  (princ "Hage"))

(defun print-thread-info ()
  (let* ((curr-thread (bt:current-thread))
	 (curr-thread-name (bt:thread-name curr-thread))
	 (all-threads (bt:all-threads)))
    (format t "Current thread: ~a~%~%" curr-thread)
    (Format t "Current thread name: ~a~%~%" curr-thread-name)
    (format t "All threads:~% ~{~a~%~}~%" all-threads))
  nil)

(defun start-gds-thread ()
  (bt:make-thread
   (lambda () (entry-point)) :name "gds"))

(defun quit-gds-thread ()
  (dolist (each (bt:all-threads))
    (let ((name (bt:thread-name each)))
      (if (string-equal name "gds")
	  (bt:destroy-thread each)))))

(start-gds-thread)

