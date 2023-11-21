(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "cd-context-plus" "cl-vectors" "bt-semaphore")))


(defpackage :cl-gdsfeel/iup-gui
  (:use #:cl
	#:alexandria
	#:cl-geometry2
	#:cl-gdsfeel/geom
	#:cl-gdsfeel/viewport
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
(defvar *viewport* nil)
(defvar *draw-by-cd* t)
(defvar *trace-button-event* nil)

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
	   (item-draw-by-wd (iup:item :title "wd"
				      :action (lambda (handle)
						(declare (ignore handle))
						(setf *draw-by-cd* nil)
						(invalidate-canvas)
						iup:+default+)))
	   (item-draw-by-cd (iup:item :title "cd"
				      :action (lambda (handle)
						(declare (ignore handle))
						(setf *draw-by-cd* t)
						(invalidate-canvas)
						iup:+default+)))

	   (file-menu (iup:menu (list 
				 item-open
				 item-layout
				 (iup:separator)
				 item-exit)))
	   (debug-menu (iup:menu (list 
				  item-draw-by-wd
				  item-draw-by-cd
				  (iup:separator))))   
	   (sub-menu-file (iup:sub-menu file-menu :title "&File"))
	   (sub-menu-debug (iup:sub-menu debug-menu :title "&Debug"))
	   (menu (iup:menu (list
			    sub-menu-file
			    sub-menu-debug)))
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
			 :map_cb 'canvas-map-cb
			 :resize_cb 'canvas-resize-cb
			 :unmap_cb 'canvas-unmap-cb
			 :motion_cb 'canvas-motion-cb
			 :button_cb 'canvas-button-cb
			 :action 'canvas-redraw-cb
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
  (declare (ignore handle width))
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
  (declare (ignore self))
  (print (list text item state))
  (print nil)
  (when (zerop state)
    (return-from struclist-action-cb iup:+ignore+))
  (activate-structure (child-named (library *inform*) text))
  iup:+default+)


(defun activate-structure (structure)
  (setq *structure* structure)
  (invalidate-canvas)
  (setf (iup:attribute (iup:handle "elementlist") 1) nil)
  (loop for each in (coerce (elements structure) 'list)
	for i from 1
	do (setf (iup:attribute (iup:handle "elementlist") i)
		 (format nil "~s" each))))


(defun elementlist-action-cb (self text item state)
  (declare (ignore self text))
  (when (zerop state)
    (return-from elementlist-action-cb iup:+default+))
  ;; (print (list text item state))
  (activate-element (elt (children *structure*) (1- item)))
  iup:+default+)


(defun activate-element (element)
  (setq *element* element)
  (invalidate-canvas))


(defun invalidate-canvas ()
  (iup:redraw (iup:handle "dialog") t))


(defgeneric ad/stroke (element canvas)
  (:documentation "stroke by wd canvas"))


(defgeneric ad/stroke-cd (element canvas)
  (:documentation "stroke by cd canvas"))


(defun ad/stroke-coords (coords canvas path-mode)
  (cd:with-vertices (canvas path-mode)
    (mapcar
     (lambda (p) (wd:vertex canvas (car p) (cdr p)))
     coords)))


(defun ad/stroke-points (points canvas path-mode)
  (cd:with-vertices (canvas path-mode)
    (mapcar
     (lambda (wp)
       (let ((dp (world->device *viewport* (as-point wp))))
	 (cd:vertex canvas (x dp) (y dp))))
     points)))


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


(defmethod ad/stroke-cd (element canvas)
  (ad/stroke-points (points element)
		    canvas
		    :path-mode-open-lines))


(defmethod ad/stroke ((element <boundary>) canvas)
  (ad/stroke-coords (coords element)
		    canvas
		    :path-mode-closed-lines))


(defmethod ad/stroke-cd ((element <boundary>) canvas)
  (ad/stroke-points (points element)
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


(defmethod ad/stroke-cd ((element <path>) canvas)
  ;; stroke path center
  (ad/stroke-points (coords element)
		    canvas
		    :path-mode-open-lines)
  ;; stroke path outline
  (ad/stroke-points (ad/path-outline-coords
		     (coords element)
		     (path-width element)
		     (pathtype element)) 
		    canvas
		    :path-mode-closed-lines))


(defun ad/stroke-ref (element canvas) 
  (let ((origin (car (coords element))))
    (wd:mark canvas (car origin) (cdr origin))))


(defun mark-world (canvas wpt)
  (let ((dp (world->device *viewport* (as-point wpt))))
    (cd:mark canvas (x dp) (y dp))))


(defun mark-world-points (canvas points)
  (dolist (wpt points)
    (mark-world canvas wpt)))


(defun ad/stroke-ref-cd (element canvas)
  (mark-world canvas (first (points element))))



(defmethod ad/stroke ((element <sref>) canvas)
  (setf (cd:mark-type canvas) :mark-hollow-diamond)
  (setf (cd:foreground canvas) (if (transform-effective-p element)
				   cd:+blue+
				   cd:+black+))
  (ad/stroke-ref element canvas)
  (setf (cd:foreground canvas) cd:+magenta+)
  (let ((b (data-bounds element)))
    (wd:rect canvas (aref b 0 0) (aref b 1 0) (aref b 0 1) (aref b 1 1))))


(defmethod ad/stroke-cd ((element <sref>) canvas)
  (setf (cd:mark-type canvas) :mark-hollow-diamond)
  (setf (cd:foreground canvas) (if (transform-effective-p element)
				   cd:+blue+
				   cd:+black+))
  (ad/stroke-ref-cd element canvas)
  (setf (cd:foreground canvas) cd:+magenta+)
  (let ((b (world->device *viewport* (data-bbox element))))
    (cd:rect canvas (x-min b) (x-max b) (y-min b) (y-max b))))


(defmethod ad/stroke ((element <aref>) canvas)
  (setf (cd:mark-type canvas) :mark-x)
  (setf (cd:foreground canvas) (if (transform-effective-p element)
				   cd:+blue+
				   cd:+black+))
  (ad/stroke-ref element canvas))


(defmethod ad/stroke-cd ((element <aref>) canvas)
  (setf (cd:mark-type canvas) :mark-x)
  (setf (cd:foreground canvas) (if (transform-effective-p element)
				   cd:+blue+
				   cd:+black+))
  (ad/stroke-ref-cd element canvas))


(defun ad/extent (element)
  (let ((bounds (data-bounds element)))
    (cons 
     (- (aref bounds 1 0)
	(aref bounds 0 0))
     (- (aref bounds 1 1)
	(aref bounds 0 1)))))


(defun stroke-structure (structure canvas &optional (stroke-proc #'ad/stroke))
  (loop for each in (coerce (children structure) 'list)
	do (setf (cd:foreground canvas)
		 (if (null *element*)
		     cd:+black+
		     (if (eq *element* each) cd:+red+ cd:+black+)))
	   (funcall stroke-proc each canvas)))


(defun draw-structure-wd (structure canvas)
  ;; (print "draw-structure>>")
  (multiple-value-bind (canvas-w canvas-h w-mm h-mm)
      (cd:size canvas)
    (declare (ignore w-mm h-mm))
    (setf (cd:background canvas) cd:+white+)
    (cd:clear canvas)
    (when (null structure)
      (return-from draw-structure-wd))
    (setf (wd:window canvas)
	  (apply #'values
		 (min-max-bounds-to-fit-canvas
		  (data-bounds structure) canvas-w canvas-h)))
    (setf (wd:viewport canvas)
	  (values 0 (1- canvas-w) 0 (1- canvas-h)))
    (setf (cd:foreground canvas) cd:+black+)
    (stroke-structure structure canvas)))


(defun draw-structure-cd (structure canvas)
  ;; (print "draw-structure>>")
  (setf (cd:background canvas) cd:+white+)
  (cd:clear canvas)
  (when (null structure)
    (return-from draw-structure-cd))
  (setf (cd:foreground canvas) cd:+black+)
  (set-bounds *viewport* (data-bbox structure))
  (stroke-structure structure canvas #'ad/stroke-cd)
  (setf (cd:mark-type canvas) :mark-circle)
  (mark-world canvas (bbox-mid (data-bbox structure)))
  (setf (cd:mark-type canvas) :mark-star)
  (mark-world canvas (p 0 0))
  (setf (cd:mark-type canvas) :mark-hollow-circle)
  (mark-world-points canvas (bbox-points (data-bbox structure)))
  )


(defun draw-structure (structure canvas)
  (if *draw-by-cd*
      (draw-structure-cd structure canvas)
      (draw-structure-wd structure canvas)))


(defun draw-prototype (canvas)
  (cd:line canvas 0 0 (port-width *viewport*) (port-height *viewport*))
  (if *structure*
      (cd:line canvas 0 0 (port-center-x *viewport*) (port-center-y *viewport*))
      )
  )

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


(defun canvas-motion-cb (handle x y status)
  (when *trace-button-event* 
    (print (list :handle handle :x x :y y :status (iup:status-plist status))))
  iup:+default+)


(defun canvas-button-cb (handle button pressed x y status)
  (when *trace-button-event* 
    (print (list :handle handle :button button :pressed pressed
		 :x x :y y :status (iup:status-plist status))))
  iup:+default+)


(defun canvas-resize-cb (handle width height)
  (declare (ignore handle))
  (print (cons width height))
  (unless *viewport*
    (setf *viewport* (make-instance '<viewport> :width width
						:height height)))
  (setf (port-width *viewport*) width)
  (setf (port-height *viewport*) height)
  (damage-transform *viewport*)  
  iup:+default+)


(defun canvas-redraw-cb (handle x y)
  (declare (ignore handle x y))
  (cd:activate *canvas*)
  (draw-structure *structure* *canvas*)
  (draw-prototype *canvas*)
  (cd:flush *canvas*)
  iup:+default+)


(defun canvas-map-cb (handle)
  (setf *canvas* (cd:create-canvas
		  (iup-cd:context-iup-dbuffer-rgb) handle))
  iup:+default+)


(defun canvas-unmap-cb (handle)
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


(defun mark (x y)
  (unless (or (null *viewport*) (null *canvas*))
    (cd:activate *canvas*)
    (setf (cd:mark-type *canvas*) :mark-plus)
    (cd:flush *canvas*)

    )
  )

(start-gds-thread)

