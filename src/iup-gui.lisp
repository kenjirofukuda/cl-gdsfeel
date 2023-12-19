(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "cd-context-plus" "cl-vectors" "bt-semaphore")))


(defpackage :cl-gdsfeel/iup-gui
  (:use #:cl
	#:alexandria
	#:cl-geometry2
	#:cl-colors2
	#:cl-gdsfeel/geom
	#:cl-gdsfeel/viewport
	#:cl-gdsfeel/model
	#:cl-gdsfeel/stream)
  (:shadowing-import-from :cl-gdsfeel/model :structure)
  (:export #:main
	   #:entry-point))

(in-package :cl-gdsfeel/iup-gui)

(declaim (inline bbox-width bbox-height))

(define-constant +default-window-title+ "GdsFeel" :test #'string=)

(defvar *canvas* nil)
(defvar *inform* nil)
(defvar *structure* nil)
(defvar *element* nil)
(defvar *viewport* nil)
(defvar *layer-color-assoc* nil)
(defvar *fast-drawing* nil)
(defvar *thread-fast-drawing* nil)
(defvar *last-draw-timestamp* nil)

(defparameter *bool-keys*
  '(pixel-perfect
    trace-button-event
    trace-motion-event
    trace-wheel-event))
(defvar *bool-table* (make-hash-table))


(defun main-window ()
  (cd:use-context-plus t)
  (setup-bool-table)
  (setf *last-draw-timestamp* (local-time:now))
  (iup:with-iup ()
    (let* ((item-open (iup:item :title (format nil "&Open...~CCtrl+O" #\Tab)
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
	   (debug-menu (iup:menu (debug-menu-items)))
	   (sub-menu-file (iup:sub-menu file-menu :title "&File"))
	   (sub-menu-debug (iup:sub-menu debug-menu :title "&Debug"))
	   (menu (iup:menu (list
			    sub-menu-file
			    sub-menu-debug)))
	   (struclist
	     (iup:list :expand :yes 
		       ;;:maxsize "x800"
		       :scrollbar :yes
		       :action 'struclist-action-cb
		       :handlename "struclist"))
	   (elementlist
	     (iup:list :expand :yes 
		       ;;:maxsize "x800"
		       :scrollbar :yes
		       :action 'elementlist-action-cb
		       :handlename "elementlist"))
	   (canvas
	     (iup:canvas :expand :yes
			 ;;:maxsize "x800"
			 :minsize "512x342"
			 :map_cb 'canvas-map-cb
			 :resize_cb 'canvas-resize-cb
			 :unmap_cb 'canvas-unmap-cb
			 :motion_cb 'canvas-motion-cb
			 :button_cb 'canvas-button-cb
			 :wheel_cb 'canvas-wheel-cb
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
	     (iup:hbox (list (iup:frame (iup:label :expand :horizontal
						   :handlename "statusbar")))))
	   (vbox
	     (iup:vbox (list hbox statusbar)
		       :gap "5"
		       :margin "5x5"))
	   (dialog
	     (iup:dialog vbox :title +default-window-title+
			      :size "halfxhalf"
			      :menu "menu"
			      :handlename "dialog"
			      :shrink :yes
			      :resize_cb 'dialog-resize-cb)))
      (setf (iup:handle "menu") menu)
      (iup:show dialog)
      (iup:main-loop))))


(defun debug-menu-items ()
  (mapcar 'make-bool-item *bool-keys*))


(defun setup-bool-table ()
  (dolist (each *bool-keys*)
    (setf (gethash each *bool-table*) nil)))


(defun make-bool-item (sym &optional (initial :off))
  (let* ((str (symbol-name sym))
	 (*package* #.*package*)
	 (cb-name (intern (string-upcase (concatenate 'string str "-cb")) *package*))
	 (title (substitute #\space #\- str)))
    (iup:item :title title
	      :autotoggle :yes
	      :value initial
	      :action cb-name)))


(defun item-checked (handle)
  (string= (iup:attribute handle :value) "ON"))


(defun pixel-perfect-cb (handle)
  (setf (gethash 'pixel-perfect *bool-table*) (item-checked handle))
  (setf (device-pixel-convertor *viewport*)
	(if (gethash 'pixel-perfect *bool-table*)
	    'truncate
	    'identity))
  (invalidate-canvas)
  iup:+default+)


(defun trace-button-event-cb (handle)
  (setf (gethash 'trace-button-event *bool-table*) (item-checked handle))
  iup:+default+)


(defun trace-motion-event-cb (handle)
  (setf (gethash 'trace-motion-event *bool-table*) (item-checked handle))
  iup:+default+)


(defun trace-wheel-event-cb (handle)
  (setf (gethash 'trace-wheel-event *bool-table*) (item-checked handle))
  iup:+default+)


(defun set-status (msg)
  (setf (iup:attribute (iup:handle "statusbar") :title) msg))


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
	       (setf *layer-color-assoc* (allocate-layer-colors (library *inform*) 'rgb->cd-color))
	       (display-structure-names
		(library *inform*) (iup:handle "struclist")))))
      (iup:destroy dialog)))
  iup:+default+)


(defun struclist-action-cb (self text item state)
  (declare (ignore self))
  (print (list text item state))
  (print nil)
  (when (zerop state)
    (return-from struclist-action-cb iup:+default+))
  (activate-structure (child-named (library *inform*) text))
  iup:+default+)


(defun activate-structure (structure)
  (setq *structure* structure)
  (when *viewport* 
    (set-bounds *viewport* (data-bbox structure)))
  (invalidate-canvas)
  (setf (iup:attribute (iup:handle "elementlist") 1) nil)
  (loop for each in (coerce (elements structure) 'list)
	for i from 1
	do (setf (iup:attribute (iup:handle "elementlist") i)
		 (display-name each))))


(defmethod display-name ((el <element>))
  (let ((s (symbol-name (type-of el))))
    (subseq s 1 (1- (length s)))))


(defmethod display-name ((el <named-container>))
  (format nil "~a (\"~a\")" (call-next-method el) (name el)))


(defmethod display-name ((el <sref>))
  (format nil "~a (\"~a\")" (call-next-method el) (refname el)))


(defmethod display-name ((el <text>))
  (format nil "~a (\"~a\")" (call-next-method el) (contents el)))


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
  #-windows (iup:redraw (iup:handle "dialog") t) ;; bug fix
  #+windows (iup:redraw (iup:handle "canvas") t))


(defgeneric ad/stroke-cd (element canvas)
  (:documentation "stroke by cd canvas"))


(defun ad/stroke-points (points canvas path-mode)
  (cd:with-vertices (canvas path-mode)
    (mapcar
     (lambda (wp)
       (let ((dp (world->device *viewport* (as-point wp))))
	 (cd:vertex canvas (x dp) (y dp))))
     points)))


(defun ad/stroke-bbox (bbox canvas)
  (ad/stroke-points (bbox-points bbox)
		    canvas
		    :path-mode-closed-lines))


(defmethod ad/stroke-cd (element canvas)
  (ad/stroke-points (points element)
		    canvas
		    :path-mode-open-lines))


(defmethod ad/stroke-cd :before ((element <primitive>) canvas)
  (setf (cd:foreground canvas) (layer-color (layer element))))


(defmethod ad/stroke-cd ((element <boundary>) canvas)
  (ad/stroke-points (points element)
		    canvas
		    :path-mode-closed-lines))


(defmethod ad/stroke-cd ((element <path>) canvas)
  ;; stroke path center
  (unless *fast-drawing* 
    (ad/stroke-points (points element)
		      canvas
		      :path-mode-open-lines))
  ;; stroke path outline
  (ad/stroke-points (outline-coords element) 
		    canvas
		    :path-mode-closed-lines))


(defun mark-world (canvas wpt)
  (let ((dp (world->device *viewport* (as-point wpt))))
    (cd:mark canvas (x dp) (y dp))))


(defun mark-world-points (canvas points)
  (dolist (wpt points)
    (mark-world canvas wpt)))


(defun ad/stroke-ref-cd (element canvas)
  (mark-world canvas (first (points element))))


(defmethod ad/stroke-cd ((element <sref>) canvas)
  (with-transform *viewport* (ref-transform element)
    (stroke-structure (ref-structure element) canvas #'ad/stroke-cd)))


(defmethod ad/stroke-cd ((element <aref>) canvas)
  (dolist (each (repeated-transform element)) 
    (with-transform *viewport* each
      (stroke-structure (ref-structure element) canvas #'ad/stroke-cd))))


(defun stroke-structure (structure canvas &optional (stroke-proc #'ad/stroke-cd))
  (let ((elist (coerce (children structure) 'list)))
    (when (port-stack-empty-p *viewport*)
      (setf elist (clip-elements (get-bounds *viewport*) elist)))
    (dolist (each elist)
      (let* ((long-side (max (bbox-width (data-bbox each))
			     (bbox-height (data-bbox each))))
	     (pix-size (device-size *viewport* long-side))
	     (drawable (> pix-size (if *fast-drawing* 10.0 2.0))))
	(when drawable
	  (funcall stroke-proc each canvas))))))


(defun clip-elements (bbox elst)
  (remove-if-not (lambda (e) (bounding-boxes-intersect-p bbox (data-bbox e))) elst))


(defun draw-structure-cd (structure canvas)
  (setf (cd:background canvas) cd:+black+)
  (cd:clear canvas)
  (when (null structure)
    (return-from draw-structure-cd))
  (stroke-structure structure canvas #'ad/stroke-cd))


(defun draw-structure (structure canvas)
  (identity (draw-structure-cd structure canvas)))


(defun fire-fast-drawing ()
  (setf *fast-drawing* t)
  (when (and *thread-fast-drawing* (bt:thread-alive-p *thread-fast-drawing*))
    (bt:destroy-thread *thread-fast-drawing*)
    (setf *thread-fast-drawing* nil))
  (setf *thread-fast-drawing* (bt:make-thread (lambda ()
						(sleep 0.2)
						(setf *fast-drawing* nil))
					      :name "first-drawing")))


(defun cd-point (x y)
  (p x (truncate (cd:invert-y-axis *canvas* y))))


(defun canvas-motion-cb (handle x y status)
  (when (gethash 'trace-motion-event *bool-table*) 
    (print (list :handle handle :x x :y y :status (iup:status-plist status))))
  (let ((cp (cd-point x y)))
    (set-status (with-output-to-string (s)
		  (format s "(H: ~5d, V: ~5d)" (x cp) (y cp))
		  (unless (null *viewport*)
		    (let ((wp (device->world *viewport* cp)))
		      (format s "   (X: ~10,4f, Y: ~10,4f)" (x wp) (y wp)))))))
  iup:+default+)


(defun canvas-button-cb (handle button pressed x y status)
  (when (gethash 'trace-button-event *bool-table*)
    (print (list :handle handle :button button :pressed pressed
		 :x x :y y :status (iup:status-plist status))))
  iup:+default+)


(defun canvas-wheel-cb (handle delta x y status)
  (when (gethash 'trace-wheel-event *bool-table*)
    (print (list :handle handle :delta delta 
		 :x x :y y :status status)))
  (let ((cp (cd-point x y)))
    (whell-zoom *viewport* cp delta)
    (unless *fast-drawing*
      (invalidate-canvas))
    (fire-fast-drawing))
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
  (print "-------------------------------------------")
  (when *last-draw-timestamp*
    (let* ((diff (local-time:timestamp-difference (local-time:now) *last-draw-timestamp*)))
      (print diff)
      (when (> diff 0.1d0)
	(time (progn
		(cd:activate *canvas*)
		(draw-structure *structure* *canvas*)
		(cd:flush *canvas*)))
	(setf *last-draw-timestamp* (local-time:now)))))
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


(defun my/range (start end)
  (loop for i from start below end collect i))


(defun color-wheel (this-many sat bri hue)
  (let ((step (/ 360 (max this-many 1))))
    (mapcar (lambda (i) (hsv (+ hue (* i step)) sat bri))
	    (my/range 0 this-many))))


(defun zip-cons (xs ys) (mapcar #'cons xs ys))


(defun unit->255 (unit)
  (truncate (* 255 unit)))


(defun rgb->cd-color (rgb-color)
  (cd:encode-color (unit->255 (rgb-red rgb-color))
		   (unit->255 (rgb-green rgb-color))
		   (unit->255 (rgb-blue rgb-color))))


(defun allocate-layer-colors (lib &optional (convertor 'identity))
  (let* ((layers (used-layer-numbers lib))
	 (colors (mapcar #'as-rgb (color-wheel (length layers) 0.7 1.0 0))))
    (zip-cons layers (mapcar (lambda (v) (funcall convertor v)) colors))))


(defun layer-color (n)
  (let ((pair (assoc n *layer-color-assoc*)))
    (if pair
	(cdr pair)
	cd:+white+)))


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


(defun start-gds-thread (&key (profile nil))
  (let* ((thread-names (mapcar (lambda (th)
				 (bt:thread-name th))
			       (bt:all-threads)))
	 (targets (remove-if-not (lambda (name) (string= name "gds")) thread-names)))
    (unless (null targets)
      (print "gds already running")
      (return-from start-gds-thread))
    (bt:make-thread (lambda ()
		      (when profile
			(sb-profile:profile
			 "CL-GDSFEEL/IUP-GUI"
			 "CL-GDSFEEL/MODEL"
			 "CL-GDSFEEL/STREAM"
			 "CL-GDSFEEL/GEOM"
			 "2D-GEOMETRY"
			 "NET.TUXEE.PATHS"
			 "3D-MATRICES"))
		      (entry-point)
		      (when profile
			(sb-profile:report)))
		    :name "gds")))


(defun quit-gds-thread ()
  (dolist (each (bt:all-threads))
    (let ((name (bt:thread-name each)))
      (if (string-equal name "gds")
	  (bt:destroy-thread each)))))
