(defpackage cl-gdsfeel/gtk-gui
  (:use
   :cl
   :cl-gdsfeel/model
   :cl-gdsfeel/stream
   
   :gtk
   :gdk
   :gdk-pixbuf
   :gobject
   :glib
   :gio
   :pango
   :cairo)
  (:export
   :open-stream-format-dialog))

(in-package :cl-gdsfeel/gtk-gui)

(defparameter *window* nil)
(defparameter *inform* nil)

(defun open-stream-format-dialog ()
  (print "Now Open")
  (within-main-loop
    (setf (gtk-settings-gtk-shell-shows-menubar (gtk-settings-get-default))
	  nil)
    (let (
	  ;; Created a toplevel window with a title and a default witdth.
	  (window (make-instance 'gtk-window
				 :type :toplevel
				 :title "GDS Inspector"
				 :default-width 425
				 :default-height 250))
	  (vbox (gtk-box-new :vertical 0)))
      (let ((menu-bar (gtk-menu-bar-new))
	    (file-item (gtk-menu-item-new-with-label "File")))
	;; Add the menu bar to main container
	(gtk-container-add vbox menu-bar)
	(gtk-menu-shell-append menu-bar file-item)
	(let ((file-menu (gtk-menu-new)))
	  (setf (gtk-menu-item-submenu file-item) file-menu)
	  (let ((open-item (gtk-menu-item-new-with-label "Open")))
	    (gtk-menu-shell-append file-menu open-item)
	    (g-signal-connect open-item "activate" 'choose-gds-file)))
	)
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
			(lambda (widget)
			  (declare (ignore widget))
			  (leave-gtk-main)
			  (setq *window* nil)))
      (gtk-container-add window vbox)
      ;; Show the window.
      (setq *window* window)
      (gtk-widget-show-all window))))

(defun choose-gds-file (widget)
  (declare (ignore widget))
  (let ((dialog (gtk-file-chooser-dialog-new "Choose GDS file"
                                             nil
                                             :open
                                             "gtk-open" :accept
                                             "gtk-cancel" :cancel)))
    (when (eq :accept (gtk-dialog-run dialog))
      (let ((gds-file #P"")
	    (selected-path (gtk-file-chooser-get-filename dialog))) 
	(format t "Open file ~A~%"
		selected-path)
	(setq gds-file (pathname selected-path))
	(setq *inform*
	      (make-instance '<inform>
			     :path gds-file))
	(run *inform*)))
    (gtk-widget-destroy dialog)))


;; NOTE custom filter does not impelemented
;; (defun stream-only-filter (filter-info)
;;   (let ((display-name (gtk-file-filter-info-display-name filter-info)))  
;;     (cl-gdsfeel/stream::stream-format-p display-name))



;;;
;;; (load "~/common-lisp/register-local-project.lisp")
;;; (ql:quickload :cl-gdsfeel/gui)
;;; (in-package: cl-gdsfeel/gui)
;;; (open-stream-format-dialog)

