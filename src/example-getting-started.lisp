;;;; Example Getting Started (2021-5-13)

(in-package :gtk-example)

(defun example-gettting-started ()
  (within-main-loop
    (let (;; Created a toplevel window with a title and a default witdth.
	  (window (make-instance 'gtk-window
				 :type :toplevel
				 :title "Getting started"
				 :default-width 250)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
			(lambda (widget)
			  (declare (ignore widget))
			  (leave-gtk-main)))
      ;; Show the window.
      (gtk-widget-show-all window))))
