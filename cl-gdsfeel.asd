(defsystem "cl-gdsfeel"
  :version "0.1.0"
  :author "kenjirofukuda@gmail.com"
  :license ""
  :depends-on ("alexandria"
	       "osicat"
	       "flexi-streams"
               "nibbles"
               "array-operations"
               "local-time"
               "cl-slice"
	       ;; "cl-cffi-gtk"
	       ;; "qtools"
	       ;; "qtcore"
	       ;; "qtgui"
	       "cl-vectors"
	       "iup"
	       "cd"
	       "im"
	       "iup-cd"
	       "bt-semaphore"
	       )
  :components ((:module "src"
                :components
		((:file "iup-gui"
		  :depends-on ("model"
			       "stream"))
		 (:file "stream" :depends-on ("model"))
		 (:file "model"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-gdsfeel/tests"))))

(defsystem "cl-gdsfeel/tests"
  :author ""
  :license ""
  :depends-on ("cl-gdsfeel"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "stream"))))
  :description "Test system for cl-gdsfeel"
  :perform (test-op (op c) (symbol-call :rove :run c)))
