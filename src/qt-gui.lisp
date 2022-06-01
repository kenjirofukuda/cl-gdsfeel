(defpackage :cl-gdsfeel/qt-gui
  (:use #:cl+qt
	#:cl-gdsfeel/model
	#:cl-gdsfeel/stream
	
	#:trivial-main-thread)
  (:export #:main))


(in-package :cl-gdsfeel/qt-gui)
(in-readtable :qtools)

(define-widget <main-window> (QMainWindow)
  ())

(define-subwidget (<main-window> menu-bar) (q+:make-qmenubar <main-window>))

(define-subwidget (<main-window> contents) (q+:make-qwidget <main-window>))

(define-subwidget (<main-window> go) (q+:make-qpushbutton "Go!" <main-window>))

(define-subwidget (<main-window> layout) (q+:make-qvboxlayout <main-window>)
  (q+:add-widget layout menu-bar)
  (q+:add-widget layout contents)
  (q+:add-widget layout go)
  )

(define-menu (<main-window> File)
  (:item ("Open..." (ctrl o))
	 (princ "open")))


(trivial-main-thread:with-body-in-main-thread ()
  (with-main-window (window (make-instance '<main-window>))))

(defun main (&rest args)
  (princ "Hoge"))

(main '())
