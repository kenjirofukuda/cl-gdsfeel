(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib))

(in-package :raylib-user)

(defun basic2 ()
  (with-window (480 320 "basic2")
    (set-target-fps 60)
    (loop
      (if (window-should-close) (return))
      (with-drawing
        (draw-entry)        
        ))))

(defun draw-entry ()
  (clear-background raylib:+lightgray+)
  (draw-fps 20 21)
  (draw-text "cl-raylib3" 322 200 20 +red+)
  )


(basic2)
