(require 'ltk)
(use-package :ltk)

(with-ltk ()
  (grid
   (make-instance 'button :text "Hello world")
   0 0))
