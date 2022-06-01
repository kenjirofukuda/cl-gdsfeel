(require 'iup)

(defun list-test ()
  (iup:with-iup ()
    (let*  ((list-1 (iup:list :tip "List 1"  ;; tooltip
                              ;; multiple selection
                              :multiple :yes
                              :expand :yes))
            (list-2 (iup:list :value 2   ;; default index of the selected row
                              :tip "List 2" :expand :yes))
            (list-3 (iup:list :value 9 :tip "List 3" :expand :yes))
            (frame (iup:frame
                    (iup:hbox
                     (progn
                       ;; populate the lists: display integers.
                       (loop for i from 1 upto 10
                             do (setf (iup:attribute list-1 i)
                                      (format nil "~A" i))
                             do (setf (iup:attribute list-2 i)
                                      (format nil "~A" (+ i 10)))
                             do (setf (iup:attribute list-3 i)
                                      (format nil "~A" (+ i 50))))
                       ;; hbox wants a list of widgets.
                       (list list-1 list-2 list-3)))
                    :title "IUP List"))
            (dialog (iup:dialog frame :menu "menu" :title "List example")))

      (iup:map dialog)
      (iup:show dialog)
      (iup:main-loop))))

(defun run-list-test ()
  #-sbcl (hello)
  #+sbcl
  (sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (list-test)))


