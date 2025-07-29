;; Matrix multiplication in SBCL using BLAS
;; Miroslav Urbanek <mu@miroslavurbanek.com>

(load-shared-object "libblas.so.3")

(declaim (inline dgemm))

(define-alien-routine ("dgemm_" dgemm) void
  (transa c-string)
  (transb c-string)
  (m int :copy)
  (n int :copy)
  (k int :copy)
  (alpha double :copy)
  (a (* double))
  (lda int :copy)
  (b (* double))
  (ldb int :copy)
  (beta double :copy)
  (c (* double))
  (ldc int :copy))

(defun pointer (array)
  (sap-alien (sb-sys:vector-sap (array-storage-vector array)) (* double)))

(defun mm (a b)
  (unless (= (array-dimension a 1) (array-dimension b 0))
    (error "Matrix dimensions do not match."))
  (let* ((m (array-dimension a 0))
	 (n (array-dimension b 1))
	 (k (array-dimension a 1))
	 (c (make-array (list m n) :element-type 'double-float)))
    (sb-sys:with-pinned-objects (a b c)
      (dgemm "n" "n" n m k 1d0 (pointer b) n (pointer a) k 0d0 (pointer c) n))
    c))


(defstruct mat3
  (m (make-array '(3 3) :element-type 'double-float
			:initial-contents '((1.0d0 0.0d0 0.0d0) (0.0d0 1.0d0 0.0d0) (0.0d0 0.0d0 1.0d0)))))

(defun mat3* (a b)
  (let* ((c (make-mat3))
	 (n 3))
    (sb-sys:with-pinned-objects (a b c)
      (dgemm "n" "n" n n n 1d0 (pointer (mat3-m b)) n (pointer (mat3-m a)) n 0d0 (pointer (mat3-m c)) n))
    c))

;; (defparameter a (make-array '(2 3) :element-type 'double-float :initial-contents '((2d0 1d0 6d0) (7d0 3d0 4d0))))
;; (defparameter b (make-array '(3 2) :element-type 'double-float :initial-contents '((3d0 1d0) (6d0 5d0) (2d0 3d0))))

;; (format t "a = ~A~%b = ~A~%" a b)

;; (defparameter c (mm a b))


(defun matrix-transpose (mtrx)
  "Transpose a matrix (two-dimensional array). If the input is a vector
this function creates an output matrix with size of input vector
length by 1."
  (assert (<= (array-rank mtrx) 2))
  (if (= (array-rank mtrx) 1)
      (let ((res (make-array `(,(length mtrx) 1) :element-type 'double-float)))
        (loop for r from 0 to (1- (length mtrx))
              do (setf (aref res r 0) (aref mtrx r)))
        res)
      (let ((res (make-array (reverse (array-dimensions mtrx)) :element-type 'double-float )))
        (loop for r from 0 to (1- (array-dimension mtrx 0))
              do (loop for c from 0 to (1- (array-dimension mtrx 1))
                       do (setf (aref res c r) (aref mtrx r c))))
        res)))


(defun dgemm-demo ()
  (let* ((A (make-array '(3 3) :element-type 'double-float :initial-contents '((1d0 8d0 3d0) (2d0 10d0 8d0) (9d0 -5d0 -1d0))))
	 (B (make-array '(3 3) :element-type 'double-float :initial-contents '((9d0 8d0 3d0) (3d0 11d0 2.3d0) (-8d0 6d0 1d0))))
	 (C (make-array '(3 3) :element-type 'double-float :initial-contents '((3d0 3d0 1.2d0) (8d0 4d0 8d0) (6d0 1d0 -2d0))))
	 (A (matrix-transpose A))
	 (B (matrix-transpose B))
	 (C (matrix-transpose C))
	 (alpha 3.0d0)
	 (beta -2.0d0)
	 (n 3))
    (format t "A = ~A~%B = ~A~%C = ~A~%" A B C)
    (sb-sys:with-pinned-objects (A B C)
      (dgemm "n" "n" n n n alpha (pointer  A) n (pointer  B) n beta (pointer C) n))
    (format t "call dgemm~%")
    (format t "C = ~A~%" C)
    (list A B (matrix-transpose  C))))

(defun dgemm-demo2 ()
  (let* ((A (make-array '(3 3) :element-type 'double-float :initial-contents '((1.0d0 2.0d0 9.0d0)
									       (8.0d0 10.0d0 -5.0d0)
									       (3.0d0 8.0d0 -1.0d0))))
	 (B (make-array '(3 3) :element-type 'double-float :initial-contents '((9.0d0 3.0d0 -8.0d0) (8.0d0 11.0d0 6.0d0) (3.0d0 2.3d0 1.0d0))))
	 (C (make-array '(3 3) :element-type 'double-float :initial-contents '((3.0d0 8.0d0 6.0d0) (3.0d0 4.0d0 1.0d0) (1.2d0 8.0d0 -2.0d0))))
	 (alpha 3.0d0)
	 (beta -2.0d0)
	 (n 3)
	 )
    (format t "A = ~A~%B = ~A~%C = ~A~%" A B C)
    (sb-sys:with-pinned-objects (A B C)
      (dgemm "n" "n" n n n alpha (pointer  A) n (pointer  B) n beta (pointer C) n))
    (format t "call dgemm~%")
    (format t "C = ~A~%" C)
    (list A B C)))

