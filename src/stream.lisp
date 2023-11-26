(defpackage cl-gdsfeel/stream
  (:use #:cl
	#:alexandria
	#:flexi-streams
	#:array-operations
	#:local-time
	#:cl-geometry2
	#:cl-gdsfeel/geom
	#:cl-gdsfeel/model)
  (:import-from #:clem
		#:invert-matrix)
  (:import-from #:osicat-posix
		#:stat #:stat-size)
  (:shadowing-import-from #:alexandria
			  #:flatten)
  (:shadowing-import-from #:cl-gdsfeel/model
			  #:structure)
  (:export #:<inform>
	   #:path
	   #:run
	   #:entry))

(in-package :cl-gdsfeel/stream)

(defparameter +headers-alist+
  '((_header . #(#x00 #x02))
    (_bgnlib . #(#x01 #x02))
    (_libname . #(#x02 #x06))
    (_units . #(#x03 #x05))
    (_endlib . #(#x04 #x00))
    (_bgnstr . #(#x05 #x02))
    (_strname . #(#x06 #x06))
    (_endstr . #(#x07 #x00))
    (_boundary . #(#x08 #x00))
    (_path . #(#x09 #x00))
    (_sref . #(#x0A #x00))
    (_aref . #(#x0B #x00))
    (_text . #(#x0C #x00))
    (_layer . #(#x0D #x02))
    (_datatype . #(#x0E #x02))
    (_width . #(#x0F #x03))
    (_xy . #(#x10 #x03))
    (_endel . #(#x11 #x00))
    (_sname . #(#x12 #x06))
    (_colrow . #(#x13 #x02))
    (_textnode . #(#x14 #x00))
    (_node . #(#x15 #x00))
    (_texttype . #(#x16 #x02))
    (_presentation . #(#x17 #x01))
    ;;  (_spacing . "Discontinued")
    (_string . #(#x19 #x06))
    (_strans . #(#x1A #x01))
    (_mag . #(#x1B #x05))
    (_angle . #(#x1C #x05))
    ;;  (_uinteger . "No longer used")
    ;;  (_ustring . "No longer used")
    (_reflibs . #(#x1F #x06))
    (_fonts . #(#x20 #x06))
    (_pathtype . #(#x21 #x02))
    (_generations . #(#x22 #x02))
    (_attrtable . #(#x23 #x06))
    (_strtype . #(#x25 #x02))
    (_elflags . #(#x26 #x01))
    (_elkey . #(#x27 #x03))
    (_nodetype . #(#x2A #x02))
    (_propattr . #(#x2B #x02))
    (_propvalue . #(#x2C #x06))
    (_box . #(#x2D #x00))
    (_boxtype . #(#x2E #x02))
    (_plex . #(#x2F #x03))
    (_bgnextn . #(#x30 #x03))
    (_endextn . #(#x31 #x03))))

;; DATATYPE			VALUE
;; ________			_____
;; No Data Present		0
;; Bit Array			1
;; Double Byte Signed Integer	2
;; Four Byte Signed Integer	3
;; Four Byte Real		4
;; Eight Byte Real		5
;; ASCII String			6

(defparameter +datatypes-alist+
  '((_NIL .  0)
    (_BITARRAY . 1)
    (_INT2 . 2)
    (_INT4 . 3)
    ;;  (_REAL4 . 4)                         ;; not used
    (_REAL8 . 5)
    (_ASCII . 6)))

;; (2 . "record length")
;; (4 . "bgnlib")
;; (2 . "record length")
;; (2 . "endlib") 
(define-constant +stream-format-min-size+ 10)

;;;; ----------------- Kernel  ----------------

(defun record-size (in)
  (nibbles:ub16ref/be (read-byte-vector in 2) 0))


(defun next-bytes (in)
  (let ((size (record-size in)))
    (if (<= size 0)
        (vector)
        (read-byte-vector in (- size 2)))))


(defconstant +null+ (code-char 0))


(defun read-null-terminated-ascii (in)
  (with-output-to-string (s)
    (loop for char = (code-char (read-byte in nil 0))
          until (char= char +null+) do (write-char char s))))


(defun read-byte-vector (in size)
  (let ((buff (make-array size :element-type '(unsigned-byte 8))))
    (read-sequence buff in)
    buff))      


(defun header-bytes (bytes)
  (subseq bytes 0 2))


(defun body-bytes (bytes)
  (subseq bytes 2))


(defun header-symbol (bytes)
  (first (rassoc (header-bytes bytes) +headers-alist+ :test #'equalp)))


(defun datatype-symbol (bytes)
  (first (rassoc (aref bytes 1) +datatypes-alist+)))

;; DGL format
;; 4-byte real: exponent(7 bits) fraction(24 bits)
;; SEEEEEEE MMMMMMMM MMMMMMMM MMMMMMMM
;; 8-byte real: exponent(7 bits) fraction(56 bits)
;; SEEEEEEE MMMMMMMM MMMMMMMM MMMMMMMM MMMMMMMM MMMMMMMM MMMMMMMM MMMMMMMM

;; IEEE 754 format
;; 4-byte real: exponent(8 bits) fraction(23 bits)
;; SEEEEEEE EMMMMMMM MMMMMMMM MMMMMMMM
;; 8-byte real: exponent(11 bits) fraction(52 bits)
;; SEEEEEEE EEEEMMMM MMMMMMMM MMMMMMMM MMMMMMMM MMMMMMMM MMMMMMMM MMMMMMMM
(defun decode-dgl-real8 (octet-array)
  (assert (= (length octet-array) 8))
  (let ((sign (> (logand (elt octet-array 0) #x80) 0))
        (exponent (- (logand (elt octet-array 0) #x7f) 64))
        (mantissa-int 0)
        (mantissa-float 0.0)
        (result 0.0))
    (loop for i
          from 1 below 8
          do (setq mantissa-int (ash mantissa-int 8))
             (incf mantissa-int (elt octet-array i)))
    (setq mantissa-float (/ (coerce mantissa-int 'double-float) (expt 2 56) ))
    (setq result (* mantissa-float (expt 16 (coerce exponent 'single-float))))
    (if sign
        (- result)
        result)))

(defun fix-year (v)
  (if (and (> 100 v) (<= 60 v) (>= 99))
      (+ v 1900)
      v))

(defun fix-ce-value (v user-unit)
  (let ((ratio (rationalize user-unit)))
    (+ 0.0d0 (* (truncate (/ v ratio)) ratio))))

(defun decode-local-time (octet-array)
  (assert (= (length octet-array) 6))
  (let ((year (fix-year (elt octet-array 0)))
        (rest-time (reverse (subseq octet-array 1)))) 
    (apply
     'local-time:encode-timestamp
     (concatenate 'list #(0) rest-time (vector year)))))


;;;; ----------------- CLOS wrap ----------------
(defclass <inform> ()
  ((path :type pathname :initform (path "") :initarg :path :reader path)
   (records :type list :initform nil :accessor records)
   (library :type <library> :accessor library)))


(defun active-structure (inform)
  (check-type inform <inform>)
  (vector-last (structures (library inform))))

(defun set-container-timestamp (container body-data)
  (check-type container <named-container>)
  (assert (and (typep body-data 'sequence) (= 12 (length body-data))))
  (setf (last-modified container)
        (decode-local-time (subseq body-data 0 6)))
  (setf (last-accessed container)
        (decode-local-time (subseq body-data 6 12))))

(defun symbol-to-class (symbol)
  (find-symbol (concatenate
		'string
		"<" (subseq (string symbol) 1) ">") :cl-gdsfeel/model))

(defmethod run ((inform <inform>))
  (if (null (records inform))
      (with-open-file (in (slot-value inform 'path) :element-type '(unsigned-byte 8))
        (setf (records inform) (reverse (read-loop in)))))
  (let ((structure nil)
        (element nil)
	(uunit nil))
    (dolist (record (records inform))
      (let ((rectype (header-symbol record))
            (body-data (do-body-data record)))
        (defun recout () (print (list rectype body-data)))
        (case rectype
          ((_bgnlib _bgnstr)
           (let ((container (make-instance
                             (if (eq '_bgnlib rectype)
                                 '<library>
                                 '<structure>))))
             (set-container-timestamp container body-data)
             (if (eq '_bgnlib rectype)
                 (setf (library inform) container)
                 ;; 'bgnstr
                 (progn
                   (setq structure (add-structure (library inform) container))
                   (setf (library structure) (library inform))))))
          
          (_libname
           (setf (name (library inform)) body-data))

          (_units
           (setf (user-unit (library inform)) (elt body-data 0))
           (setq uunit (elt body-data 0))
           (setf (meter-unit (library inform)) (elt body-data 1)))
          
          (_strname
           (setf (name structure) body-data))

          (_endstr
           (setq structure nil))

          ((_path _boundary _text _sref _aref)
           (setq element (make-instance (symbol-to-class rectype)))
           (setf (structure element) structure)
           (add-element structure element))

	  (_width
	   (setf (path-width element) (fix-ce-value (* uunit body-data) uunit)))
          (_pathtype
           (setf (pathtype element) body-data))
	  
          (_xy
           (setf (xy element) (map 'list
				   (lambda (x) (fix-ce-value (* uunit x) uunit))
				   body-data))
	   (when (eq (type-of element) '<aref>)
	     (let* ((mat (ref-transform element))
		    (col-point (invert-point mat (second (points element))))
		    (row-point (invert-point mat (third (points element)))))
	       (setf (x-step element) (fix-ce-value (/ (x col-point) (column-count element)) uunit))
	       (setf (y-step element) (fix-ce-value (/ (y row-point) (row-count element)) uunit)))))
	  
          (_layer
           (setf (layer element) body-data))
          
          (_datatype
           (setf (datatype element) body-data))
          
          (_sname
           (setf (refname element) body-data))

	  (_strans
	   (setf (reflected-p element) (not (zerop (logand (elt record 2) #x80))))
	   (setf (abs-angle-p element) (not (zerop (logand (elt record 3) #x01))))
	   (setf (abs-mag-p element) (not (zerop (logand (elt record 3) #x02))))
	   (unless (zerop body-data)
	     (recout)
	     (print record)
	     (format t "~19,,' ,4:B" body-data)))

          (_mag
	   (setf (mag element) body-data)
	   (unless (zerop body-data)
	     (recout)))
	  
          (_angle
	   (setf (angle element) body-data)
	   (unless (zerop body-data)
	     (recout)))

	  (_colrow
	   (setf (column-count element) (first body-data))
	   (setf (row-count element) (second body-data)))
	  
          ;; ((_strans elflags strclass presentation)
          ;;  (recout)
          ;;  (print record)
          ;;  (format t "~%")
          ;;  (format t "~19,,' ,4:B" body-data))
          
          (_string
           (setf (contents element) body-data))
	  
          (_endel
           (setq element nil)
	   )
	  

          ))))
  inform)

;;; use alexandria's
;; (defun random-elt (sequence)
;;   (elt sequence (random (length sequence))))

(defun random-element (inform)
  (random-elt (children (random-elt (children (library inform))))))

;; (defun entry ()
;;   (setq *inform* (make-instance '<inform> :path *sample-sf*))
;;   (run *inform*))


;;;; ----------------- debugging IN  ----------------

(defun one-element-as-atomic (sequence)
  "'(0 1 2) => '(0 1 2)
   '(0)     => 0"
  (if (= (length sequence) 1)
      (elt sequence 0)
      sequence))

(defgeneric body-data (type body-bytes)
  (:documentation "generic body data"))


(defmethod body-data ((type symbol) body-bytes))


(defmethod body-data ((type (eql '_ASCII)) body-bytes)
  (with-input-from-sequence (in body-bytes)
    (read-null-terminated-ascii in)))


(defmethod body-data ((type (eql '_INT2)) body-bytes)
  (let ((result nil)
        (arr (aops:reshape body-bytes '(t 2))))
    (dotimes (i (aops:nrow arr))
      (let ((row (aops:sub arr i)))
        (push (nibbles:sb16ref/be row 0) result)))
    (one-element-as-atomic (reverse result))))


(defmethod body-data ((type (eql '_INT4)) body-bytes)
  (let ((result nil)
        (arr (aops:reshape body-bytes '(t 4))))
    (dotimes (i (aops:nrow arr))
      (let ((row (aops:sub arr i)))
        (push (nibbles:sb32ref/be row 0) result)))
    (one-element-as-atomic (reverse result))))


(defmethod body-data ((type (eql '_REAL8)) body-bytes)
  (let ((result nil)
        (arr (aops:reshape body-bytes '(t 8))))
    (dotimes (i (aops:nrow arr))
      (let ((row (aops:sub arr i)))
        (push (decode-dgl-real8 row ) result)))
    (one-element-as-atomic (reverse result))))


(defmethod body-data ((type (eql '_BITARRAY)) body-bytes)
  (nibbles:ub16ref/be body-bytes 0))


(defun debug-record (bytes)
  (print bytes)
  (print (header-symbol bytes))
  (print (datatype-symbol bytes))
  (format t "~%"))


(defgeneric handle-record (header bytes)
  (:documentation "generic handle records"))


(defmethod handle-record ((header symbol) bytes)
  (debug-record bytes)
  (print (do-body-data bytes)))


(defmethod handle-record ((header (eql '_BGNLIB)) bytes)
  (call-next-method)
  (print "BGNLIB found"))


(defun do-body-data (bytes)
  (body-data (datatype-symbol bytes) (body-bytes bytes)))


(defun do-handle-record (bytes)
  (handle-record (header-symbol bytes) bytes))


;;;; ----------------- debugging OUT ----------------

;;; (dolist (each *all-records*) (handle-record each))

;; (defparameter *sample-sf* (truename "~/Nextcloud/gds/GDSreader.0.3.2/test.gds"))

;; (defparameter *inform* nil)

;; (defparameter *fd* nil)

;; (defparameter *all-records* nil)

;; (defparameter *step-index* -1)

;; (defun open-gds ()
;;   (setq *fd* (open *sample-sf* :element-type '(unsigned-byte 8))))


;; (defun close-gds ()
;;   (close *fd*))


;; (defun reset-gds ()
;;   (file-position *fd* 0))


(defun read-loop (fd)
  (do ((eof 0) (bytes) (records))
      ((equalp eof 1) records)
    (setq bytes (next-bytes fd))
    (if (<= 2 (length bytes))
        (push bytes records)
        (setq eof 1))))


;; (defun ensure-all-records ()
;;   (setq *all-records* nil)
;;   (with-open-file (f *sample-sf* :element-type '(unsigned-byte 8))
;;     (setq *all-records* (reverse (read-loop f)))
;;     nil))


;; (defun next-record ()
;;   (if (null *all-records*)
;;       (ensure-all-records))
;;   (if (>= (length *all-records*) (- *step-index* 1))
;;       (nth (incf *step-index*) *all-records*)
;;       'nil))

;; ---- Utilities -------

(defun file-size (path)
  (let ((stat (osicat-posix:stat (pathname  path))))
    (osicat-posix:stat-size stat)))

(defun stream-format-p (path)
  (and (uiop:file-exists-p path)
       (<= +stream-format-min-size+ (file-size path))
       (with-open-file (s path :element-type '(unsigned-byte 8))
	 (let ((len (nibbles:read-ub16/be s)))
	   (file-position s 0)
	   (and (= 6 len)    ;; 2 + size of header
		(eq 'header (header-symbol (next-bytes s))))))))
