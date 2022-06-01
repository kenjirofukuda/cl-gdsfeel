(defpackage cl-gdsfeel/deprecated
  (:use :cl))

(in-package :cl-gdsfeel/deprecated)


(defun add-unsigned-byte (a b)
  (declare (type (unsigned-byte 8) a b))
  (+ a b))

(defun unsigned-seq-only (v)
  (declare (type (array (unsigned-byte 8) *) v))
  v)

(defun int16-only (v)
  (declare (type (signed-byte 16) v))
  v)


(defun read-big-u16 (stream)
  (let ((hi 0)
        (lo 0))
    (setq hi (read-byte stream))
    (setq lo (read-byte stream))
    (if (eq (+ hi lo) 0)
        '0
        (+ (* hi 256) lo))))

(defun read-big-u32 (stream)
  (let ((b0 0)
        (b1 0)
        (b2 0)
        (b3 0))
    (setq b0 (read-byte stream))
    (setq b1 (read-byte stream))
    (setq b2 (read-byte stream))
    (setq b3 (read-byte stream))
    (if (eq (+ b0 b1 b2 b3) 0)
        '0
        (+ (* b0 16777216) (* b1 65536) (* b2 256) b3))))

(defun u-to-s8 (v)
  (check-type v (unsigned-byte 8))
  (let ((result (logand v #x7F)))    
    (if (zerop (logand v #x80))
        result
        (- v 256))))

(defun u-to-s16 (v)
  (check-type v (unsigned-byte 16))
  (let ((result (logand v #x7FFFF)))    
    (if (zerop (logand v #x8000))
        result
        (- v 65536))))

(defun u-to-s32 (v)
  (check-type v (unsigned-byte 32))
  (let ((result (logand v #x7FFFFFFFF)))    
    (if (zerop (logand v #x80000000))
        result
        (- v #x100000000))))

(defun read-big-s16 (stream)
  (u-to-s16 (read-big-u16 stream)))

(defun read-big-s32 (stream)
  (u-to-s32 (read-big-u32 stream)))

(defun vector-limited (octet-array)
  (check-type octet-array vector)
  ;; (assert (eq (element-type octet-array) '(unsigned-byte 8)))
  (print octet-array))

;; check for atomic ant 0 .. 255 8bit unsigned byte
(defun octet-p (v)
  (typep v '(unsigned-byte 8)))

;; check for byte array sequence
(defun octet-seq-p (p)
  (and (subtypep (type-of p) 'sequence)
       (every 'octet-p p)))

(defun as-byte-array (v)
  (assert (octet-seq-p v))
  (coerce v 'simple-vector))

