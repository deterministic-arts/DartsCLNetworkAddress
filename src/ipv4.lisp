#|                                           -*- mode: lisp; coding: utf-8 -*-
  Network Address Representations
  Copyright (c) Dirk Eßer
  See file LICENCE
|#

(in-package #:darts.lib.network-address)

(defstruct (ipv4-address (:copier nil) (:predicate ipv4-address-p)
                         (:constructor make-ipv4-address-1 (value)))
  (value 0 :type (unsigned-byte 32) :read-only t))

(defun make-ipv4-address-4 (n0 n1 n2 n3)
  (declare (type (unsigned-byte 8) n0 n1 n2 n3))
  (make-ipv4-address-1 (dpb n0 (byte 8 24)
                            (dpb n1 (byte 8 16)
                                 (dpb n2 (byte 8 8)
                                      n3)))))

(defmethod make-load-form ((object ipv4-address) &optional environment)
  (declare (ignore environment))
  `(make-ipv4-address-1 ,(ipv4-address-value object)))

(defun parse-ipv4-address (string &key (start 0) end junk-allowed)
  (labels
      ((bad-value ()
         (if junk-allowed
             (return-from parse-ipv4-address nil)
             (error 'address-parse-error
                    :input `(,string :start ,start ,@(when end `(:end ,end)))
                    :expected-type 'ipv4-address)))
       (parse-int (str)
         (let ((num (parse-integer str :radix 10 :junk-allowed t)))
           (if (and num (<= 0 num 255)) num (bad-value)))))
    (let* ((string (string string))
           (end (or end (length string)))
           (parts (split-sequence #\. string :start start :end end)))
      (if (not (eql 4 (length parts)))
          (bad-value)
          (let ((n0 (parse-int (pop parts)))
                (n1 (parse-int (pop parts)))
                (n2 (parse-int (pop parts)))
                (n3 (parse-int (pop parts))))
            (make-ipv4-address-4 n0 n1 n2 n3))))))

(defmethod address-bytes ((object ipv4-address))
  (let ((array (make-array 4 :element-type '(unsigned-byte 8)))
        (value (ipv4-address-value object)))
    (setf (aref array 0) (ldb (byte 8 24) value))
    (setf (aref array 1) (ldb (byte 8 16) value))
    (setf (aref array 2) (ldb (byte 8 8) value))
    (setf (aref array 3) (ldb (byte 8 0) value))
    array))

(defmethod address-equal ((object1 ipv4-address) (object2 ipv4-address))
  (eql (ipv4-address-value object1) (ipv4-address-value object2)))

(defmethod address-hash ((object ipv4-address))
  (sxhash (ipv4-address-value object)))

(defmethod address-order ((object1 ipv4-address) (object2 ipv4-address))
  (- (ipv4-address-value object1) (ipv4-address-value object2)))

(defmethod print-address ((object ipv4-address) stream &key)
  (let ((value (ipv4-address-value object)))
    (format stream "~D.~D.~D.~D"
            (ldb (byte 8 24) value)
            (ldb (byte 8 16) value)
            (ldb (byte 8 8) value)
            (ldb (byte 8 0) value))))

(defmethod print-object ((object ipv4-address) stream)
  (if (not *print-escape*)
      (print-address object stream)
      (print-unreadable-object (object stream :type t :identity nil)
        (print-address object stream)))
  object)

(defgeneric ipv4-address (object)
  (:method ((object ipv4-address)) object)
  (:method ((object string)) (or (parse-ipv4-address object :junk-allowed t) (call-next-method)))
  (:method ((object t))
    (error 'simple-type-error
           :datum object :expected-type 'ipv4-address
           :format-control "~S is not a supported designator for an IPv4 address"
           :format-arguments (list object))))

(defmethod ipv4-address ((object integer))
  (typecase object
    ((unsigned-byte 32) (make-ipv4-address-1 object))
    (t (call-next-method))))

(defmethod ipv4-address ((object array))
  (typecase object
    ((array (unsigned-byte 8) (4)) (make-ipv4-address-4 (aref object 0) (aref object 1) (aref object 2) (aref object 3)))
    (t (call-next-method))))
