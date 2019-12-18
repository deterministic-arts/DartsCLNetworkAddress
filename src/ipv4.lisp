#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Network Address Representations
  Copyright (c) 2019 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
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
             (error "~S is not a well-formed IPv4 address string"
                    string)))
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

(defmethod address-type ((object ipv4-address))
  (declare (ignore object))
  :ipv4)

(defmethod address= ((object1 ipv4-address) (object2 ipv4-address))
  (eql (ipv4-address-value object1)
       (ipv4-address-value object2)))

(defmethod address< ((object1 ipv4-address) (object2 ipv4-address))
  (< (ipv4-address-value object1)
     (ipv4-address-value object2)))

(defmethod address-hash ((object ipv4-address))
  (sxhash (ipv4-address-value object)))

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
