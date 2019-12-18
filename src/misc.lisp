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

(defun ipv6-address (value)
  (labels
      ((bad-value ()
         (error 'simple-type-error
                :datum value :expected-type 'ipv6-address
                :format-control "~S is not a supported IPv6 address designator"
                :format-arguments (list value))))
    (typecase value
      (ipv6-address value)
      (ipv4-address (make-ipv6-address-1 0 (ipv4-address-value value)))
      ((unsigned-byte 128) (make-ipv6-address-1 (ldb (byte 64 64) value) (ldb (byte 64 0) value)))
      ((array (unsigned-byte 8) (16))
       (let ((w1 0) (w2 0))
         (loop
            for p upfrom 0 below 8
            for b downfrom 56 by 8
            do (setf (ldb (byte 8 b) w1) (aref value p)))
         (loop
            for p upfrom 8 below 16
            for b downfrom 56 by 8
            do (setf (ldb (byte 8 b) w2) (aref value p)))
         (make-ipv6-address-1 w1 w2)))
      (string (or (parse-ipv6-address value :junk-allowed t) (bad-value)))
      (t (bad-value)))))

(defun ipv4-address (value)
  (labels
      ((bad-value ()
         (error 'simple-type-error
                :datum value :expected-type 'ipv4-address
                :format-control "~S is not a supported designator for an IPv4 address"
                :format-arguments (list value))))
    (typecase value
      (ipv4-address value)
      (ipv4-embedded-in-ipv6 (make-ipv4-address-1 (ipv6-address-word2 value)))
      ((unsigned-byte 32) (make-ipv4-address-1 value))
      ((array (unsigned-byte 8) (4)) (make-ipv4-address-4 (aref value 0) (aref value 1) (aref value 2) (aref value 3)))
      (string (or (parse-ipv4-address value :junk-allowed t) (bad-value)))
      (t (bad-value)))))

(defmethod address< ((object1 ipv4-address) (object2 ipv6-address))
  (or (not (zerop (ipv6-address-word1 object2)))
      (< (ipv4-address-value object1)
         (ipv6-address-word2 object2))))

(defmethod address< ((object1 ipv6-address) (object2 ipv4-address))
  (and (zerop (ipv6-address-word1 object1))
       (< (ipv6-address-word2 object1)
          (ipv4-address-value object2))))

(defmethod print-address :around (address stream &key prefix suffix)
  (when prefix (princ prefix stream))
  (multiple-value-prog1 (call-next-method)
    (when suffix (princ suffix stream))))
