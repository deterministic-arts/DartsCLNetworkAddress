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

(declaim (inline address/= address<= address>= address>))

(defgeneric address-bytes (object))
(defgeneric address-type (object))
(defgeneric address= (object1 object2))
(defgeneric address< (object1 object2))
(defgeneric address-hash (object))
(defgeneric print-address (object stream &key))

(defmethod address= (object1 object2)
  (eql object1 object2))

(defmethod address< (object1 object2)
  (declare (ignore object1 object2))
  nil)

(defun address/= (object1 object2)
  (not (address= object1 object2)))

(defun address> (object1 object2)
  (address< object2 object1))

(defun address>= (object1 object2)
  (not (address< object1 object2)))

(defun address<= (object1 object2)
  (not (address< object2 object1)))

(defun address-string (object &rest options)
  (with-output-to-string (stream)
    (apply #'print-address object stream options)))
