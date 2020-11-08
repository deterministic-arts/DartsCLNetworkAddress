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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun host-name-string-p (value)
    (declare (optimize speed))
    (and (stringp value)
         (<= 1 (length value) 253)
         (flet ((classify (char)
                  (cond
                    ((char<= #\a char #\z) :alnum)
                    ((char<= #\A char #\Z) :alnum)
                    ((char<= #\0 char #\9) :alnum)
                    ((eql char #\-) :hyphen)
                    ((eql char #\.) :period)
                    (t :other))))
           (declare (inline classify))
           (loop
              with len of-type fixnum = 0
              for previous-class = :initial then class
              for char across value
              for class = (classify char)
              do (ecase class
                   ((:alnum) (incf len))
                   ((:other) (return nil))
                   ((:hyphen)
                    (case previous-class
                      ((:alnum :hyphen) (incf len))
                      (otherwise (return nil))))
                   ((:period)
                    (cond
                      ((zerop len) (return nil))
                      ((> len 63) (return nil))
                      ((not (eq previous-class :alnum)) (return nil))
                      (t (setf len 0)))))
              finally (return (and (< len 64) (member class '(:alnum :period)) t)))))))

(deftype host-name-string ()
  '(and string (satisfies host-name-string-p)))

(defgeneric host-name-string (object)
  (:method ((object t))
    (error 'simple-type-error
           :datum object :expected-type 'host-name-string
           :format-control "~S is not a well-formed host name string"
           :format-arguments (list object))))

(defmethod host-name-string ((object symbol))
  (host-name-string (symbol-name object)))

(defmethod host-name-string ((object character))
  (host-name-string (string object)))

(defmethod host-name-string ((object string))
  (if (host-name-string-p object) (string-downcase object) (call-next-method)))



(defstruct (host-name (:copier nil) (:predicate host-name-p)
                      (:constructor make-host-name-1 (value)))
  (value (error "missing host name")
         :type (and simple-base-string host-name-string)
         :read-only t))

(defmethod host-name-string ((object host-name))
  (host-name-value object))

(defmethod address-equal ((o1 host-name) (o2 host-name))
  (string= (host-name-value o1) (host-name-value o2)))

(defmethod address-hash ((ob host-name))
  (sxhash ob))

(defmethod address-order ((o1 host-name) (o2 host-name))
  (let ((v1 (host-name-value o1))
        (v2 (host-name-value o2)))
    (cond
      ((string< v1 v2) -1)
      ((string< v2 v1) 1)
      (t 0))))

(defmethod print-address ((object host-name) stream &key)
  (write-string (host-name-value object) stream))

(defmethod print-object ((object host-name) stream)
  (if (not *print-escape*)
      (write-string (host-name-value object) stream)
      (print-unreadable-object (object stream :type t :identity nil)
        (write-string (host-name-value object) stream)))
  object)

(defgeneric host-name (object)
  (:method ((object host-name)) object)
  (:method ((object t))
    (error 'simple-type-error
           :datum object :expected-type 'host-name
           :format-control "~S is not a ~S"
           :format-arguments (list object 'host-name))))

(defun parse-host-name (string &key (start 0) end junk-allowed)
  (let ((substr (subseq (string string) start end)))
    (if (not (host-name-string-p substr))
        (if junk-allowed
            nil
            (error 'address-parse-error :input `(string :start ,start ,@(when end `(:end ,end)))
                                        :expected-type 'host-name))
        (let ((lower (string-downcase substr)))
          (if (typep lower 'simple-base-string)
              (make-host-name-1 lower)
              (let ((copy (make-string (length lower) :element-type 'base-char)))
                (replace copy lower)
                (make-host-name-1 copy)))))))

(defmethod host-name ((object string))
  (or (parse-host-name object) (call-next-method)))

(defmethod host-name ((object symbol))
  (host-name (symbol-name object)))

(defmethod host-name ((object character))
  (host-name (string object)))
