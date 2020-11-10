#|                                           -*- mode: lisp; coding: utf-8 -*-
  Network Address Representations
  Copyright (c) Dirk Eßer
  See file LICENCE
|#

(in-package #:darts.lib.network-address)

(defgeneric address-parse-error-input (object))
(defgeneric address-parse-error-expected-type (object))

(define-condition address-parse-error (parse-error)
  ((input :initarg :input :reader address-parse-error-input)
   (expected-type :initarg :expected-type :reader address-parse-error-expected-type))
  (:report (lambda (object stream)
             (format stream "could not parse ~S as ~S"
                     (address-parse-error-input object)
                     (address-parse-error-expected-type object)))))

(define-condition uri-parse-error (error)
  ((input :initform nil :initarg :input :reader uri-parse-error-input)))

(define-condition simple-uri-parse-error (simple-condition uri-parse-error)
  ())
