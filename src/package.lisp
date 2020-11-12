#|                                           -*- mode: lisp; coding: utf-8 -*-
  Network Address Representations
  Copyright (c) Dirk Eßer
  See file LICENCE
|#

(defpackage #:darts.lib.uri
  (:use)
  (:export #:uri-parse-error #:uri-parse-error-input #:split-uri #:parse-uri #:resolve-uri
           #:uri-string #:uri-scheme #:uri-user #:uri-host #:uri-port #:uri-path #:uri-query
           #:uri-fragment #:uri-equal #:uri-hash #:uri #:urip #:make-uri)
  (:documentation ""))

(defpackage #:darts.lib.network-address
  (:use #:common-lisp #:split-sequence #:darts.lib.uri)
  (:export #:address-bytes #:address-string #:print-address #:address-order #:address-equal
           #:address-hash #:address= #:address/= #:address< #:address<= #:address>= #:address>
           #:ipv4-address #:ipv4-address-p #:parse-ipv4-address
           #:ipv6-address #:ipv6-address-p #:parse-ipv6-address
           #:address-parse-error #:address-parse-error-input #:address-parse-error-expected-type
           #:host-name-string-p #:host-name-string #:host-name #:host-name-p #:parse-host-name)
  (:documentation "A simple library to parse, format, and represent network addresses 
    in various formats and of various kinds (IPv4, IPv6, etc.)"))
