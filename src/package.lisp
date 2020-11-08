#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Network Address Representations
  Copyright (c) 2019, 2020 Dirk Esser

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

(defpackage #:darts.lib.network-address
  (:use #:common-lisp #:split-sequence)
  (:export #:address-bytes #:address-string #:print-address #:address-order #:address-equal
           #:address-hash #:address= #:address/= #:address< #:address<= #:address>= #:address>
           #:ipv4-address #:ipv4-address-p #:parse-ipv4-address
           #:ipv6-address #:ipv6-address-p #:parse-ipv6-address
           #:address-parse-error #:address-parse-error-input #:address-parse-error-expected-type
           #:host-name-string-p #:host-name-string #:host-name #:host-name-p #:parse-host-name)
  (:documentation "A simple library to parse, format, and represent network addresses 
    in various formats and of various kinds (IPv4, IPv6, etc.)"))
