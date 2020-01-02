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

(in-package #:common-lisp-user)
(defpackage #:darts.asdf (:use #:common-lisp #:asdf))
(in-package #:darts.asdf)

(defsystem #:darts.lib.network-address
  :name "darts.lib.network-address"
  :author "Dirk Esser"
  :version "0.1"
  :maintainer "Dirk Esser"
  :licence "MIT"
  :description "Representations for network addresses and hosts"
  :long-description ""
  :depends-on (#:split-sequence)
  :components
  ((:module :src
    :components
    ((:file "package")
     (:file "protocol" :depends-on ("package"))
     (:file "errors" :depends-on ("package"))
     (:file "ipv4" :depends-on ("protocol" "errors" "package"))
     (:file "ipv6" :depends-on ("protocol" "errors" "package"))
     (:file "host-name" :depends-on ("protocol" "errors" "package"))
     (:file "misc" :depends-on ("protocol" "package" "ipv4" "ipv6" "host-name"))))))

;;; EOF
