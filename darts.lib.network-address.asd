#|                                           -*- mode: lisp; coding: utf-8 -*-
  Network Address Representations
  Copyright (c) Dirk Eßer
  See file LICENCE
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
  :depends-on (#:split-sequence #:babel)
  :serial t
  :components
  ((:module :src
    :components
    ((:file "package")
     (:file "protocol")
     (:file "errors")
     (:file "ipv4")
     (:file "ipv6")
     (:file "host-name")
     (:file "misc")
     (:file "uris")))))

;;; EOF
