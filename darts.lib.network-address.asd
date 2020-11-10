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
     (:file "misc" :depends-on ("protocol" "package" "ipv4" "ipv6" "host-name"))
     (:file "uris" :depends-on ("package"))))))

;;; EOF
