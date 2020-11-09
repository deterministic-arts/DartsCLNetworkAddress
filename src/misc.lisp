#|                                           -*- mode: lisp; coding: utf-8 -*-
  Network Address Representations
  Copyright (c) Dirk Eßer
  See file LICENCE
|#

(in-package #:darts.lib.network-address)

(defmethod ipv6-address ((value ipv4-address))
  (make-ipv6-address-1 0 (ipv4-address-value value)))

(defmethod ipv4-address ((object ipv6-address))
  (typecase object
    (ipv4-embedded-in-ipv6 (make-ipv4-address-1 (ipv6-address-word2 object)))
    (t (call-next-method))))

