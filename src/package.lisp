
(defpackage #:darts.lib.network-address
  (:use #:common-lisp #:split-sequence)
  (:export #:address-bytes #:address-string #:print-address #:address-order #:address-equal
           #:address-hash #:address= #:address/= #:address< #:address<= #:address>= #:address>
           #:ipv4-address #:ipv4-address-p #:parse-ipv4-address
           #:ipv6-address #:ipv6-address-p #:parse-ipv6-address
           #:address-parse-error #:address-parse-error-input #:address-parse-error-expected-type
           #:host-name-string-p #:host-name-string #:host-name #:host-name-p)
  (:documentation "A simple library to parse, format, and represent network addresses 
    in various formats and of various kinds (IPv4, IPv6, etc.)"))
