
(defpackage #:darts.lib.network-address
  (:use #:common-lisp #:split-sequence)
  (:export #:address-bytes #:address-string #:address-type #:address= #:address/=
           #:address< #:address<= #:address>= #:address> #:address-hash #:ipv4-address
           #:ipv4-address-p #:parse-ipv4-address #:ipv6-address #:ipv6-address-p
           #:parse-ipv6-address #:print-address #:parse-ipv6-address #:address-parse-error
           #:address-parse-error-input #:address-parse-error-expected-type
           #:host-name-string-p #:host-name-string)
  (:documentation "A simple library to parse, format, and represent network addresses 
    in various formats and of various kinds (IPv4, IPv6, etc.)"))
