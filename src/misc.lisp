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

(defmethod ipv6-address ((value ipv4-address))
  (make-ipv6-address-1 0 (ipv4-address-value value)))

(defmethod ipv4-address ((object ipv6-address))
  (typecase object
    (ipv4-embedded-in-ipv6 (make-ipv4-address-1 (ipv6-address-word2 object)))
    (t (call-next-method))))

(defmethod address< ((object1 ipv4-address) (object2 ipv6-address))
  (or (not (zerop (ipv6-address-word1 object2)))
      ;; This is subtle... We want addresses to have a total order; in
      ;; particular, we want the "usual" property, that for any two addresses
      ;; A and B, exactly one of
      ;;
      ;;   A < B, A = B, A > B
      ;;
      ;; since only then is it meaningful to derive <=, >=, etc. from <.
      ;; But we also want ipv4 != ipv6, since even though numerically equal, those
      ;; addresses are of different type, and we do not want them to compare
      ;; as equal.
      ;;
      ;; So here, the definition of < must follow the lead. We define (arbitrarily)
      ;; that IPv4 < IPv6 (if their numeric values are equal.) So, what we
      ;; actually test is:
      ;;
      ;; A < B    <==>   num(A) < num(B) or num(A) = num(B) and prio(A) < prio(B)
      ;;
      ;; with prio(X) = 0 if X is an IPv4, 1 if X is an IPv6
      ;;
      ;; Taking into account, that we know already that A is an IPv4 and
      ;; B is an IPv6, we can shorten that into a single test (i.e., using
      ;; <= here instead of <)
      (<= (ipv4-address-value object1) (ipv6-address-word2 object2))))

(defmethod address< ((object1 ipv6-address) (object2 ipv4-address))
  (and (zerop (ipv6-address-word1 object1))
       (< (ipv6-address-word2 object1)
          (ipv4-address-value object2))))

(defmethod print-address :around (address stream &key prefix suffix)
  (when prefix (princ prefix stream))
  (multiple-value-prog1 (call-next-method)
    (when suffix (princ suffix stream))))
