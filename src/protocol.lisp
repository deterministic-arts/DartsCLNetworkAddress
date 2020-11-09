#|                                           -*- mode: lisp; coding: utf-8 -*-
  Network Address Representations
  Copyright (c) Dirk Eßer
  See file LICENCE
|#

(in-package #:darts.lib.network-address)

(defgeneric address-bytes (object)
  (:method ((object t)) (declare (ignore object)) nil)
  (:documentation "Answers a byte array (i.e., array of (UNSIGNED-BYTE 8)
    elements) that represent the address OBJECT. The array is assumed to
    be suitable for being passed down to socket-open functions, etc."))

(declaim (inline address= address< address<= address/= address<= address>= address>))

(defgeneric address-hash (object)
  (:documentation "Answers a fixnum, which represents a hash code
    for the given address OBJECT. The hashing algorithm should be
    compatible with type's notion of the equivalence as tested for
    by ADDRESS-HASH."))

(defgeneric address-equal (object1 object2)
  (:method (o1 o2) (eql o1 o2))
  (:documentation "Tests, whether the given addresses are equal.
    A valid implementation of this method provides an equivalence
    relation over a certain subset of addresses, i.e., it satisfies 
    the following properties:

     - (ADDRESS-EQUAL x x) is true (reflexivity)
     - (ADDRESS-EQUAL x y) = (ADDRESS-EQUAL y x) (symmetry)
     - (AND (ADDRESS-EQUAL x y) (ADDRESS-EQUAL y z)) ==> (ADDRESS-EQUAL x z) (transitivity)
    
    The default method returns false for any input object. If you
    provide a method on this function for some new class, you should
    also provide a compatible method on ADDRESS-HASH."))

(defgeneric address-order (object1 object2)
  (:documentation "Compares two addresses and returns an integer value
    that reflects their relative order. The value returned is 

     - negative, if OBJECT1 is strictly less than OBJECT2
     - zero, if OBJECT1 and OBJECT2 are considered equal, and
     - positive, if OBJECT1 is strictly greater than OBJECT2.

    This function is optional, and needs to be implemented only for
    addresses, which can meaningfully be compared for order. That said,
    all address types defined in this library support it. When suitable
    methods are provided for this function, a type becomes automatically
    supported by functions ADDRESS=, ADDRESS<, ADDRESS<=, ADDRESS>=,
    ADDRESS>, and ADDRESS/=."))


(defun address< (object1 object2)
  (minusp (address-order object1 object2)))

(defun address= (object1 object2)
  (zerop (address-order object1 object2)))

(defun address> (object1 object2)
  (plusp (address-order object1 object2)))

(defun address/= (object1 object2)
  (not (zerop (address-order object1 object2))))

(defun address<= (object1 object2)
  (not (plusp (address-order object1 object2))))

(defun address>= (object1 object2)
  (not (minusp (address-order object1 object2))))

#+SBCL
(sb-ext:define-hash-table-test address-equal address-hash)



(defgeneric print-address (object stream &key)
  (:documentation "Print a representation of address OBJECT to STREAM
    subject to the remaining options given as keyword arguments. Returns
    the input OBJECT as only value. For every method defined on this
    function there is the strong expectation, that the rendition of
    OBJECT generated without any modifier arguments can be parsed
    back into an address instance equivalent with OBJECT."))

(defmethod print-address :around (address stream &key prefix suffix)
  (when prefix (princ prefix stream))
  (call-next-method)
  (when suffix (princ suffix stream))
  address)

(defun address-string (object &rest options)
  "Answers a string representation of the address OBJECT subject to
   the remaining options. The convention is, that when"
  (with-output-to-string (stream)
    (apply #'print-address object stream options)))
