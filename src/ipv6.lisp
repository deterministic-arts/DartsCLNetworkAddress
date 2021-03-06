#|                                           -*- mode: lisp; coding: utf-8 -*-
  Network Address Representations
  Copyright (c) Dirk Eßer
  See file LICENCE
|#

(in-package #:darts.lib.network-address)

(defstruct (ipv6-address (:copier nil) (:predicate ipv6-address-p)
                         (:constructor make-ipv6-address-1 (word1 word2)))
  (word1 0 :type (unsigned-byte 64) :read-only t)
  (word2 0 :type (unsigned-byte 64) :read-only t))

(defmethod make-load-form ((object ipv6-address) &optional environment)
  (declare (ignore environment))
  `(make-ipv6-address-1 ,(ipv6-address-word1 object)
                        ,(ipv6-address-word2 object)))

(defmethod address-equal ((object1 ipv6-address) (object2 ipv6-address))
  (and (eql (ipv6-address-word1 object1) (ipv6-address-word1 object2))
       (eql (ipv6-address-word2 object1) (ipv6-address-word2 object2))))

(defmethod address-hash ((object ipv6-address))
  (sxhash (logxor (ipv6-address-word1 object) (ipv6-address-word2 object))))

(defmethod address-order ((object1 ipv6-address) (object2 ipv6-address))
  (let ((h1 (ipv6-address-word1 object1))
        (h2 (ipv6-address-word1 object2)))
    (cond
      ((< h1 h2) -1)
      ((> h1 h2) 1)
      (t (let ((l1 (ipv6-address-word2 object1))
               (l2 (ipv6-address-word2 object2)))
           (cond
             ((< l1 l2) -1)
             ((> l1 l2) 1)
             (t 0)))))))

(defmethod address-bytes ((object ipv6-address))
  (let ((w1 (ipv6-address-word1 object))
        (w2 (ipv6-address-word2 object))
        (array (make-array 16 :element-type '(unsigned-byte 8))))
    (loop
       for p upfrom 0 below 8
       for b downfrom 56 by 8
       do (setf (aref array p) (ldb (byte 8 b) w1)))
    (loop
       for p upfrom 8 below 16
       for b downfrom 56 by 8
       do (setf (aref array p) (ldb (byte 8 b) w2)))
    array))

(defun ipv4-embedded-in-ipv6-p (value)
  (and (zerop (ipv6-address-word1 value))
       (<= 0 (ipv6-address-word2 value) #xFFFFFFFF)))

(deftype ipv4-embedded-in-ipv6 ()
  '(and ipv6-address (satisfies ipv4-embedded-in-ipv6-p)))

(defmethod print-address ((object ipv6-address) stream
                          &key all-limbs leading-zeroes)
  (let* ((w1 (ipv6-address-word1 object))
         (w2 (ipv6-address-word2 object))
         (qs (list (ldb (byte 16 48) w1) (ldb (byte 16 32) w1) (ldb (byte 16 16) w1)
                   (ldb (byte 16 0) w1) (ldb (byte 16 48) w2) (ldb (byte 16 32) w2)
                   (ldb (byte 16 16) w2) (ldb (byte 16 0) w2))))
    (labels
        ((find-longest-zero-range (list position start length best-start best-length)
           (cond
             ((null list) (if (> length best-length)
                              (values start length)
                              (values best-start best-length)))
             ((zerop (car list))
              (find-longest-zero-range (cdr list) (1+ position) (if (plusp length) start position)
                                       (1+ length) best-start best-length))
             (t (if (> length best-length)
                    (find-longest-zero-range (cdr list) (1+ position) (1+ position) 0
                                             start length)
                    (find-longest-zero-range (cdr list) (1+ position) (1+ position) 0
                                             best-start best-length))))))
      (multiple-value-bind (skip-start skip-length)
          (if all-limbs (values 0 0) (find-longest-zero-range qs 0 0 0 nil 0))
        (if (< skip-length 2)
            (if leading-zeroes
                (format stream "~(~{~4,'0x~^:~}~)" qs)
                (format stream "~(~{~x~^:~}~)" qs))
            (let ((write-limb (if leading-zeroes
                                  (lambda (limb) (format stream "~(~4,'0X~)" limb))
                                  (lambda (limb) (format stream "~(~X~)" limb)))))
              (loop
                 for p upfrom 0 while (< p skip-start)
                 do (funcall write-limb (pop qs))
                    (write-char #\: stream))
              (when (zerop skip-start) (princ #\: stream))
              (loop while (and qs (zerop (car qs))) do (pop qs))
              (if (null qs)
                  (princ #\: stream)
                  (loop
                     for s in qs
                     do (write-char #\: stream)
                        (funcall write-limb s))))))
      object)))

(defmethod print-object ((object ipv6-address) stream)
  (if (not *print-escape*)
      (print-address object stream)
      (print-unreadable-object (object stream :type t :identity nil)
        (print-address object stream)))
  object)

(defmacro dpbs (new-byte position value &rest more)
  (if (null more)
      `(dpb ,new-byte ,position ,value)
      `(dpb ,new-byte ,position (dpbs ,value ,@more))))

(defun tokenize-ipv6-address (string &optional (start 0) (end (length string)))
  (labels
      ((possible-ipv4-p (string &key (start 0) end)
         (let* ((string (string string))
                (end (or end (length string))))
           (labels
               ((dotp (index)
                  (and (< index end) (eql (char string index) #\.)))
                (parse-octet (index value count)
                  (let* ((char (and (< index end) (char string index)))
                         (digit (and char (digit-char-p char 10))))
                    (if (not digit)
                        (if (and (plusp count) (<= value 255))
                            (values value index)
                            (values nil nil))
                        (parse-octet (1+ index) (+ (* value 10) digit) (1+ count))))))
             (multiple-value-bind (d0 index) (parse-octet start 0 0)
               (and d0
                    (dotp index)
                    (multiple-value-bind (d1 index) (parse-octet (1+ index) 0 0)
                      (and d1
                           (dotp index)
                           (multiple-value-bind (d2 index) (parse-octet (1+ index) 0 0)
                             (and d2
                                  (dotp index)
                                  (multiple-value-bind (d3 index) (parse-octet (1+ index) 0 0)
                                    (and d3 (eql index end)
                                         (dpbs d0 (byte 8 24)
                                               d1 (byte 8 16)
                                               d2 (byte 8 8)
                                               d3))))))))))))
       (read-ub32 (index)
         (let ((ip4 (possible-ipv4-p string :start index :end end)))
           (if ip4
               (values :ub32 ip4 end)
               (read-ub16 index 0))))
       (read-ub16 (index value)
         (if (>= index end)
             (if (<= 0 value #xFFFF)
                 (values :ub16 value index)
                 (return-from tokenize-ipv6-address nil))
             (let* ((char (char string index))
                    (digit (digit-char-p char 16)))
               (if digit
                   (read-ub16 (1+ index) (+ (* value 16) digit))
                   (if (<= 0 value #xFFFF)
                       (values :ub16 value index)
                       (return-from tokenize-ipv6-address nil))))))
       (next-token (index)
         (if (>= index end)
             (values nil nil nil)
             (let ((char (char string index)))
               (cond
                 ((eql char #\:)
                  (if (and (< (1+ index) end) (eql (char string (1+ index)) #\:))
                      (values :double-colon nil (+ index 2))
                      (values :colon nil (1+ index))))
                 ((digit-char-p char 10) (read-ub32 index))
                 ((digit-char-p char 16) (read-ub16 index 0))
                 (t (return-from tokenize-ipv6-address nil)))))))
    (let ((index start) token value result)
      (loop
         (multiple-value-setq (token value index) (next-token index)) 
         (if (not token)
             (return-from tokenize-ipv6-address (nreverse result))
             (push (cons token value) result))))))

(defun parse-ipv6-address (string &key (start 0) end junk-allowed)
  (labels
      ((bad-value ()
         (if junk-allowed
             (return-from parse-ipv6-address nil)
             (error 'address-parse-error
                    :input `(,string :start ,start ,@(when end `(:end ,end)))
                    :expected-type 'ipv6-address))))
    (let* ((string (string string))
           (end (or end (length string)))
           (word1 0) (word2 0)
           (tokens (tokenize-ipv6-address string start end)))
      (labels
          (((setf quibble) (value n)
             (if (< n 4)
                 (setf (ldb (byte 16 (- 48 (* n 16))) word1) value)
                 (setf (ldb (byte 16 (- 48 (* (- n 4) 16))) word2) value))))
        (loop
           with state = :start and position = 0 and lower-bound = -1
           while tokens
           do (let* ((head (pop tokens))
                     (token (car head))
                     (value (cdr head)))
              (case token
                ((:ub16)
                 (case state
                   ((:left-to-right :start)
                    (if (= position 8) (bad-value)
                        (progn
                          (setf (quibble position) value)
                          (incf position)
                          (setf state :left-to-right-1))))
                   ((:right-to-left :after-turn)
                    (if (= position lower-bound) (bad-value)
                        (progn
                          (setf (quibble position) value)
                          (decf position)
                          (setf state :right-to-left-1))))
                   (otherwise (bad-value))))
                ((:ub32)
                 (case state
                   ((:left-to-right :start)
                    (if (/= position 6) (bad-value)
                        (progn
                          (setf (quibble 6) (ldb (byte 16 16) value))
                          (setf (quibble 7) (ldb (byte 16 0) value))
                          (incf position 2)
                          (setf state :end))))
                   ((:after-turn)
                    (if (>= lower-bound 6) (bad-value)
                        (progn
                          (setf (quibble 6) (ldb (byte 16 16) value))
                          (setf (quibble 7) (ldb (byte 16 0) value))
                          (decf position 2)
                          (setf state :right-to-left-1))))
                   (t (bad-value))))
                ((:colon)
                 (case state
                   ((:left-to-right-1) (setf state :left-to-right))
                   ((:right-to-left-1) (setf state :right-to-left))
                   (otherwise (bad-value))))
                ((:double-colon)
                 (case state
                   ((:start)
                    (setf tokens (reverse tokens))
                    (setf lower-bound -1)
                    (setf position 7)
                    (setf state :after-turn))
                   ((:left-to-right-1)
                    (setf tokens (reverse tokens))
                    (setf lower-bound (1- position))
                    (setf position 7)
                    (setf state :after-turn))
                   (otherwise (bad-value))))
                (otherwise (bad-value))))
           finally (case state
                     ((:left-to-right-1) (unless (< position 8) (bad-value)))
                     ((:right-to-left-1) (unless (>= position lower-bound) (bad-value)))
                     ((:after-turn :end))
                     (otherwise (bad-value)))))
        (make-ipv6-address-1 word1 word2))))

(defgeneric ipv6-address (object)
  (:method ((object ipv6-address)) object)
  (:method ((object string)) (or (parse-ipv6-address object :junk-allowed t) (call-next-method)))
  (:method ((object t))
    (error 'simple-type-error
                :datum object :expected-type 'ipv6-address
                :format-control "~S is not a supported IPv6 address designator"
                :format-arguments (list object))))

(defmethod ipv6-address ((object array))
  (typecase object
    ((array (unsigned-byte 8) (16))
     (let ((w1 0) (w2 0))
         (loop
            for p upfrom 0 below 8
            for b downfrom 56 by 8
            do (setf (ldb (byte 8 b) w1) (aref object p)))
         (loop
            for p upfrom 8 below 16
            for b downfrom 56 by 8
            do (setf (ldb (byte 8 b) w2) (aref object p)))
         (make-ipv6-address-1 w1 w2)))
    (t (call-next-method))))

(defmethod ipv6-address ((object integer))
  (typecase object
    ((unsigned-byte 128) (make-ipv6-address-1 (ldb (byte 64 64) value) (ldb (byte 64 0) value)))
    (t (call-next-method))))
