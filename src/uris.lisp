
(in-package #:darts.lib.network-address)

(deftype ascii-charset ()
  '(simple-bit-vector 128)) 

(declaim (ftype (function (t) ascii-charset) make-ascii-charset))

(defun make-ascii-charset (description)
  (labels
      ((make-bitmap (&optional (fill 0))
         (make-array 128 :element-type 'bit
                         :initial-element fill))
       (index (object)
         (cond
           ((and (integerp object) (<= 0 object 127)) object)
           ((characterp object) (index (char-code object)))
           (t (error 'simple-type-error
                     :datum object :expected-type '(integer 0 127)
                     :format-control "~S does not designate an ASCII character"
                     :format-arguments (list object)))))
       (range (array start end)
         (loop
           for k upfrom (index start) to (index end)
           do (setf (sbit array k) 1))
         array)
       (evaluate (object &optional (array (make-bitmap)))
         (etypecase object
           (ascii-charset (unless (eq array object) (replace array object)))
           (character (setf (sbit array (index object)) 1))
           (integer (setf (sbit array (index object)) 1))
           (string (map nil (lambda (ch) (setf (sbit array (index ch)) 1)) object))
           (symbol
            (ecase object
              ((:any) (range array 0 127))
              ((:none))
              ((:alpha) (range array #\a #\z) (range array #\A #\Z))
              ((:digit) (range array #\0 #\9))
              ((:hex-digit) (range array #\0 #\9) (range array #\a #\f) (range array #\A #\F))))
           (cons
            (ecase (car object)
              ((:range) (range array (cadr object) (caddr object)))
              ((:union)
               (loop for s in (cdr object)
                     do (bit-ior array (evaluate s) array))))))
         array))
    (evaluate description)))

(defun char-match-p (char charset)
  (declare (type character char) (type ascii-charset charset))
  (let ((code (char-code char)))
    (and (<= 0 code 127)
         (not (zerop (sbit charset code))))))

(defparameter +unreserved-chars+ (make-ascii-charset '(:union :alpha :digit "-._~")))
(defparameter +sub-delim-chars+ (make-ascii-charset "!$&'()*+,;="))
(defparameter +pchar-chars+ (make-ascii-charset `(:union ,+unreserved-chars+ ,+sub-delim-chars+ ":@")))
(defparameter +path-chars+ (make-ascii-charset `(:union ,+pchar-chars+ #\/)))
(defparameter +query-chars+ (make-ascii-charset `(:union ,+pchar-chars+ "/?")))
(defparameter +fragment-chars+ (make-ascii-charset `(:union ,+pchar-chars+ "/?")))
(defparameter +scheme-start-chars+ (make-ascii-charset :alpha))
(defparameter +scheme-chars+ (make-ascii-charset `(:union :alpha :digit ".-+")))
(defparameter +userinfo-chars+ (make-ascii-charset `(:union ,+unreserved-chars+ ,+sub-delim-chars+ #\:)))
(defparameter +regname-chars+ (make-ascii-charset `(:union ,+unreserved-chars+ ,+sub-delim-chars+)))
(defparameter +hex-chars+ (make-ascii-charset :hex-digit))

(defun decode-unreserved-characters (string)
  (let* ((length (length string))
         (buffer (make-array length :element-type 'character :fill-pointer 0))
         found-any)
    (labels
        ((to-simple-string (string)
           (if (typep string 'simple-string) string
               (let ((copy (make-string (length string))))
                 (replace copy string)
                 copy)))
         (unescape (position)
           (let* ((digit1 (digit-char-p (char string position) 16))
                  (digit2 (digit-char-p (char string (1+ position)) 16))
                  (code (dpb digit1 (byte 4 4) digit2)))
             (if (and (< code 128) (not (zerop (sbit +unreserved-chars+ code))))
                 (progn
                   (vector-push-extend (code-char code) buffer)
                   (setf found-any t))
                 (progn
                   (vector-push-extend #\% buffer)
                   (vector-push-extend (char-upcase (char string position)) buffer)
                   (vector-push-extend (char-upcase (char string (1+ position))) buffer)))
             (scan (+ position 2))))
         (scan (position)
           (if (>= position length)
               (if found-any
                   (to-simple-string buffer)
                   (to-simple-string string))
               (let ((char (char string position)))
                 (if (eql char #\%)
                     (unescape (1+ position))
                     (progn
                       (vector-push-extend char buffer)
                       (scan (1+ position))))))))
      (scan 0))))

(defun decode-host-characters (string)
  (let* ((length (length string))
         (buffer (make-array length :element-type 'character :fill-pointer 0))
         found-any)
    (labels
        ((to-simple-string (string)
           (if (typep string 'simple-string) string
               (let ((copy (make-string (length string))))
                 (replace copy string)
                 copy)))
         (unescape (position)
           (let* ((digit1 (digit-char-p (char string position) 16))
                  (digit2 (digit-char-p (char string (1+ position)) 16))
                  (code (dpb digit1 (byte 4 4) digit2)))
             (if (and (< code 128) (not (zerop (sbit +unreserved-chars+ code))))
                 (progn
                   (vector-push-extend (char-downcase (code-char code)) buffer)
                   (setf found-any t))
                 (progn
                   (vector-push-extend #\% buffer)
                   (vector-push-extend (char-upcase (char string position)) buffer)
                   (vector-push-extend (char-upcase (char string (1+ position))) buffer)))
             (scan (+ position 2))))
         (scan (position)
           (if (>= position length)
               (if found-any
                   (to-simple-string buffer)
                   (to-simple-string string))
               (let ((char (char string position)))
                 (if (eql char #\%)
                     (unescape (1+ position))
                     (progn
                       (vector-push-extend (char-downcase char) buffer)
                       (scan (1+ position))))))))
      (scan 0))))

;;; Splits an URI into its components, namely scheme, user-info, host, port, path,
;;; query, and fragment. Except for the path, any of these components may be missing
;;; in some form or another in a valid URI. If the URI can successfully be parsed,
;;; the result is a list of the form
;;;
;;;   (<path> [:scheme <scheme>] [:user <user-info>] [:host <host>] [:port <port>]
;;;           [:query <query>] [:fragment <fragment>])
;;;
;;; with each of the marked parts present, if the associated URI component was
;;; present in the input URI.
;;;
;;; With the exception of the <host> component, all components are validated in a
;;; very limited way (making sure, that they contain only allowed characters.) The
;;; host component is only cursory looked at by this function.

(defun split-uri (string &key (start 0) end junk-allowed)
  (let* ((string (string string))
         (end (or end (length string))))
    (labels
        ((fail (&optional control &rest arguments)
           (if junk-allowed
               (return-from split-uri nil)
               (let ((input `(,string :start ,start :end ,end)))
                 (if (not control)
                     (error 'uri-parse-error :input input)
                     (error 'simple-uri-parse-error
                            :input input
                            :format-control control
                            :format-arguments arguments)))))
         (yield (scheme user host port path query fragment)
           (cons (decode-unreserved-characters path)
                 (nconc (and scheme (list :scheme scheme))
                        (and user (list :user (decode-unreserved-characters user)))
                        (and host (list :host host))
                        (and port (list :port port))
                        (and query (list :query (decode-unreserved-characters query)))
                        (and fragment (list :fragment (decode-unreserved-characters fragment))))))
         (normalize-ip6 (string start end)
           (let ((ip6 (if (and (> (- end start) 3) (string-equal "v1." string :start2 start :end2 (+ start 3)))
                          (parse-ipv6-address string :start (+ start 3) :end end :junk-allowed t)
                          (parse-ipv6-address string :start start :end end :junk-allowed t))))
             (if ip6
                 (address-string ip6 :prefix "[" :suffix "]")
                 (fail "malformed IPv6 address in domain literal: ~S" (subseq string start end)))))
         (split-authority (authority)
           (if (not (eql (char authority 0) #\[))
               (let* ((colon (position #\: authority :from-end t))
                      (host (if colon (subseq authority 0 colon) authority)))
                 (cond
                   ((not (component-match host +regname-chars+)) (fail "invalid characters in host component ~S" host))
                   ((or (not colon) (eql (1+ colon) (length authority))) (values (decode-host-characters host) nil))
                   (t (let ((port (parse-integer authority :start (1+ colon) :junk-allowed t :radix 10)))
                        (if (and port (<= 0 port 65535))
                            (values (decode-host-characters host) port)
                            (fail "invalid TCP port number in authority component ~S" authority))))))
               (let ((close (position #\] authority :from-end t)))
                 (if (not close)
                     (fail "unterminated domain literal in authority component ~S" authority)
                     (let ((colon (position #\: authority :from-end t :start (1+ close))))
                       (cond
                         ((not (or (eql close (1- (length authority))) (eql colon (1+ close))))
                          (fail "junk after domain literal in authority component ~S" authority))
                         ((or (not colon) (eql (1+ colon) (length authority)))
                          (values (normalize-ip6 authority 1 close) nil))
                         (t
                          (let ((port (parse-integer authority :start (1+ colon) :radix 10 :junk-allowed t)))
                            (if (and port (<= 0 port 65535))
                                (values (normalize-ip6 authority 1 close) port)
                                (fail "invalid TCP port number in authority component ~S"
                                      authority))))))))))
         (end-of-authority-p (ch) (or (eql ch #\/) (eql ch #\?) (eql ch #\#)))
         (end-of-path-p (ch) (or (eql ch #\?) (eql ch #\#)))
         (end-of-query-p (ch) (eql ch #\#))
         (component-match (value charset)
           (labels
               ((scan (p)
                  (or (>= p (length value))
                      (let ((ch (char value p)))
                        (cond
                          ((char-match-p ch charset) (scan (1+ p)))
                          ((eql ch #\%) (escaped (1+ p)))
                          (t nil)))))
                (escaped (p)
                  (and (>= (- (length value) p) 2)
                       (char-match-p (char value p) +hex-chars+)
                       (char-match-p (char value (1+ p)) +hex-chars+)
                       (scan (+ p 2)))))
             (scan 0)))
         (scan-scheme (p)
           (and (< p end)
                (let ((char (char string p)))
                  (cond
                    ((eql char #\:) p)
                    ((not (char-match-p char +scheme-chars+)) nil)
                    (t (scan-scheme (1+ p)))))))
         (scan-until (test p)
           (cond
             ((>= p end) p)
             ((funcall test (char string p)) p)
             (t (scan-until test (1+ p))))))
      (let ((position start)
            scheme authority path query fragment)
        (when (and (< start end) (char-match-p (char string position) +scheme-start-chars+))
          (let ((new-position (scan-scheme (1+ position))))
            (when new-position
              (setf scheme (subseq string position new-position))
              (setf position (1+ new-position)))))
        (when (and (>= (- end position) 2) (eql (char string position) #\/) (eql (char string (1+ position)) #\/))
          (incf position 2)
          (let ((new-position (scan-until #'end-of-authority-p position)))
            (setf authority (subseq string position new-position))
            (setf position new-position)))
        (let ((new-position (scan-until #'end-of-path-p position)))
          (setf path (subseq string position new-position))
          (setf position new-position))
        (when (and (< position end) (eql (char string position) #\?))
          (incf position)
          (let ((new-position (scan-until #'end-of-query-p position)))
            (setf query (subseq string position new-position))
            (setf position new-position)))
        (when (and (< position end) (eql (char string position) #\#))
          (incf position)
          (setf fragment (subseq string position end))
          (setf position end))
        (cond
          ((< position end) (fail "junk characters after URI"))
          ((and path (not (component-match path +path-chars+))) (fail "invalid characters in path component ~S" path))
          ((and query (not (component-match query +query-chars+))) (fail "invalid characters in query component ~S" query))
          ((and fragment (not (component-match fragment +fragment-chars+))) (fail "invalid characters in fragment component ~S" fragment))
          ((not authority) (yield scheme nil nil nil path query fragment))
          ((zerop (length authority)) (yield scheme nil "" nil path query fragment))
          (t (let ((delimiter (position #\@ authority)))
               (if (not delimiter)
                   (multiple-value-bind (host port) (split-authority authority)
                     (yield scheme nil host port path query fragment))
                   (let ((user (subseq authority 0 delimiter))
                         (server (subseq authority (1+ delimiter))))
                     (unless (component-match user +userinfo-chars+) (fail "invalid characters in user-info component ~S" user))
                     (multiple-value-bind (host port) (split-authority server)
                       (yield scheme user host port path query fragment)))))))))))


(defstruct (uri (:copier nil) (:conc-name uri-) (:predicate urip)
                (:constructor make-uri-1))
  (%string nil :type (or null string))
  (scheme nil :type (or null string) :read-only t)
  (user nil :type (or null string) :read-only t)
  (host nil :type (or null string) :read-only t)
  (port nil :type (or null (integer 0 65535)) :read-only t)
  (path "" :type string :read-only t)
  (query nil :type (or null string) :read-only t)
  (fragment nil :type (or null string) :read-only t))

(defmethod make-load-form ((object uri) &optional environment)
  (declare (ignore environment))
  (labels
      ((maybe (key value) (and value (list key value))))
    `(make-uri-1 :path ,(uri-path object)
                 ,@(maybe :scheme (uri-scheme object))
                 ,@(maybe :user (uri-user object))
                 ,@(maybe :host (uri-host object))
                 ,@(maybe :port (uri-port object))
                 ,@(maybe :query (uri-query object))
                 ,@(maybe :fragment (uri-fragment object)))))  

(defun uri-authority (object)
  (let ((user (uri-user object)) (host (uri-host object))
        (port (uri-port object)))
    (and (or user host port)
         (format nil "~@[~A@~]~@[~A~]~@[:~D~]"
                 user host port))))

(defun uri-string (object)
  (or (uri-%string object)
      (setf (uri-%string object)
            (format nil "~@[~A:~]~@[//~A~]~A~@[?~A~]~@[~A~]"
                    (uri-scheme object) (uri-authority object)
                    (uri-path object) (uri-query object)
                    (uri-fragment object)))))

(defun uri-equal (u1 u2)
  (string= (uri-string u1) (uri-string u2)))

(defun uri-hash (object)
  (sxhash (uri-string object)))

#+SBCL
(sb-ext:define-hash-table-test uri-equal uri-hash)

(defun parse-uri (string &key (start 0) end junk-allowed)
  (let ((data (split-uri string :start start :end end :junk-allowed junk-allowed)))
    (apply #'make-uri-1 :path (car data) (cdr data))))

(defgeneric uri (object)
  (:method ((object t)) (error 'type-error :datum object :expected-type 'uri))
  (:method ((object uri)) object)
  (:method ((object string)) (or (parse-uri object :junk-allowed t) (call-next-method))))

(defun remove-dot-segments (path)
  (labels
      ((copy-segment (list drops result)
         (cond
           ((null list) result)
           ((string= "." (car list)) (copy-segment (cdr list) drops result))
           ((string= ".." (car list)) (copy-segment (cdr list) (1+ drops) result))
           ((zerop drops) (copy-segment (cdr list) 0 (cons (car list) result)))
           (t (copy-segment (cdr list) (1- drops) result)))))
    (let* ((segments (split-sequence #\/ path))
           (absolute (and segments (zerop (length (car segments)))))
           (new-path (copy-segment (reverse (if absolute (cdr segments) segments)) 0 nil)))
      (with-output-to-string (stream)
        (loop
          for segment in new-path
          for first = (not absolute) then nil
          do (unless first (write-char #\/ stream))
             (write-string segment stream))))))

(defun merge-uri-paths (reference base)
  (let ((r-path (uri-path reference)))
    (if (and (plusp (length r-path)) (eql (char r-path 0) #\/))
        r-path
        (let ((b-path (uri-path base)))
          (if (zerop (length b-path))
              (if (or (uri-user base) (uri-host base) (uri-port base))
                  (remove-dot-segments (concatenate 'string "/" r-path))
                  (remove-dot-segments r-path))
              (let* ((slash (position #\/ b-path :from-end t))
                     (prefix (if slash (subseq b-path 0 (1+ slash)) "")))
                (remove-dot-segments (concatenate 'string prefix r-path))))))))

(defun resolve-uri (reference base &key (start 0) end junk-allowed (strict t))
  (let ((reference (parse-uri reference :start start :end end :junk-allowed junk-allowed)))
    (and reference
         (let ((base (uri base)))
                      (let ((r-scheme (uri-scheme reference)) (r-user (uri-user reference))
                 (r-host (uri-host reference)) (r-port (uri-port reference))
                 (r-path (uri-path reference)) (r-query (uri-query reference))
                 (r-fragment (uri-fragment reference))
                 (b-scheme (uri-scheme base)) (b-user (uri-user base))
                 (b-host (uri-host base)) (b-port (uri-port base))
                 (b-path (uri-path base)) (b-query (uri-query base)))
             (cond
               ((and r-scheme (or strict (not (equalp r-scheme b-scheme))))
                (make-uri-1 :scheme r-scheme :user r-user :host r-host
                            :port r-port :path (remove-dot-segments r-path)
                            :query r-query :fragment r-fragment))
               ((or r-user r-host r-port)
                (make-uri-1 :scheme b-scheme :user r-user :host r-host
                            :port r-port :path (remove-dot-segments r-path)
                            :query r-query :fragment r-fragment))
               ((zerop (length r-path))
                (make-uri-1 :scheme b-scheme :user b-user :host b-host
                            :port b-port :path b-path
                            :query (or r-query b-query) :fragment r-fragment))
               (t
                (make-uri-1 :scheme b-scheme :user b-user :host b-host
                            :port b-port :path (merge-uri-paths reference base)
                            :query (or r-query b-query) :fragment r-fragment))))))))

(defmethod print-object ((object uri) stream)
  (if (not *print-escape*)
      (write-string (uri-string object) stream)
      (print-unreadable-object (object stream :type t :identity t)
        (write-string (uri-string object) stream)))
  object)

(defun uri-scheme-string-p (object)
  (and (stringp object)
       (let ((length (length object)))
         (and (plusp length)
              (char-match-p (char object 0) +scheme-start-chars+)
              (loop
                for k upfrom 1 below length
                always (char-match-p (char object k) +scheme-chars+))))))

(defun canonicalize-uri-component (string charset caser name)
  (let* ((digits "0123456789ABCDEF")
         (string (string string))
         (buffer (make-array (length string) :element-type 'character :fill-pointer 0)))
    (labels
        ((add (char) (vector-push-extend (if caser (funcall caser char) char) buffer))
         (scan (position)
           (if (>= position (length string))
               (let ((copy (make-string (length buffer))))
                 (replace copy buffer)
                 copy)
               (let ((char (char string position)))
                 (cond
                   ((char-match-p char charset) (add char) (scan (1+ position)))
                   ((eql char #\%) (escaped (1+ position)))
                   (t (error "malformed value ~S for URI component ~S" string name))))))
         (escaped (position)
           (if (> (+ position 2) (length string))
               (error "malformed value ~S for URI component ~S" string name)
               (let ((digit1 (digit-char-p (char string position) 16))
                     (digit2 (digit-char-p (char string (1+ position)) 16)))
                 (if (not (and digit1 digit2))
                     (error "malformed value ~S for URI component ~S" string name)
                     (let ((code (dpb digit1 (byte 4 4) digit2)))
                       (if (and (< code 128) (not (zerop (sbit +unreserved-chars+ code))))
                           (add (code-char code))
                           (progn
                             (add #\%) (add (char digits digit1))
                             (add (char digits digit2))))
                       (scan (+ position 2))))))))
      (scan 0)
      (let ((copy (make-string (length buffer))))
        (replace copy buffer)
        copy))))

(defun escape-uri-component (string charset encoding)
  (let* ((digits "0123456789ABCDEF")
         (octets (babel:string-to-octets string :encoding encoding :use-bom nil :errorp t))
         (buffer (make-array (* 3 (length octets)) :element-type 'character :fill-pointer 0)))
    (loop
      for octet across octets
      do (if (and (< octet 128) (not (zerop (sbit charset octet))))
             (vector-push-extend (code-char octet) buffer)
             (progn
               (vector-push-extend #\% buffer)
               (vector-push-extend (char digits (ldb (byte 4 4) octet)) buffer)
               (vector-push-extend (char digits (ldb (byte 4 0) octet)) buffer))))
    (let ((copy (make-string (length buffer))))
      (replace copy buffer)
      copy)))

(defun escape-uri-path (path &key (encoding :utf-8))
  (etypecase path
    (string (escape-uri-component path +path-chars+ encoding))
    (null "")))

(defun escape-uri-query (string &key (encoding :utf-8))
  (escape-uri-component string +query-chars+ encoding))

(defun escape-uri-fragment (string &key (encoding :utf-8))
  (escape-uri-component string +fragment-chars+ encoding))

(defun escape-uri-user (string &key (encoding :utf-8))
  (escape-uri-component string +userinfo-chars+ encoding))

(defun escape-uri-host (host &key (encoding :utf-8))
  (etypecase host
    (ipv4-address (address-string host))
    (ipv6-address (address-string host :prefix "[" :suffix "]"))
    (host-name (address-string host))
    (string (escape-uri-component (string-downcase host) +regname-chars+ encoding))))

(defun canonicalize-uri-path (string)
  (if (zerop (length string)) ""
      (canonicalize-uri-component string +path-chars+ #'identity :path)))
                     
(defun canonicalize-uri-query (string)
  (if (zerop (length string)) ""
      (canonicalize-uri-component string +query-chars+ #'identity :query)))

(defun canonicalize-uri-fragment (string)
  (if (zerop (length string)) ""
      (canonicalize-uri-component string +fragment-chars+ #'identity :fragment)))

(defun canonicalize-uri-user (string)
  (if (zerop (length string)) ""
      (canonicalize-uri-component string +userinfo-chars+ #'identity :user)))

(defun canonicalize-uri-host/regname (string)
  (if (zerop (length string)) ""
      (canonicalize-uri-component string +regname-chars+ #'char-downcase :host)))

(defun canonicalize-uri-host (string)
  (etypecase string
    (ipv4-address (address-string string))
    (ipv6-address (address-string string :prefix "[" :suffix "]"))
    (host-name (address-string string))
    (string
     (if (zerop (length string)) ""
         (if (not (eql (char string 0) #\[))
             (canonicalize-uri-component string +regname-chars+ #'char-downcase :host)
             (let ((length (length string)))
               (if (not (eql (char string (1- length)) #\]))
                   (error "malformed value ~S for URI component ~S" string :host)
                   (let ((address (parse-ipv6-address string
                                                      :junk-allowed t :end (1- length)
                                                      :start (if (and (> length 4) (string-equal "V1." string :start2 1 :end2 4))
                                                                 4 1))))
                     (address-string address :prefix "[" :suffix "]")))))))))

(defun make-uri (&key
                   scheme authority user host port path query fragment
                   raw-user raw-host raw-path raw-query raw-fragment
                   (encoding :utf-8))
  (let ((scheme* (and scheme
                      (if (not (uri-scheme-string-p scheme))
                          (error 'type-error :datum scheme :expected-type '(and string (satisfies uri-scheme-string-p)))
                          (string-downcase scheme))))
        (path* (cond
                 ((and raw-path path) (error "cannot use ~S and ~S at the same time" :path :raw-path))
                 (path (canonicalize-uri-path path))
                 (raw-path (escape-uri-path raw-path :encoding encoding))
                 (t "")))
        (query* (cond
                  ((and raw-query query) (error "cannot use ~S and ~S at the same time" :query :raw-query))
                  (query (canonicalize-uri-query query))
                  (raw-query (escape-uri-query raw-query :encoding encoding))
                  (t nil)))
        (fragment* (cond
                     ((and raw-fragment fragment) (error "cannot use ~S and ~S at the same time" :fragment :raw-fragment))
                     (fragment (canonicalize-uri-fragment fragment))
                     (raw-fragment (escape-uri-fragment raw-fragment :encoding encoding))
                     (t nil)))
        user* host* port*)
    (when authority
      (when (or user host port raw-user raw-host)
        (error "cannot use ~S and one of ~S, ~S, ~S, ~S, ~S at the same time"
               :authority :user :host :port :raw-user :raw-host))
      (let ((delimiter (position #\@ authority)))
        (when delimiter
          (setf user (subseq authority 0 delimiter))
          (setf authority (subseq authority (1+ delimiter))))
        (let ((colon (position #\: authority :from-end t))
              (close (position #\] authority :from-end t)))
          (if (and colon (or (not close) (< close colon)))
              (progn
                (setf host (subseq authority 0 colon))
                (setf authority (subseq authority (1+ colon))))
              (progn
                (setf host authority)
                (setf authority nil)))
          (when (and authority (plusp (length authority)))
            (setf port (or (parse-integer authority :radix 10 :junk-allowed t)
                           (error "malformed value ~S for ~S in ~S argument"
                                  authority :port :authority)))))))
    (setf user* (cond
                  ((and raw-user user) (error "cannot use ~S and ~S at the same time" :user :raw-user))
                  (user (canonicalize-uri-user user))
                  (raw-user (escape-uri-user raw-user :encoding encoding))
                  (t nil)))
    (setf host* (cond
                  ((and raw-host host) (error "cannot use ~S and ~S at the same time" :host :raw-host))
                  (host (canonicalize-uri-host host))
                  (raw-host (escape-uri-host raw-host :encoding encoding))
                  (t nil)))
    (setf port* (and port
                     (if (typep port '(integer 0 65535)) port
                         (error 'type-error :datum port :expected-type '(integer 0 65535)))))
    (make-uri-1 :scheme scheme* :user user* :host host* :port port*
                :path path* :query query* :fragment fragment*)))

#||

Regex in CL-PPCRE format: ^(?:([^:/?#]+):)?(?://([^/?#]*))?([^?#]*)(?:\\?([^#]*))?(?:#(.*))?
Grammar from https://tools.ietf.org/html/rfc3986

   URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]

   hier-part     = "//" authority path-abempty
                 / path-absolute
                 / path-rootless
                 / path-empty

   URI-reference = URI / relative-ref

   absolute-URI  = scheme ":" hier-part [ "?" query ]

   relative-ref  = relative-part [ "?" query ] [ "#" fragment ]

   relative-part = "//" authority path-abempty
                 / path-absolute
                 / path-noscheme
                 / path-empty

   scheme        = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )

   authority     = [ userinfo "@" ] host [ ":" port ]
   userinfo      = *( unreserved / pct-encoded / sub-delims / ":" )
   host          = IP-literal / IPv4address / reg-name
   port          = *DIGIT

   IP-literal    = "[" ( IPv6address / IPvFuture  ) "]"

   IPvFuture     = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )

   IPv6address   =                            6( h16 ":" ) ls32
                 /                       "::" 5( h16 ":" ) ls32
                 / [               h16 ] "::" 4( h16 ":" ) ls32
                 / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
                 / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
                 / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
                 / [ *4( h16 ":" ) h16 ] "::"              ls32
                 / [ *5( h16 ":" ) h16 ] "::"              h16
                 / [ *6( h16 ":" ) h16 ] "::"

   h16           = 1*4HEXDIG
   ls32          = ( h16 ":" h16 ) / IPv4address
   IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet

   dec-octet     = DIGIT                 ; 0-9
                 / %x31-39 DIGIT         ; 10-99
                 / "1" 2DIGIT            ; 100-199
                 / "2" %x30-34 DIGIT     ; 200-249
                 / "25" %x30-35          ; 250-255

   reg-name      = *( unreserved / pct-encoded / sub-delims )

   path          = path-abempty    ; begins with "/" or is empty
                 / path-absolute   ; begins with "/" but not "//"
                 / path-noscheme   ; begins with a non-colon segment
                 / path-rootless   ; begins with a segment
                 / path-empty      ; zero characters

   path-abempty  = *( "/" segment )
   path-absolute = "/" [ segment-nz *( "/" segment ) ]
   path-noscheme = segment-nz-nc *( "/" segment )
   path-rootless = segment-nz *( "/" segment )
   path-empty    = 0<pchar>

   segment       = *pchar
   segment-nz    = 1*pchar
   segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
                 ; non-zero-length segment without any colon ":"

   pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

   query         = *( pchar / "/" / "?" )

   fragment      = *( pchar / "/" / "?" )

   pct-encoded   = "%" HEXDIG HEXDIG

   unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
   reserved      = gen-delims / sub-delims
   gen-delims    = ":" / "/" / "?" / "#" / "[" / "]" / "@"
   sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
                 / "*" / "+" / "," / ";" / "="
||#
