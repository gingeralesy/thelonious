(in-package #:thelonious)

(defparameter *english-notation-regex*
  (cl-ppcre:create-scanner "^([A-G])([bf♭]?)([#s]?)([0-8])$" :case-insensitive-mode T))

(defparameter *german-notation-regex*
  (cl-ppcre:create-scanner "^(,{0,2})(([AC-H])|([ac-h]))(i?s)?(e?s)?(['’]{0,5})?$"
                           :case-insensitive-mode NIL))

(defparameter *black-keys-on-piano* '(2 4 7 9 11))

(define-condition invalid-key-notation-error (error)
  ((text :initarg :text :reader text)))

(defun invalid-key-notation-error (key)
  (error 'invalid-key-notation-error :text (format NIL "No such key ~a." key)))

(defun note-order (note)
  (let ((note (etypecase note
                (string (char (string-upcase note) 0))
                (character note))))
    (ecase note
      (#\C 1)
      (#\D 2)
      (#\E 3)
      (#\F 4)
      (#\G 5)
      (#\A 6)
      ((#\B #\H) 7))))

(defun note-order-from-piano (note)
  (unless (typep note 'integer) (error (format NIL "Invalid note number: ~a." note)))
  (- note (length (loop for black in *black-keys-on-piano*
                        until (< note black)
                        collecting black))))

(defun note-order-on-piano (note)
  (let ((note (typecase note
                (integer note)
                (T (note-order note)))))
    (loop for black in *black-keys-on-piano*
          when (<= black note) do (incf note))
    note))

(defun note-from-order (number &key (notation 'english))
  (ecase number
    (1 #\C)
    (2 #\D)
    (3 #\E)
    (4 #\F)
    (5 #\G)
    (6 #\A)
    (7 (ecase notation
         (english #\B)
         (german #\H)))))

(defun notation (note octave &key flat-p sharp-p (notation 'english))
  (ecase notation
    (english (format NIL "~a~a~a"
                     note (cond (flat-p #\u266D) (sharp-p #\#) (T "")) octave))))

(defun ensure-german-piano-key (key-string groups)
  (when key-string
    (let ((sharp-p (aref groups 4))
          (flat-p (aref groups 5))
          (low (when (aref groups 2) (length (aref groups 0))))
          (high (when (aref groups 3) (length (aref groups 6)))))
      (when (or (and high low) (and flat-p sharp-p))
        (invalid-key-notation-error key-string))
      (let ((note (string-upcase (if low (aref groups 2) (aref groups 3)))))
        (+ (cond (flat-p -1) (sharp-p 1) (T 0))
           (cond
             ((and low (= low 3)
                   (string= "A" note)) 1)
             ((and low (= low 3)
                   (string= "B" note)) 3)
             ((and high (= high 6)
                   (string= "C" note)) 88)
             ((and low (< low 3))
              (format T "~a~%" low)
              (+ 3 (* (- 1 low) 12) (note-order-on-piano note)))
             ((and high (< high 6))
              (+ 27 (* high 12) (note-order-on-piano note)))
             (T (invalid-key-notation-error key-string))))))))

(defun ensure-english-piano-key (key-string groups)
  (when key-string
    (let ((note (char (aref groups 0) 0))
          (flat-p (aref groups 1))
          (sharp-p (aref groups 2))
          (octave (aref groups 3)))
      (when (and flat-p sharp-p)
        (invalid-key-notation-error key-string))
      (+ (cond (flat-p -1) (sharp-p 1) (T 0))
         (cond
           ((= 0 octave)
            (cond ((string= "A" note) 1)
                  ((string= "B" note) 3)
                  (T (invalid-key-notation-error key-string))))
           ((= 8 octave)
            (cond ((string= "C" note) 88)
                  (T (invalid-key-notation-error key-string))))
           (T (+ 3 (* (1- octave) 12) (note-order-on-piano note))))))))

(defun ensure-piano-key (key)
  (cond
    ((typep key 'integer) key)
    ((or (typep key 'string) (typep key 'symbol) (typep key 'keyword))
     ;; Notation is the standard English piano notation ([A-G])([#sbf])?([0-8])
     ;; where A-G is the note, # and s mean sharp, b and f mean flat, and 0-8 mean octave.
     ;; This is then converted into standard piano key number that is from 1 to 88.
     (let* ((key (format NIL "~a" key))
            (piano-key (multiple-value-bind (key-string groups)
                           (cl-ppcre:scan-to-strings *english-notation-regex* key)
                         (if key-string
                             (ensure-english-piano-key key-string groups)
                             (multiple-value-bind (key-string groups)
                                 (cl-ppcre:scan-to-strings *german-notation-regex* key)
                               (unless (and key-string (< 0 (length key-string)))
                                 (invalid-key-notation-error key))
                               (ensure-german-piano-key key-string groups))))))
       (unless piano-key (invalid-key-notation-error key))
       piano-key))
    (T (invalid-key-notation-error key))))

(defun ensure-double-float (number)
  (etypecase number
    (double-float number)
    (number (coerce number 'double-float))))

(defun ensure-pitch (pitch)
  (etypecase pitch
    (symbol (case pitch
              ((german austrian) 443.0d0)
              (swiss 442.0d0)
              (von-kajaran 444.0d0)
              (T 440.0d0)))
    (number (ensure-double-float pitch))))

(defun piano-key->pitch (key &key (tuning 440.0d0))
  (coerce (+ (- (ensure-pitch tuning) 440.0d0)
             (* 440.0d0 (expt 2.0d0 (/ (- (ensure-piano-key key) 49.0d0) 12.0d0))))
          'single-float))

(defun pitch->piano-key (pitch &key (tuning 440.0d0))
  (let ((pitch (ensure-double-float (- pitch (- 440.0d0 tuning)))))
    (let* ((key (round (+ 49.0d0 (/ (* 12.0d0 (log (/ pitch 440.0d0))) (log 2.0d0)))))
           (offset (- pitch (piano-key->pitch key :tuning tuning))))
      (values key (if (< (abs offset) 0.001d0) 0.0 (/ (round (* offset 1000.0)) 1000.0))))))

(defun piano-key->notation (key &key (notation 'english))
  (unless (<= 1 key 88) (error (format NIL "Invalid key: ~a" key)))
  (cond
    ((< key 4)
     (notation (if (= key 3) #\B #\A) 0 :sharp-p (= key 2) :notation notation))
    ((= key 88) (notation #\C 8 :notation notation))
    (T (let ((key-of-octave (mod (- key 3) 12)))
         (when (= 0 key-of-octave) (setf key-of-octave 12))
         (notation (note-from-order (note-order-from-piano key-of-octave))
                   (floor (+ key 8) 12)
                   :sharp-p (find key-of-octave *black-keys-on-piano*)
                   :notation notation)))))

(defun pitch->notation (pitch &key (tuning 440.0d0) (notation 'english))
  (multiple-value-bind (key offset)
      (pitch->piano-key pitch :tuning tuning)
    (values (piano-key->notation key :notation notation) offset)))
