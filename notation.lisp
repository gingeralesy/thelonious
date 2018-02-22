(in-package #:thelonious)

(defparameter *flat-char* #\u266D)

(defparameter *double-flat-char* #\u1D12B)

(defparameter *sharp-char* #\u266F)

(defparameter *double-sharp-char* #\u1D12A)

(defparameter *english-notation-regex*
  ;; The space in double sharp group is the character #\u1D12A
  ;; Similarly the double flat group has #\u1D12B
  (cl-ppcre:create-scanner "^([A-G])([bf♭]{0,2}|[𝄫])([♯#s]{0,2}|[x𝄪])([0-8])$"
                           :case-insensitive-mode T))

(defparameter *german-notation-regex*
  (cl-ppcre:create-scanner "^(,{0,2})(([A-H])|([a-h]))(is)?(e?s)?(['’´]{0,5})?$"
                           :case-insensitive-mode NIL))

(defparameter *black-keys-on-piano* '(2 4 7 9 11))

(define-condition invalid-key-notation-error (error)
  ((key :initarg :key :reader key)))

(defun invalid-key-notation-error (key)
  (error 'invalid-key-notation-error :key (format NIL "No such key ~a." key)))

(defun note-order (note)
  (let ((note (etypecase note
                (string (char (string-upcase note) 0))
                (character (char-upcase note)))))
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
  (- note (for:for ((black in *black-keys-on-piano*)
                    (count counting black))
            (while (<= black note))
            (returning (1- count)))))

(defun note-order-on-piano (note)
  (let ((note (typecase note
                (integer note)
                (T (note-order note)))))
    (for:for ((black in *black-keys-on-piano*))
      (when (<= black note) (incf note))
      (until (< note black)))
    note))

(defun note-from-order (number &key (notation :english))
  (ecase number
    (1 #\C)
    (2 #\D)
    (3 #\E)
    (4 #\F)
    (5 #\G)
    (6 #\A)
    (7 (ecase (ensure-keyword notation)
         (:english #\B)
         (:german #\H)))))

(defun ensure-tuning (tuning)
  (etypecase tuning
    ((or keyword symbol)
     (case (ensure-keyword tuning)
       ((:german :austrian) 443.0d0)
       (:swiss 442.0d0)
       (:von-kajaran 444.0d0)
       (T 440.0d0)))
    (number (ensure-double-float tuning))))

(defun notation (note octave &key intonation (notation :english))
  (let ((note (ensure-char note))
        (octave (ensure-integer octave)))
    (ecase (ensure-keyword notation)
      (:english (format NIL "~a~a~a"
                        note
                        (case (and intonation (ensure-keyword intonation))
                          (:flat *flat-char*)
                          (:sharp *sharp-char*)
                          (:double-flat *double-flat-char*)
                          (:double-sharp *double-sharp-char*)
                          (T ""))
                        octave))
      (:german
       (let ((sharpness (cond ((and (eql intonation :flat) (not (eql #\H note)))
                               (if (or (eql #\A note) (eql #\E note)) "s" "es"))
                              ((eql intonation :sharp) "is")
                              (T ""))))
         (if (< octave 3)
             (format NIL "~{~a~}~:@(~a~)~a"
                     (for:for ((i repeat (- 2 octave))
                               (commas collecting #\,))
                       (returning commas))
                     (if (and (eql intonation :flat) (eql note #\H)) #\B note)
                     sharpness)
             (format NIL "~(~a~)~a~{~a~}"
                     (if (and (eql intonation :flat) (eql note #\H)) #\B note)
                     sharpness
                     (for:for ((i repeat (- octave 3))
                               (syms collecting #\’))
                       (returning syms)))))))))

(defun german-notation->piano-key (key-string groups)
  (unless key-string (invalid-key-notation-error key-string))
  (let ((sharp-p (when (< 0 (length (aref groups 4))) (aref groups 4)))
        (flat-p (when (< 0 (length (aref groups 5))) (aref groups 5)))
        (low (when (< 0 (length (aref groups 2))) (length (aref groups 0))))
        (high (when (< 0 (length (aref groups 3))) (length (aref groups 6)))))
    (when (or (and high low) (and flat-p sharp-p))
      (invalid-key-notation-error key-string))
    (let ((note (char (if low (aref groups 2) (aref groups 3)) 0)))
      (+ (cond (flat-p -1) (sharp-p 1) (T 0))
         (cond
           ((and low (= low 3) (char-equal #\A note))
            1)
           ((and low (= low 3) (char-equal #\B note))
            3)
           ((and high (= high 6) (char-equal #\C note))
            88)
           ((and low (< low 3))
            (+ 3 (* (- 1 low) 12) (note-order-on-piano note)))
           ((and high (< high 6))
            (+ 27 (* high 12) (note-order-on-piano note)))
           (T (invalid-key-notation-error key-string)))))))

(defun english-notation->piano-key (key-string groups)
  (unless key-string (invalid-key-notation-error key-string))
  (let ((note (char (aref groups 0) 0))
        (flat (when (< 0 (length (aref groups 1))) (aref groups 1)))
        (sharp (when (< 0 (length (aref groups 2))) (aref groups 2)))
        (octave (parse-integer (aref groups 3))))
    (when (and flat sharp)
      (invalid-key-notation-error key-string))
    (+ (- (if (char-equal *double-flat-char* (char flat 0)) 2 (length sharp)))
       (if (char-equal *double-sharp-char* (char sharp 0)) 2 (length sharp))
       (cond
         ((= 0 octave)
          (cond ((char-equal #\A note) 1)
                ((char-equal #\B note) 3)
                (T (invalid-key-notation-error key-string))))
         ((= 8 octave)
          (cond ((char-equal #\C note) 88)
                (T (invalid-key-notation-error key-string))))
         (T (+ 3 (* (1- octave) 12) (note-order-on-piano note)))))))

(defun notation->piano-key (note)
  (unless (or (typep note 'string) (typep note 'symbol) (typep note 'keyword))
    (invalid-key-notation-error note))
  (let ((note (ensure-string note)))
    (multiple-value-bind (key-string groups)
        (cl-ppcre:scan-to-strings *english-notation-regex* note)
      (if (and key-string (< 0 (length key-string)))
          (english-notation->piano-key key-string groups)
          (multiple-value-bind (key-string groups)
              (cl-ppcre:scan-to-strings *german-notation-regex* note)
            (unless (and key-string (< 0 (length key-string)))
              (invalid-key-notation-error note))
            (german-notation->piano-key key-string groups))))))

(defun piano-key->pitch (key &key (tuning 440.0d0))
  (coerce (+ (- (ensure-tuning tuning) 440.0d0)
             (* 440.0d0 (expt 2.0d0 (/ (- key 49.0d0) 12.0d0))))
          'single-float))

(defun pitch->piano-key (pitch &key (tuning 440.0d0))
  (let ((pitch (ensure-double-float (- pitch (- 440.0d0 tuning)))))
    (let* ((key (round (+ 49.0d0 (/ (* 12.0d0 (log (/ pitch 440.0d0))) (log 2.0d0)))))
           (offset (- pitch (piano-key->pitch key :tuning tuning))))
      (values key (if (< (abs offset) 0.001d0) 0.0 (/ (round (* offset 1000.0)) 1000.0))))))

(defun piano-key->notation (key &key (notation :english))
  (unless (<= 1 key 88) (error (format NIL "Invalid key: ~a" key)))
  (let ((note-b (ecase (ensure-keyword notation) (:english #\B) (:german #\H))))
    (cond
      ((< key 4)
       (notation (if (= key 3) note-b #\A) 0 :intonation (when (= key 2) :sharp)
                                             :notation notation))
      ((= key 88) (notation #\C 8 :notation notation))
      (T (let ((key-of-octave (mod (- key 3) 12)))
           (when (= 0 key-of-octave) (setf key-of-octave 12))
           (notation (note-from-order (note-order-from-piano key-of-octave) :notation notation)
                     (floor (+ key 8) 12)
                     :intonation (when (find key-of-octave *black-keys-on-piano*) :sharp)
                     :notation notation))))))

(defun notation->pitch (note &key (tuning 440.0d0))
  (piano-key->pitch (notation->piano-key note) :tuning tuning))

(defun pitch->notation (pitch &key (tuning 440.0d0) (notation :english))
  (multiple-value-bind (key offset)
      (pitch->piano-key pitch :tuning tuning)
    (values (piano-key->notation key :notation notation) offset)))
