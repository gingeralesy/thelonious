(in-package #:thelonious)

(define-condition invalid-key-notation-error (error)
  ((text :initarg :text :reader text)))

(defun invalid-key-notation-error (key)
  (error 'invalid-key-notation-error :text (format NIL "No such key ~a." key)))

(defun note-order (note)
  (let ((note (etypecase note
                (string (char note 0))
                (char note))))
    (ecase note
      (#\C 1)
      (#\D 2)
      (#\E 3)
      (#\F 4)
      (#\G 5)
      (#\A 6)
      ((#\B #\H) 7))))

(defun ensure-piano-key (key)
  (cond
    ((typep key 'integer) key)
    ((or (typep key 'string) (typep key 'symbol) (typep key 'keyword))
     ;; Notation is the standard English piano notation ([A-G])([#sbf])?([0-8])
     ;; where A-G is the note, # and s mean sharp, b and f mean flat, and 0-8 mean octave.
     ;; This is then converted into standard piano key number that is from 1 to 88.
     (multiple-value-bind (key-string groups)
         (cl-ppcre:scan-to-strings "([A-G])([#SBF])?([0-8])"
                                   (format NIL "~:@(~a~)" key))
       (unless key-string (invalid-key-notation-error key))
       (let ((note (char (aref groups 0) 0))
             (flat-p (when (and (aref groups 1) (or (string= "B" (aref groups 1))
                                                    (string= "F" (aref groups 1))))))
             (sharp-p (when (and (aref groups 1) (or (string= "#" (aref groups 1))
                                                     (string= "S" (aref groups 1))))))
             (octave (aref groups 2)))
         (unless (and note octave)
           (invalid-key-notation-error key-string))
         (+ (cond (flat-p -1) (sharp-p 1) (T 0))
            (cond
              ((= 0 octave)
               (cond ((string= "A" note) 1)
                     ((string= "B" note) 3)
                     (T (invalid-key-notation-error key))))
              ((= 8 octave)
               (cond ((string= "C" note) 88)
                     (T (invalid-key-notation-error key))))
              (T (+ 4 (* octave 7) (note-order note))))))))
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
             (* 440.0d0
                (expt 2.0d0 (/ (- (ensure-piano-key key) 49.0d0) 12.0d0))))
          'single-float))
