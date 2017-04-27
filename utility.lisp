(in-package #:thelonious)

(defun queue ()
  (cons NIL NIL))

(defun queue-empty-p (queue)
  (null (car queue)))

(defun queue-peek (queue)
  (caar queue))

(defun queue-length (queue)
  (length (car queue)))

(defun queue-append (queue items)
  (for:for ((item in items))
    (queue-push queue item))
  queue)

(defun queue-push (queue item)
  (if (queue-empty-p queue)
      (setf (car queue) (cons item NIL)
            (cdr queue) (car queue))
      (setf (cddr queue) (cons item NIL)
            (cdr queue) (cddr queue)))
  queue)

(defun queue-pop (queue)
  (unless (queue-empty-p queue)
    (let ((val (caar queue)))
      (setf (car queue) (cdar queue))
      val)))

(defun queue-as-list (queue)
  (copy-list (car queue)))

(for:define-value-binding in-queue (var queue &aux (current (car queue)))
  `(if ,current
       (let ((value (car ,current)))
         (setf ,current (cdr ,current))
         (for:update ,var value))
       (for:end-for)))

(defun ensure-string (val)
  (format NIL "~a" val))

(defun ensure-char (val)
  (typecase val
    (character val)
    (T (char (ensure-string val) 0))))

(defun ensure-integer (number)
  (typecase number
    (integer number)
    (number (round number))
    (T (parse-integer (ensure-string number)))))

(defun ensure-double-float (number)
  (etypecase number
    (double-float number)
    (number (coerce number 'double-float))
    (string (coerce (ensure-integer number) 'double-float))))
