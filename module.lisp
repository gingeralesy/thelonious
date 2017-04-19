(in-package #:cl-user)
(defpackage #:thelonious
  (:use #:cl)
  (:export #:play))
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
  (loop for item in items
        do (queue-push queue item)))

(defun queue-push (queue item)
  (if (queue-empty-p queue)
      (setf (car queue) (cons item NIL)
            (cdr queue) (car queue))
      (setf (cddr queue) (cons item NIL)
            (cdr queue) (cddr queue))))

(defun queue-pop (queue)
  (unless (queue-empty-p queue)
    (let ((val (caar queue)))
      (setf (car queue) (cdar queue))
      val)))

(defun queue-as-list (queue)
  (copy-list (cdr queue)))
