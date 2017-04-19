(in-package #:cl-user)
(defpackage #:thelonious
  (:use #:cl))
(in-package #:thelonious)

(defun queue ()
  (cons NIL NIL))

(defun queue-empty-p (notes)
  (null (car notes)))

(defun queue-peek (notes)
  (caar notes))

(defun queue-length (notes)
  (length (car notes)))

(defun queue-push (notes note)
  (if (queue-empty-p notes)
      (setf (car notes) (cons note NIL)
            (cdr notes) (car notes))
      (setf (cddr notes) (cons note NIL)
            (cdr notes) (cddr notes))))

(defun queue-pop (notes)
  (unless (queue-empty-p notes)
    (let ((val (caar notes)))
      (setf (car notes) (cdar notes))
      val)))

(defun queue-as-list (notes)
  (copy-list (cdr notes)))
