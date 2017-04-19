(in-package #:thelonious)

(defvar *out* NIL)

(defun initialize-playback ()
  (when *out* (cl-out123:disconnect *out*))
  (setf *out* (cl-out123:connect (cl-out123:make-output NIL :channels 1
                                                            :encoding :float)
                                 :driver "pulse")))

(defun sine-wave (position frequency sample-rate)
  (let ((wave-length (floor (/ sample-rate frequency))))
    (sin (* (/ (mod position wave-length) wave-length) (coerce pi 'single-float) 2))))

(defun square-wave (position frequency sample-rate)
  (let ((wave-length (floor (/ sample-rate frequency))))
    (if (< (mod position wave-length) (/ wave-length 2)) 1.0s0 -1.0s0)))

(defun triangle-wave (position frequency sample-rate)
  (let* ((wave-length (floor (/ sample-rate frequency)))
         (quarter-wave (/ wave-length 4.0s0))
         (position (mod position wave-length)))
    (cond
      ((< position quarter-wave) (* position 4.0s0))
      ((<= (- wave-length quarter-wave) position) (* (- position wave-length) 4.0s0))
      (T (- 1.0s0 (* (- position quarter-wave) 4.0s0))))))

(defun sawtooth-wave (position frequency sample-rate)
  (let* ((wave-length (floor (/ sample-rate frequency)))
         (value (* (mod position wave-length) 2.0s0)))
    (if (< 1.0s0 value) (- value 2.0s0) value)))

(defun generate-wave (waves array sample-rate)
  (let ((frequency 440)
        (function #'sine-wave)
        (amplitude 1.0s0))
    (dotimes (i (array-dimension array 0))
      (loop for wave in waves
            do (let ((wave wave))
                 (unless (typep wave 'list)
                   (etypecase wave
                     (number (setf wave (list :frequency wave)))
                     (function (setf wave (list :function wave)))))
                 (setf frequency (or (getf wave :frequency) frequency)
                       function (or (getf wave :function) function)
                       amplitude (or (when (getf wave :amplitude)
                                       (min 1.0s0 (max 0.0s0 (getf wave :amplitude))))
                                     amplitude))
                 (incf (aref array i) (* amplitude (funcall function i frequency sample-rate))))
            finally (setf (aref array i) (/ (aref array i) (length waves)))))))

(defun play (waves duration)
  (unless *out* (initialize-playback))
  (cl-out123:start *out*)
  (unwind-protect
       (let* ((sample-rate (cl-out123:playback-format *out*))
              (data (make-array (floor (* duration sample-rate))
                                :element-type 'single-float
                                :initial-element 0.0s0)))
         (generate-wave waves data sample-rate)
         (cl-out123:play *out* data))
    (cl-out123:stop *out*)))
