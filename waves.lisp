(in-package #:thelonious)

(defvar *out* NIL)

(defun initialize-playback ()
  (when *out* (cl-out123:disconnect *out*))
  (setf *out* (cl-out123:connect (cl-out123:make-output NIL :channels 1
                                                            :encoding :float))))

(defun sine-wave (position frequency sample-rate)
  (let ((wave-length (floor sample-rate frequency)))
    (sin (* (/ (mod position wave-length) wave-length) (coerce pi 'single-float) 2))))

(defun square-wave (position frequency sample-rate)
  (let ((wave-length (floor sample-rate frequency)))
    (if (< (mod position wave-length) (/ wave-length 2)) 1.0s0 -1.0s0)))

(defun triangle-wave (position frequency sample-rate)
  (let* ((wave-length (floor sample-rate frequency))
         (quarter-wave (/ wave-length 4.0s0))
         (position (mod position wave-length))
         (multiplier (/ 4.0s0 wave-length)))
    (cond
      ((< position quarter-wave) (* position multiplier))
      ((<= (- wave-length quarter-wave) position) (* (- position wave-length) multiplier))
      (T (- 1.0s0 (* (- position quarter-wave) multiplier))))))

(defun sawtooth-wave (position frequency sample-rate)
  (let* ((wave-length (floor sample-rate frequency))
         (value (/ (mod position wave-length) (* 2.0s0 wave-length))))
    (if (< 1.0s0 value) (- value 2.0s0) value)))

(defun normalize-wave-spec (wave)
  (etypecase wave
    (list wave)
    (number (setf wave (list :frequency wave)))
    (function (setf wave (list :function wave)))))

(defun generate-wave (waves array sample-rate)
  (loop with div = (/ (length waves))
        for spec in waves
        for wave = (normalize-wave-spec spec)
        for frequency = (getf wave :frequency 440.0s0) then (getf wave :frequency frequency)
        for function = (getf wave :function #'sine-wave) then (getf wave :function function)
        for amplitude = (min 1.0s0 (max 0.0s0 (getf wave :amplitude 1.0s0)))
        then (min 1.0s0 (max 0.0s0 (getf wave :amplitude amplitude)))
        do (dotimes (i (length array))
             (incf (aref array i) (* div amplitude (funcall function i frequency sample-rate))))))

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
