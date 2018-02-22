(in-package #:thelonious)

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
    (number (list :frequency wave))
    (function (list :function wave))))

(defun play (waves)
  (harmony-simple:initialize)
  (harmony-simple:start)
  (harmony-simple:play 'wave-source :music :waves waves))
