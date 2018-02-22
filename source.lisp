(in-package #:thelonious)

(defun normalize-wave-spec (wave)
  (etypecase wave
    (list wave)
    (number (list :frequency wave))
    (function (list :function wave))))

(defclass wave-source (harmony:source cl-mixed:virtual)
  ((wave-phase :initform 0 :accessor wave-phase)
   (waves :initarg :waves :accessor waves))
  (:default-initargs :waves (error "WAVES is undefined")))

(defmethod harmony:seek-to-sample ((source wave-source) position)
  (setf (wave-phase source) (mod position (harmony:samplerate (harmony:context source)))))

(defmethod harmony:sample-count ((source wave-source))
  T)

(defmethod harmony:process ((source wave-source) samples)
  (let ((buffers (cl-mixed:outputs source))
        (samplerate (harmony:samplerate (harmony:context source)))
        (waves (waves source))
        (phase (wave-phase source)))
    (for:for ((s repeat samples)
              (i = (1- s)))
      (for:for ((buffer across buffers))
        (setf (cffi:mem-aref (cl-mixed:data buffer) :float i) 0.0)))
    (loop with div = (/ (length waves))
          for spec in waves
          for wave = (normalize-wave-spec spec)
          for frequency = (getf wave :frequency 440.0s0) then (getf wave :frequency frequency)
          for function = (getf wave :function #'sine-wave) then (getf wave :function function)
          for amplitude = (min 1.0s0 (max 0.0s0 (getf wave :amplitude 1.0s0)))
          then (min 1.0s0 (max 0.0s0 (getf wave :amplitude amplitude)))
          do (for:for ((s repeat samples)
                       (i = (1- s)))
               (for:for ((buffer across buffers)
                         (sample = (+ (cffi:mem-aref (cl-mixed:data buffer) :float i)
                                      (* div amplitude (funcall function
                                                                (+ phase i) frequency samplerate)))))
                 (setf (cffi:mem-aref (cl-mixed:data buffer) :float i) sample))
               (incf phase)))
    (setf (wave-phase source) (mod phase samplerate))))
