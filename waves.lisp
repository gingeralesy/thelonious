(in-package #:thelonious)

(defvar *out* NIL)

(defun initialize-playback ()
  (when *out* (cl-out123:disconnect *out*))
  (setf *out* (cl-out123:connect (cl-out123:make-output NIL :channels 1
                                                            :encoding :float)
                                 :driver "pulse")))

(defun sine-wave (duration &key (frequency 440) (sample-rate 44100))
  (let ((length (* duration sample-rate)))
    (make-array length :element-type 'single-float
                       :initial-contents
                       (let* ((constant (* 2 (coerce pi 'single-float)))
                              (pos-increase (/ frequency sample-rate))
                              (pos-limit (- 1 pos-increase)))
                         (loop for i from 1 to length
                               for pos = 0 then (+ pos pos-increase
                                                   (if (<= pos pos-limit) 0 -1))
                               collect (sin (* pos constant)))))))

(defun square-wave (duration &key (frequency 440) (sample-rate 44100))
  (let ((length (* duration sample-rate))
        (switch (/ sample-rate frequency 2)))
    (make-array length :element-type 'single-float
                       :initial-contents
                       (loop for i from 1 to length
                             collect (if (= 0 (mod (floor (/ i switch)) 2)) 1.0s0 -1.0s0)))))

(defun triangle-wave (duration &key (frequency 440) (sample-rate 44100))
  (let ((length (floor (* duration sample-rate))))
    (make-array length :element-type 'single-float
                       :initial-contents
                       (let* ((y-inc (/ 4.0s0 (/ sample-rate frequency)))
                              (max-limit (- 1.0s0 y-inc))
                              (min-limit (+ -1.0s0 y-inc))
                              (going-up T))
                         (loop for i from 0 to (1- length)
                               for y = -1.0s0 then (+ y (if going-up y-inc (- y-inc)))
                               when (< max-limit y) do (setf going-up NIL)
                               when (< y min-limit) do (setf going-up T)
                               collect y)))))

(defun sawtooth-wave (duration &key (frequency 440) (sample-rate 44100))
  (let ((length (* duration sample-rate)))
    (make-array length :element-type 'single-float
                       :initial-contents
                       (let* ((constant (/ 2.0s0 (/ sample-rate frequency))))
                         (loop for i from 1 to length
                               for y = 1.0s0 then (- y constant)
                               when (< y -1.0s0) do (incf y 2.0s0)
                               collect y)))))

(defun combine-waves (waves &optional amplitudes)
  (dotimes (i (- (length waves) (length amplitudes)))
    (push 1 amplitudes))
  (let ((output (make-array (loop for wave in waves
                                  maximizing (array-dimension wave 0) into max
                                  finally (return max))
                            :element-type 'single-float
                            :initial-element 0.0s0)))
    (dotimes (i (array-dimension output 0))
      (loop for wave in waves
            for amp in amplitudes
            do (setf (aref output i) (+ (aref output i) (* amp (aref wave i))))
            finally (setf (aref output i) (/ (aref output i) (length waves)))))
    output))

(defun play (wave-types frequencies duration &optional amplitudes)
  (unless *out* (initialize-playback))
  (unless (typep wave-types 'list)
    (setf wave-types (list wave-types)))
  (unless (typep frequencies 'list)
    (setf frequencies (list frequencies)))
  (cl-out123:start *out*)
  (unwind-protect
       (let ((sample-rate (cl-out123:playback-format *out*)))
         (cl-out123:play *out*
                         (combine-waves (loop for frequency in frequencies
                                              for wave-type in wave-types
                                              collect
                                              (let ((wave-func
                                                      (ecase (alexandria:make-keyword wave-type)
                                                        ((:sine :sine-wave) #'sine-wave)
                                                        ((:square :square-wave) #'square-wave)
                                                        ((:triangle :triangle-wave) #'triangle-wave)
                                                        ((:sawtooth :sawtooth-wave) #'sawtooth-wave))))
                                                (funcall wave-func
                                                         duration
                                                         :frequency frequency
                                                         :sample-rate sample-rate)))
                                        amplitudes)))
    (cl-out123:stop *out*)))
