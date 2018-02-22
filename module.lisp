(in-package #:cl-user)
(defpackage #:thelonious
  (:use #:cl #:my-utils)
  (:export #:sine-wave
           #:square-wave
           #:triangle-wave
           #:sawtooth-wave
           #:play))
(in-package #:thelonious)
