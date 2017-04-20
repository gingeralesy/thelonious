(in-package #:cl-user)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (push :verbose-no-init *features*)
  #+quicklisp (ql:quickload :verbose)
  #-quicklisp (asdf:load-system :verbose))

(asdf:defsystem #:thelonious
  :name "Thelonious"
  :author "Janne Pakarinen <gingeralesy.gmail.org>"
  :version "0.1"
  :licence "GNU Lesser General Public License (version 2.1)"
  :description "For handling audio I/O in a midi-like manner for audio generation systems."
  :depends-on (:cl-out123 :cl-ppcre)
  :components ((:file "module")
               (:file "waves")
               (:file "notation")))
