(in-package #:cl-user)

(asdf:defsystem #:thelonious
  :name "Thelonious"
  :author "Janne Pakarinen <gingeralesy@gmail.org>"
  :version "0.1"
  :licence "GNU Lesser General Public License (version 2.1)"
  :description "For handling audio I/O in a midi-like manner for audio generation systems."
  :depends-on (:harmony-simple :cl-ppcre :for :my-utils)
  :components ((:file "module")
               (:file "source")
               (:file "waves")
               (:file "notation")))
