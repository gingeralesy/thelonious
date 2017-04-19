(in-package #:thelonious)

(defclass note () ())
(defgeneric in-millis (note)
  (:documentation "Returns the duration of this note in milliseconds."))

(defclass note-container () ())
(defgeneric clean (note-container)
  (:documentation "Removes all sub-pieces from this container."))
(defgeneric notes (note-container)
  (:documentation "Gets the notes in this container."))
(defgeneric add-note (note-container note &key)
  (:documentation "Add note to a container."))
(defgeneric (setf pan) (value note-container)
  (:documentation "Set the pan for all notes in the container."))

(defclass phrase (note-container) ())
(defclass part (note-container) ())
(defclass score (note-container) ())

(defclass note ()
  ((pitch :initform (error "A note must have a pitch.")
          :initarg :pitch
          :accessor pitch)
   (duration :initform (error "A note must have a duration.")
             :initarg :duration
             :accessor duration)
   (dynamic :initform 1 :initarg :dynamic :accessor dynamic)
   (pan :initform 0.5 :initarg :pan :accessor pan)
   (in-millis :reader in-millis))
  (:documentation "A representation of a note in standard music notation."))

(defmethod in-millis ((note note)))

(defclass note-container ()
  ((name :initarg name :accessor name))
  (:documentation "A container in the standard music notation with notes in it."))

(defclass phrase (note-container)
  ((notes :initform (queue))
   (start-time :initform 0 :initarg :start :accessor start))
  (:documentation "A representation of a phrase in standard music notation."))

(defmethod initialize-instance :after ((phrase phrase) &key notes name)
  (loop for note in notes do (add-note phrase note))
  (unless name (setf (name phrase) "Unnamed phrase")))

(defmethod notes ((phrase phrase))
  (queue-as-list (slot-value phrase 'notes)))

(defmethod add-note ((phrase phrase) (note note) &key)
  (queue-push (notes phrase) note))

(defmethod (setf pan) (value (phrase phrase))
  (loop for note in (notes phrase) do (setf (pan note) value)))

(defmethod (setf dynamic) (value (phrase phrase))
  (loop for note in (notes phrase) do (setf (dynamic note) value)))

(defmethod clean ((phrase phrase))
  (setf (slot-value phrase 'notes) (queue)))

(defclass part (note-container)
  ((channel :initform 0 :initarg :channel :accessor channel)
   (intrument :initform :piano :initarg :instrument :accessor :instrument)
   (phrases :initarg :phrases :accessor phrases)))

(defmethod initialize-instance :after ((part part) &key name)
  (unless name (setf (name part) "Unnamed part")))

(defmethod clean ((part part))
  (setf (phrases part) NIL))

(defclass score (note-container))
