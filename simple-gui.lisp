;;;; simple-gui.lisp
(in-package #:simple-gui)

(generate-all-qt-class-macros)
(generate-all-qt-methods)
(generate-all-qt-enums)

(defvar *q-objects* (make-hash-table)
  "Keep Tracking of all Qt GUI elements.")

(defmacro q-application (&rest args)
  "Return or create the singleton Qt application object."
  `(make-qapplication ,@args))


