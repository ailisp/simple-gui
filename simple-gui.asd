;;;; cl-gui.asd

(defsystem #:simple-gui
  :name "simple-gui"
  :description "A declarative GUI definition tool for Common Lisp"
  :author "Bo Yao"
  :license "BSD"
  :version "0.1"
  :serial t
  :components ((:file "package")
	       (:file "utils" )
	       (:file "qt-utils")
               (:file "simple-gui"))
  :depends-on (:qt :alexandria :cl-ppcre :named-readtables))
