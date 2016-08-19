;;;; package.lisp
(in-package #:cl-user)

(defpackage #:top.myfyb.utils
  (:use #:cl)
  (:export #:mkstr
	   #:group
	   #:mac
	   #:macq
	   #:join-str
	   #:singlep
	   #:memof))

(defpackage #:top.myfyb.simple-gui
  (:nicknames #:simple-gui)
  (:use #:cl #:top.myfyb.utils #:qt #:named-readtables #:ppcre #:alexandria #:iterate)
  (:export #:q-application
	   #:gui
	   #:q-new
	   #:it
	   #:q-connect
	   #:q-connect*))
