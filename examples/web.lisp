(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :simple-gui)
  (use-package :q-))

(defun main ()
  (q-application)
  (q-main-window window
    (:set
     (:window-title "QWebView")
     (:geometry 100 100 800 600)
     (:central-widget
      (q-web-view web))))
  (qload (gui web) (q-new q-url "http://www.cliki.net/"))
  (show (gui window))
  (exec (q-application)))

