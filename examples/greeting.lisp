(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :simple-gui)
  (use-package :q-))

(defun main ()
  (q-application)
  (q-widget main
    (:set
     (:window-title "What's your name?")
     (:layout
      (q-h-box-layout a
	(:add (q-line-edit name)
	      (q-push-button button
		(:set (:text "OK"))
		(:connect (:clicked
			   #'(lambda ()
			       (information 'q-message-box (gui main) "Simple GUI"
					    (concatenate 'string
							 "Hello, "
							 (text (gui name)))))))))))))
  (show (gui main))
  (exec (q-application)))
