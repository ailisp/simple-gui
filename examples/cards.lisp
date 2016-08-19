(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :simple-gui)
  (use-package :q-))

(defparameter *rows* 6)
(defparameter *cols* 8)
(defparameter *block-size* 90)
(defparameter *kinds* 15)

(defvar *empty-icon*)
(defvar *current-flip* nil)
(defvar *board-total-size*)
(defvar *board*)
(defvar *board-1d*)

(defun build-data ()
  (setf *board-total-size* (* *rows* *cols*))
  (assert (evenp *board-total-size*))
  (setf *board-1d* (make-array *board-total-size* :element-type 'integer))
  (loop for i below (/ *board-total-size* 2) do
       (setf (aref *board-1d* i) (random *kinds*)
	     (aref *board-1d* (- *board-total-size* 1 i)) (aref *board-1d* i)))
  (setf *board-1d* (alexandria:shuffle *board-1d*))
  (setf *board* (make-array `(,*cols* ,*rows*) :displaced-to *board-1d*))
  (setf *empty-icon* (q-new q-icon "./pic/empty.png")))

(defun mkstr (&rest args)
  "Make a string from ARGS by princ."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun build-widgets ()
  (q-widget window
    (:set
     (:window-title "Cards Rolling-Over")
     (:geometry 200 200 800 600)
     (:layout (q-grid-layout grid))))
  (simple-gui::mkstr)
  (loop for i below *cols* do
       (loop for j below *rows* do
	    (progn
	      (add-widget (layout (gui grid))
			  (let* ((data (list i j))
				 (button
				  (q-tool-button -
				   (:set (:icon-size (q-new q-size *block-size* *block-size*)))
				   (:connect* (:clicked #'on-click-block (append data (list it)))))))
			   button)
			 j i 1 1)))))

(defun flip-button-icon-and-delete (data)
  (show-button-icon data)
  (let ((timer (q-new q-timer)))
    (set-interval timer 1000)
    (q-connect timer :timeout #'(lambda ()
				   (stop timer)
				   (set-visible (third data) nil)))
    (start timer)))

(defun flip-button-icon (data)
  (show-button-icon data)
  (let ((timer (q-new q-timer)))
    (set-interval timer 1000)
    (q-connect timer :timeout #'(lambda ()
				   (stop timer)
				   (hide-button-icon data)))
    (start timer)))

(defun show-button-icon (data)
  (set-icon (third data) (q-new q-icon (mkstr "./pic/" (aref *board* (first data) (second data)) ".png"))))

(defun hide-button-icon (data)
  (set-icon (third data) *empty-icon*))

(defun on-click-block (data)
  (cond ((null *current-flip*)
	 (setf *current-flip* data)
	 (show-button-icon data))
	((equal data *current-flip*))
	((eql (aref *board* (first data) (second data))
	      (aref *board* (first *current-flip*) (second *current-flip*)))
	 (flip-button-icon-and-delete data)
	 (flip-button-icon-and-delete *current-flip*)
	 (setf *current-flip* nil))
	(t
	 (flip-button-icon data)
	 (flip-button-icon *current-flip*)
	 (setf *current-flip* nil))))

(defun main ()
  (q-application)
  (build-data)
  (build-widgets)
  (show (gui window))
  (exec (q-application)))
