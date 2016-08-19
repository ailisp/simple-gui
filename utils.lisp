(in-package #:top.myfyb.utils)

(defun mkstr (&rest args)
  "Make a string from ARGS by princ."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun group (source n)
  "Group the list SOURCE to a new list with element as new list contains n elements of SOURCE.
e.g. (group '(a b c d e) 2) => '((a b) (c d) (e))"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defmacro macq (expr)
  "Display the macroexpand-1 of quote EXPR."
  `(pprint (macroexpand-1 ',expr)))
  
(defmacro mac (expr)
  "Display the macroexpand-1 of EXPR."
  `(pprint (macroexpand-1 ,expr)))

(defun singlep (lst)
  "Return T if LST is a list contains only one element."
  (and (consp lst) (not (cdr lst))))

(defun join-str (seprator lst)
  "Join a list of string LST into one str, with seprator SEPRATOR."
  (cond ((null lst) "")
	((singlep lst) (string (car lst)))
	(t
	 (mkstr (car lst)
		seprator
		(join-str seprator (cdr lst))))))

(defun memoize (fn)
  "Return a memoized version of function FN."
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	      (setf (gethash args cache)
		    (apply fn args)))))))

(defmacro _f (op place &rest args)
  "Set the PLACE to the result of (OP PLACE ,@ARGS)."
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
	    (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro memof (fn-name)
  "Set the FN-NAME's function definition to its memoized version."
  `(_f memoize (fdefinition ',fn-name)))

(defun array->vector (array)
  "Return a 1D version of ARRAY"
  (copy-array (make-array (array-total-size array)
				     :element-type (array-element-type array)
				     :displaced-to array)))
