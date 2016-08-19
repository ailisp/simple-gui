(in-package #:simple-gui)

(defvar *qt-modules* '(:qtcore
		       :qtgui
		       :qtopengl
		       :qtsvg
		       :qtwebkit)
  "Qt Modules to load and generate GUI description macros.")


;;;; Some meta utils

(defun all-qclasses ()
  "List all qt classes, each is represented by a number."
  (let ((all-qclasses nil))
    (qt::map-classes #'(lambda (qclass) (push qclass all-qclasses)))
    (nreverse all-qclasses)))
(memof all-qclasses)

(defun all-qsubclasses-of (qclass)
  "List all subclasses of given QCLASS. QCLASS is a number."
  (remove-if-not #'(lambda (qsubclass)
		     (qt::qsubclassp qsubclass qclass))
		 (all-qclasses)))
(memof all-qsubclasses-of)

(defun qclass-names (qclass-list)
  "All qt class names of QCLASS-LIST contains a list of numbers."
  (mapcar #'qt::qclass-name qclass-list))

(defun all-qmethods ()
  "List all qt methods, each is represented by a number."
  (let ((all-qmethods nil))
    (qt::map-methods #'(lambda (qmethod)
			 (unless (or (qt::qmethod-ctor-p qmethod)
				     (qt::qmethod-dtor-p qmethod)
				     (qt::qmethod-enum-p qmethod))
			   (push qmethod all-qmethods))))
    all-qmethods))
(memof all-qmethods)

(defun all-qenums ()
  "List all qt enums, each is represented by a number."
  (let ((all-qenums nil))
    (qt::map-methods #'(lambda (qmethod)
			 (if (qt::qmethod-enum-p qmethod)
			     (push qmethod all-qenums))))
    all-qenums))
(memof all-qenums)

(defvar *except-qmethod-names* 	'("drop_action" "setDrop_action" "default_action"
				  "setDefault_action" "set_widget" "_deviceType"
				  "set_deviceType" "_touchPointStates"
				  "set_touchPointStates" "_touchPoints"
				  "set_touchPoints" "_widget")
  "Excepted qt method names, which are actually not in qt doc but for internal use.")

(defun except-qmethod-name-p (qmethod-name)
  "Return T if QMETHOD-NAME is an except method name, otherwise return NIL"
  (find qmethod-name
	*except-qmethod-names*
	:test #'string=))

(defun all-qmethod-names ()
  "List all qt method names, duplicate name but different method are only count once."
  (let ((names (make-hash-table :test #'equal)))   
    (mapcar #'(lambda (qmethod)
		(let ((qmethod-name (qt::qmethod-name qmethod)))
		  (unless (except-qmethod-name-p qmethod-name)
		    (setf (gethash qmethod-name names)
			  t))))
	    (all-qmethods))
    (hash-table-keys names)))
(memof all-qmethod-names)

(defvar *except-enum-names*  '("Q_COMPLEX_TYPE" "Q_PRIMITIVE_TYPE" "Q_STATIC_TYPE" 
			       "Q_MOVABLE_TYPE" "Q_DUMMY_TYPE" "LicensedGui" "LicensedXml" 
			       "LicensedQt3SupportLight" "LicensedScript" "LicensedOpenVG" 
			       "LicensedDBus" "LicensedTest" "LicensedActiveQt" 
			       "LicensedScriptTools" "LicensedSvg" "LicensedDeclarative" 
			       "LicensedSql" "LicensedOpenGL" "LicensedCore" "QtDebugMsg" 
			       "QtWarningMsg" "QtCriticalMsg" "QtFatalMsg" "QtSystemMsg" 
			       "LicensedHelp" "LicensedMultimedia" "LicensedQt3Support" 
			       "LicensedXmlPatterns" "LicensedNetwork")
  "Except qt enum names, which are actually not implemented by CommonQt.")

(defun except-qenum-name-p (enum-class-and-name)
  "Return T if ENUM-CLASS-AND-NAME is an except qt enum name, otherwise return NIL."
  (and (string= (car enum-class-and-name) "QGlobalSpace")
       (find (cdr enum-class-and-name)
	     *except-enum-names*
	     :test #'string=)))

(defun all-qenum-names ()
  "List all qt enum names, each of name is in a form (QCLASS-NAME . QENUM-NAME)."
  (let ((names nil))
    (mapcar #'(lambda (qmethod)
		(let ((enum-class-and-name
		       (cons (qt::qclass-name (qt::qmethod-class qmethod))
			     (qt::qmethod-name qmethod))))
		  (unless (except-qenum-name-p enum-class-and-name)
		    (push enum-class-and-name names))))
	    (all-qenums))
    names))
(memof all-qenum-names)


;;;; Name conversions

(defun convert-to-qt-class-name (s)
  "Convert a string foo-bar to string FooBar"
  (let ((camel-case (convert-lisp-name-to-camel-case s)))
    (setf (aref camel-case 0) (char-upcase (aref camel-case 0)))
    camel-case))

(defun convert-to-lisp-name (s)
  "Convert a string QPushButton or qPush_button to string Q-PUSH-BUTTON."
  (let ((temp-name
	 (ppcre:regex-replace-all "--"
				  (ppcre:regex-replace-all
				   "_" (string-upcase
					(ppcre:regex-replace-all "[A-Z]" s "-\\\&")) "-")
				  "-")))
    (if (eql (aref temp-name 0) #\-)
	(subseq temp-name 1)
	temp-name)))

(defvar *convert-special-cases*
  '(("OPERATOR--" . "OPERATOR--")				  
    ("APPEND" . "QAPPEND") ("SUBSTITUTE" . "QSUBSTITUTE") ("SPACE" . "QSPACE")
    ("FIND" . "QFIND") ("POSITION" . "QPOSITION") ("LAST" . "QLAST")
    ("READLINE" . "QREAD-LINE") ("NUMBER" . "QNUMBER") ("WARNING" . "QWARNING")
    ("OPEN" . "QOPEN") ("OPTIMIZE" . "QOPTIMIZE") ("STRING" . "QSTRING")
    ("CONJUGATE" . "QCONJUGATE") ("LOAD" . "QLOAD") ("TYPE" . "QTYPE")
    ("WRITE" . "QWRITE") ("READ" . "QREAD") ("SECOND" . "QSECOND")
    ("CLASSNAME" . "Q-CLASS-NAME") ("CHECKTYPE" . "QCHECK-TYPE")
    ("FIRST" . "QFIRST") ("CLOSE" . "QCLOSE") ("MERGE" . "QMERGE")
    ("ABORT" . "QABORT") ("SEQUENCE" . "QSEQUENCE") ("SPEED" . "QSPEED")
    ("IGNORE" . "QIGNORE") ("REPLACE" . "QREPLACE") ("METHOD" . "Q-METHOD")
    ("SORT" . "QSORT") ("ERROR" . "QERROR") ("REMOVE" . "QREMOVE")
    ("SIGNAL" . "Q-SIGNAL") ("PUSH" . "QPUSH") ("MAP" . "QMAP")
    ("RESTART" . "QRESTART") ("DIRECTORY" . "QDIRECTORY") ("FILL" . "QFILL")
    ("T" . "Q-T") ("LOG" . "QLOG") ("PRINT" . "QPRINT") ("FORMAT" . "QFORMAT")
    ("LENGTH" . "QLENGTH") ("TIME" . "QTIME") ("BLOCK" . "QBLOCK")
    ("TRUNCATE" . "QTRUNCATE") ("VECTOR" . "QVECTOR") ("COUNT" . "QCOUNT")
    ("SHADOW" . "QSHADOW") ("PROFILE" . "QPROFILE") ("RESET" . "QRESET")
    ("TIMEOUT" . "QTIMEOUT") ("ExIT" . "QEXIT") ("QUIT" . "QQuit")
    ("DEREF" . "QDEREF") ("CAST" . "QCAST"))
  "Special cases where name conversions to lisp is manually specified.")

(defun convert-qt-method-name-to-lisp-name (s)
  "Do the conversion by CONVERT-TO-LISP-NAME, but with some special-case."
  (let ((special (assoc s *convert-special-cases* :test #'string-equal)))
    (if special
	(cdr special)
	(convert-to-lisp-name s))))

(defun convert-to-qt-method-name (s)
  "Convert a string foo-bar to string fooBar"
  (convert-lisp-name-to-camel-case s))

(defun convert-lisp-name-to-camel-case (s)
  "Convert a string designator foo-bar to string fooBar"
  (let ((s (string-downcase (string s))))
    (do ((i 0 (1+ i)))
	((>= i (length s)))
      (when (eql (aref s i) #\-)
	(incf i)
	(when (< i (length s))
	  (setf (aref s i) (char-upcase (aref s i))))))
    (delete #\- s)))

(defun convert-to-cpp-basic-type (s)
  "Convert a symbol or string S to the string indicating a C++ basic type."
  (string-downcase (string s)))

(defun convert-to-cpp-basic-type-or-qt-class (s)
  "Convert a symbol or string s to the string indicating a C++ basic type or a qt class."
  (if (find #\- (string s))
      (convert-to-qt-class-name s)
      (convert-to-cpp-basic-type s)))

(defun convert-to-qt-signal-name (sig &rest args)
  "Convert a list like (:clicked :int :q-push-button) to corresponding qt signal name."
  (mkstr (convert-to-qt-method-name sig)
	 "("
	 (join-str "," (mapcar #'convert-to-cpp-basic-type-or-qt-class args))
	 ")"))


;;;; Translation for simple descriptive GUI definition.

(defun expand-qt-method-call (method-name &rest args)
  "Expand to a CommonQt method call."
  `(optimized-call t ,(car args) ,method-name ,@(cdr args)))

(defun translate-set (obj prop)
  "Translate a set specification to a qt setter method."
  (let ((setter (car prop))
	(vals (cdr prop)))
    (apply #'expand-qt-method-call
	   (convert-to-qt-method-name (mkstr "set-" setter))
	   obj
	   vals)))

(defun translate-add (obj to-add)
  "Translate an add specification to the q-add wrapper."
  `(q-add ,obj ,to-add))

(defun q-add (parent child)
  "A wrapper that will determine to call addWidget or addLayout depends on type of child."
  (let ((child-class (slot-value child 'class)))
    (cond ((qt::qsubclassp child-class (qt::find-qclass "QWidget"))
	   (optimized-call t parent "addWidget" child))
	  ((qt::qsubclassp child-class (qt::find-qclass "QLayout"))
	   (optimized-call t parent "addLayout" child))
	  (t (error "Illegal object ~a to add." child)))))

(defun translate-connect (sender sig-and-fn)
  "Translate a qt signal connect specification."
  (let ((sig (butlast sig-and-fn))
	(fn (lastcar sig-and-fn)))
    (expand-qt-connect-signal sender sig fn)))

(defun translate-connect* (sender sig-fn-data)
  "Translate a more detailed connect specification, which can pass an additional
data argument to the connected function."
  (let* ((sig-fn (butlast sig-fn-data))
	 (sig (butlast sig-fn))
	 (fn (lastcar sig-fn))
	 (data (lastcar sig-fn-data))
	 (gargs (gensym)))
    (expand-qt-connect-signal sender sig `#'(lambda (&rest ,gargs)
					      (apply ,fn ,data ,gargs)))))

(defun expand-qt-connect-signal (sender sig fn)
  "Common parts for expand connect and connect* specification."
  `(connect ,sender ,(apply #'convert-to-qt-signal-name sig) ,fn))

(defun translate-spec (spec-opr obj spec)
  "Translate a specification according to the SPEC-OPR."
  (ecase spec-opr
    (:add (translate-add obj spec))
    (:set (translate-set obj spec))
    ((:con :connect) (translate-connect obj spec))
    ((:con* :connect*) (translate-connect* obj spec))))

(defun translate-specs (name specs)
  "Translate a group of specifications of same type."
  (if (keywordp (car specs))
      (mapcar #'(lambda (spec)
		  (translate-spec (car specs)
				  name
				  spec))
	      (cdr specs))
      (mapcar #'(lambda (spec)
		  (translate-set name spec))
	      specs)))

(defun translate-body (name body)
  "Translate a GUI element body, contains groups of specifications."
  (mapcan #'(lambda (specs)
	      (translate-specs name specs)) body))


;;;; Generate macros for GUI element description.

(defmacro generate-all-qt-class-macros ()
  "Generate all macros for GUI element description."
  `(progn
     ,@(mapcar #'expand-qt-class-macro
	       (qclass-names
		(remove-duplicates 
		 (append
		  (all-qsubclasses-of (qt::find-qclass "QWidget"))
		  (all-qsubclasses-of (qt::find-qclass "QLayout"))))))))

(defun expand-qt-class-macro (qclass-name)
  "Gernerate a single GUI element description."
  (let* ((qclass-lisp-name (convert-to-lisp-name qclass-name))
	 (qclass-symbol (intern qclass-lisp-name :simple-gui)))
    `(progn
       (export ',qclass-symbol :simple-gui)
       (defmacro ,qclass-symbol (name &body body)
	 (if (or (null name) (string= (symbol-name name)
				      "-"))
	     (setf name (gensym)))
	 `(let* ((,name (optimized-new ,,qclass-name))
		 (it ,name))
	    (declare (ignorable it))
	    (setf (gethash ',name *q-objects*) ,name)
	    ,@ (translate-body name body)
	       ,name)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun load-qt-stage1 ()
    "Load qt modules and switch to CommonQt's readtable."
    (named-readtables:in-readtable :qt)
    (mapcar #'ensure-smoke *qt-modules*)))
(load-qt-stage1)

(defmacro gui (name)
  "Macro for access gui elements."
  `(gethash ',name *q-objects*))


;;;; Generate all qt methods in a lisp like way to reprensent method names.

(defvar *qt-method-package* (or (find-package "Q-")
				(make-package "Q-" :use ()))
  "The package that will contains all qt methods (including enums).")

(defmacro generate-all-qt-methods ()
  "Generate all qt methods, each different method name converts to only one macro defination,
which method will be invoked is determined by smoke, not in cl."
  `(progn
     ,@(mapcar #'expand-qt-method-macro
	       (all-qmethod-names))))

(defmacro generate-all-qt-enums ()
  "Generate all qt enums."
  `(progn
     ,@ (mapcar #'expand-qt-enum-macro
		(all-qenum-names))))

(defun expand-qt-method-macro (qmethod-name)
  "Generate a macro for the call of QMETHOD-NAME."
  (let* ((qmethod-symbol-name (convert-qt-method-name-to-lisp-name qmethod-name))
	 (qmethod-symbol (intern qmethod-symbol-name *qt-method-package*)))
    `(progn
       (export ',qmethod-symbol ,(package-name (symbol-package qmethod-symbol)))
       (defmacro ,qmethod-symbol (&rest args)
	 `(let ((class-or-obj ,(car args)))
	    (optimized-call t
			    (if (symbolp class-or-obj)
				(convert-to-qt-class-name class-or-obj)
				class-or-obj)
			    ,,qmethod-name ,@(cdr args)))))))

(defun expand-qt-enum-macro (enum-class-and-name)
  "Generate a symbol macro for the enum in the format Q-CLASS/Q-ENUM."
  (let* ((qenum-symbol-name
	  (mkstr
	   (convert-to-lisp-name (car enum-class-and-name))
	   "/"
	   (convert-qt-method-name-to-lisp-name (cdr enum-class-and-name))))
	 (qenum-symbol (intern qenum-symbol-name *qt-method-package*)))
    `(progn
       (export ',qenum-symbol ,(package-name (symbol-package qenum-symbol)))
       (define-symbol-macro ,qenum-symbol 
	   (optimized-call t ,(car enum-class-and-name)
			    ,(cdr enum-class-and-name))))))

(defmacro q-new (qclass &rest args)
  "Make new qt non GUI element, maybe useful sometimes."
  `(optimized-new ,(convert-to-qt-class-name qclass) ,@args))

(defmacro q-connect (obj &rest spec)
  (translate-connect obj spec))

(defmacro q-connect* (obj &rest spec)
  (translate-connect* obj spec))
