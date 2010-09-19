(in-package #:hecss)

(defmacro with-css-identifiers ((&rest identifiers) &body body)
  `(let (,@(loop for identifier in identifiers
		 collect `(,identifier ,(string-downcase (symbol-name identifier)))))
    (declare (ignorable ,@identifiers))
    ,@body))

(defun form-with-operator-p (form operator)
  (and (consp form)
       (eq (first form) operator)))

(deftype simple-css-value ()
  `(or string real))

(defun simple-css-value (value)
  (etypecase value
    (string value)
    (real (if (zerop value) "0" (format nil "~Aem" value)))
    ((satisfies keywordp) (string-downcase (symbol-name value)))))

(defmacro build-css (&body rules)
  `(list ,@(mapcan #'css-build-toplevel rules)))

(defun expand-rule (rule)
  (let* ((selectors (first rule))
	 (nested (loop for nested in (cddr rule)
		       nconc (expand-rule (destructuring-bind ((operator &rest selector-rest)
							       &rest rule-rest)
					      nested
					    `((,operator ,selectors ,@selector-rest)
					      ,@rule-rest))))))
    (if (second rule)
	(cons (list (first rule)
		    (second rule))
	      nested)
	nested)))

(defun css-build-toplevel (form)
  (if (form-with-operator-p form 'import)
      (list `(make-instance 'css-import
	      :target ,(second form) :media-types (list ,@(cddr form))))
      (mapcar #'css-build-rule (expand-rule form))))

(defmacro mapvalues (function values &rest more-values)
  `(mapcar ,function (multiple-value-call #'list ,values ,@more-values)))

(defun css-build-rule (rule)
  (let ((selectors (bubble-expand 'or (first rule)))
	(declarations (second rule)))
    `(make-instance 'css-rule
      :selectors (list ,@(if (form-with-operator-p selectors 'or)
			     (mapcar #'css-build-selector (cdr selectors))
			     (list (css-build-selector selectors))))
      :declarations (list ,@(loop for (property value) on declarations by #'cddr
				  nconc (css-build-declaration property value))))))

(defun css-build-selector (selector)
  (or (and (consp selector)
	   (labels ((red (relation form)
		      (reduce #'(lambda (left right)
				  `(make-instance 'css-compound-selector
				    :relation ,relation
				    :left ,left
				    :right ,(css-build-selector right)))
			      (cddr form) :initial-value (css-build-selector (second form)))))
	     (case (first selector)
	       (>> (red :descendant selector))
	       (> (red :child selector))
	       (+ (red :adjacent selector)))))
      (css-build-simple-selector selector)))

(defun css-build-simple-selector (selector)
  (or (and (consp selector)
	   (case (first selector)
	     (multi (let* ((second (second selector))
			   (explicit-type-p (stringp second)))
		      `(make-instance 'css-simple-selector
			:type ,(if explicit-type-p second "*")
			:specifiers (list ,@(mapcar #'css-build-simple-selector
						    (if explicit-type-p
							(cddr selector)
							(cdr selector)))))))
	     (id `(make-instance 'css-id-selector :id (simple-css-value ,(second selector))))
	     (class `(make-instance 'css-class-selector
		      :name (simple-css-value ,(second selector))))
	     (pclass `(make-instance 'css-pclass-selector
		       :name (simple-css-value ,(second selector))))
	     (attribute `(make-instance 'css-attribute-selector
			  :type ,(second selector)
			  :attribute (simple-css-value ,(third selector))
			  :operand (simple-css-value ,(fourth selector))))))
      `(make-instance 'css-simple-selector :type ,selector :specifiers '())))

(defvar *replicators* nil)

(defmacro define-property-replicator (property (&rest replications))
  (once-only (property)
    (with-unique-names (entry)
      `(let ((,entry (assoc ,property *replicators*)))
	(if ,entry
	    (setf (cdr ,entry) (list ,@replications))
	    (push (cons ,property (list ,@replications)) *replicators*))
	,property))))

(define-property-replicator :padding-lr (:padding-left :padding-right))
(define-property-replicator :padding-tb (:padding-top :padding-bottom))
(define-property-replicator :border-lr (:border-left :border-right))
(define-property-replicator :border-tb (:border-top :border-bottom))
(define-property-replicator :margin-lr (:margin-left :margin-right))
(define-property-replicator :margin-tb (:margin-top :margin-bottom))

#+nil(let ((values (if (and (consp value)
			    (eql (first value) 's))
		       (cdr value)
		       (list value))))
       (append values
	       (make-list (let ((autocomplete (- (length property-expansion)
						 (length values))))
			    (if (< autocomplete 0)
				(error "css-build-declaration property-expansion autocomplete failed because there are too many values")
				autocomplete))
			  :initial-element (car (last values)))))

(defun css-build-declaration (property value)
  (let ((property-expansion (cdr (assoc property *replicators*))))
    (if property-expansion
	(mapcan (fmask #'css-build-declaration ? (? value))
		property-expansion)
	(list `(make-instance 'css-declaration
		:property (string-downcase
			   (string ,property))
		:value ,(css-build-declaration-value
			 value))))))

(defun css-build-declaration-value (value)
  (or (and (consp value)
	   (case (first value)
	     ((s space)
	      `(make-instance 'css-list
		:type :space
		:elements (list ,@(mapcar #'css-build-declaration-value (cdr value)))))
	     ((c comma)
	      `(make-instance 'css-list
		:type :comma
		:elements (list ,@(mapcar #'css-build-declaration-value (cdr value)))))
	     ((p percent)
	      (with-unique-names (operand)
		`(let ((,operand ,(second value)))
		  (check-type ,operand real)
		  (format nil "~A%" ,operand))))
	     (color
	      `(make-instance 'color-reference :name ,(second value)))))
      `(simple-css-value ,value)))