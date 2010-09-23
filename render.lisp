(in-package #:hecss)

(defclass css-renderer ()
  ((template-env :initarg :template-env
		 :reader template-env
		 :type template-env)))

(defmethod shared-initialize :after ((renderer css-renderer) slot-names
				     &key template-env)
  (check-type template-env template-env))

(defun render-css (renderer element stream)
  (css-render renderer element stream))

(defgeneric css-render (renderer element stream))

(defmethod css-render ((renderer css-renderer) (function function) stream)
  (funcall function renderer stream))

(defmethod css-render ((renderer css-renderer) (rules list) stream)
  (mapc (fmask #'css-render ? (renderer ? stream))
	rules))

(defmethod css-render ((renderer css-renderer) (import css-import) stream)
  (write-string "@import \"" stream)
  (css-render renderer (css-import-target import) stream)
  (write-char #\" stream)
  (let ((medias (css-import-media-types import)))
    (when medias
      (princ " " stream)
      (css-render renderer (car medias) stream)
      (dolist (media (cdr medias))
	(write-string ", " stream)
	(css-render renderer media stream)))))

(defmethod css-render ((renderer css-renderer) (rule css-rule) stream)
  (let ((selectors (rule-selectors rule))
	(declarations (rule-declarations rule)))
    (css-render renderer (first selectors) stream)
    (dolist (selector (cdr selectors))
      (princ ", " stream)
      (css-render renderer selector stream))
    (princ "  {" stream)
    (dolist (declaration declarations)
      (format stream "~%    ")
      (css-render renderer declaration stream))
    (format stream "~%}")))

(defmethod css-render ((renderer css-renderer) (sel css-simple-selector) stream)
  (let ((type (simple-selector-type sel))
	(specifiers (simple-selector-specifiers sel)))
    (if (or (string/= type "*")
	    (not specifiers))
	(write-string type stream))
    (mapc (fmask #'css-render ? (renderer ? stream))
	  specifiers)))

(defmethod css-render ((renderer css-renderer) (sel css-id-selector) stream)
  (format stream "#~A" (id-selector-id sel)))

(defmethod css-render ((renderer css-renderer) (sel css-class-selector) stream)
  (format stream ".~A" (class-selector-name sel)))

(defmethod css-render ((renderer css-renderer) (sel css-pclass-selector) stream)
  (format stream ":~A" (pclass-selector-name sel)))

(defmethod css-render ((renderer css-renderer) (sel css-attribute-selector) stream)
  (with-readers ((attribute attribute-selector-attribute)
		 (type attribute-selector-type)
		 (operand attribute-selector-operand))
      sel
    (format stream "[~A~A\"" attribute (attribute-selector-type-operator type))
    (unless (eq type :set)
      (css-render renderer operand stream))
    (princ "\"]" stream)))

(defmethod css-render ((renderer css-renderer) (sel css-compound-selector) stream)
  (css-render renderer (left-selector sel) stream)
  (princ (compound-selector-traditional-relation-string
	  (compound-selector-relation sel))
	 stream)
  (css-render renderer (right-selector sel) stream))

(defmethod css-render ((renderer css-renderer) (dec css-declaration) stream)
  (format stream "~A: " (declaration-property dec))
  (css-render renderer (declaration-value dec) stream)
  (princ ";" stream))

(defmethod css-render ((renderer css-renderer) (css-list css-list) stream)
  (let ((list (css-list-elements css-list))
	(separator (ecase (css-list-type css-list)
		     (:space " ")
		     (:comma ", "))))
    (when list
      (css-render renderer (car list) stream)
      (dolist (element (cdr list))
	(princ separator stream)
	(css-render renderer element stream)))))

(defmethod css-render ((renderer css-renderer) (string string) stream)
  (princ string stream))

(defmethod css-render ((renderer css-renderer) (nothing null) stream)
  (declare (ignore renderer nothing stream)))

(defmethod css-render ((renderer css-renderer) (uri uri) stream)
  (princ uri stream))

#+nil(defmethod css-renderer ((renderer css-renderer) (ref template-env-reference) stream)
       (css-render renderer (resolve-template-env-reference ref (template-env renderer)) stream))
