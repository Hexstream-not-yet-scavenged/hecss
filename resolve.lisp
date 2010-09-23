(in-package #:hecss)

(defclass css-resolver ()
  ((template-env :initarg :template-env
		 :reader template-env
		 :type template-env)
   (leaf-only :initarg :leaf-only
	      :reader leaf-only
	      :initform nil)
   (css-env :initarg :css-env
	    :reader css-env
	    :type css-env)))

(defmethod shared-initialize :after ((renderer css-resolver) slot-names
				     &key template-env)
  (check-type template-env template-env))

(defun resolve-list (resolver list)
  (mapcar (fmask #'css-resolve ? (resolver ?))
	  list))

(defun resolve-css (resolver element)
  (css-resolve resolver element))

(defgeneric css-resolve (resolver element))

(defmethod css-resolve ((resolver css-resolver) anything)
  anything)

(defmethod css-resolve ((resolver css-resolver) (rules list))
  (resolve-list resolver rules))

(defmethod css-resolve ((resolver css-resolver) (import css-import))
  (make-instance 'css-import
		 :target (css-resolve resolver (css-import-target import))
		 :media-types (resolve-list resolver (css-import-media-types import))))

(defmethod css-resolve ((resolver css-resolver) (rule css-rule))
  (make-instance 'css-rule
		 :selectors (resolve-list resolver (rule-selectors rule))
		 :declarations (resolve-list resolver (rule-declarations rule))))

(defmethod css-resolve ((resolver css-resolver) (sel css-simple-selector))
  (make-instance 'css-simple-selector
		 :type (css-resolve resolver (simple-selector-type sel))
		 :specifiers (resolve-list resolver (simple-selector-specifiers sel))))

(defmethod css-resolve ((resolver css-resolver) (sel css-id-selector))
  (format nil "#~A" (id-selector-id sel)))

(defmethod css-resolve ((resolver css-resolver) (sel css-class-selector))
  (format nil ".~A" (class-selector-name sel)))

(defmethod css-resolve ((resolver css-resolver) (sel css-pclass-selector))
  (format nil ":~A" (pclass-selector-name sel)))

(defmethod css-resolve ((resolver css-resolver) (sel css-attribute-selector))
  (let ((type (attribute-selector-type sel)))
    (if (eq type :set)
	sel				;necessarily fully resolved
	(make-instance 'css-attribute-selector
		       :type type
		       :attribute (attribute-selector-attribute sel)
		       :operand (css-resolve resolver (attribute-selector-operand sel))))))

(defmethod css-resolve ((resolver css-resolver) (sel css-compound-selector))
  (make-instance 'css-compound-selector
		 :relation (compound-selector-relation sel)
		 :left (css-resolve resolver (left-selector sel))
		 :right (css-resolve resolver (right-selector sel))))

(defmethod css-resolve ((resolver css-resolver) (dec css-declaration))
  (make-instance 'css-declaration
		 :property (declaration-property dec)
		 :value (css-resolve resolver (declaration-value dec))))

(defmethod css-resolve ((resolver css-resolver) (list css-list))
  (make-instance 'css-list
		 :type (css-list-type list)
		 :elements (resolve-list resolver (css-list-elements list))))

(defmethod css-resolve ((resolver css-resolver) (uri uri))
  (princ-to-string uri))

(defmethod css-resolve ((resolver css-resolver) (ref template-env-reference))
  (css-resolve resolver (resolve-template-env-reference ref (template-env resolver))))

(defmethod css-resolve ((resolver css-resolver) (ref color-reference))
  (css-resolve resolver (resolve-color-reference ref (css-env resolver))))
