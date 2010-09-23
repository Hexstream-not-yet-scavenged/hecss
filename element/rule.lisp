(in-package #:hecss)

(deftype css-toplevel ()
  `(or css-rule css-import))

(defclass css-rule ()
  ((selectors :initarg :selectors
	      :reader rule-selectors
	      :type list
	      :documentation "A list of selectors that describe the set of elements the declarations will apply to.")
   (declarations :initarg :declarations
		 :reader rule-declarations
		 :type list
		 :documentation "The declarations that take effect for the selected elements.")))

(defmethod shared-initialize :before ((rule css-rule) slot-names &key selectors declarations)
  (check-type selectors list)
  (check-type declarations list)
  (if (null selectors)
      (error "A css rule must have at least one selector!")))

(defun pprint-mandatory (stream list &optional (colon-p t) at-sign-p)
  (declare (ignore at-sign-p))
  (let ((*print-right-margin* 0))
    (pprint-linear stream list colon-p)))

(defmethod print-object ((rule css-rule) stream)
  (pprint-logical-block (stream nil)
    (format stream "(~:I~W ~:@_"
	    (rule-selectors rule))
    (pprint-mandatory stream (rule-declarations rule))))
