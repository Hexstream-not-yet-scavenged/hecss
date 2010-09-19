(in-package #:hecss)


(deftype css-selector ()
  `(or css-simple-selector css-compound-selector css-selector-specifier))

(deftype css-selector-specifier ()
  `(or css-id-selector css-class-selector css-pclass-selector css-attribute-selector))

(defclass css-simple-selector ()
  ((type :initarg :type
	 :reader simple-selector-type
	 :type string
	 :documentation "A string naming an element type or the universal selector *.")
   (specifiers :initarg :specifiers
	       :reader simple-selector-specifiers
	       :type list
	       :documentation #.(format nil "A list of attribute selectors, ID selectors, 
                                             classes and pseudo-classes."))))

(defmethod shared-initialize :before ((sel css-simple-selector) slot-names
				      &key type specifiers)
  (check-type type string)
  (check-type specifiers list))

(defmethod print-object ((sel css-simple-selector) stream)
  (pprint-logical-block (stream nil)
    (let ((type (simple-selector-type sel))
	  (specifiers (simple-selector-specifiers sel)))
      (if (or (string/= type "*")
	      (not specifiers))
	  (write-string type stream))
      (if specifiers
	  (format stream "~_~{ ~_~A~}"
		  specifiers)))))


(defclass css-id-selector ()
  ((id :initarg :id
       :reader id-selector-id
       :type string
       :documentation "A string naming the target ID.")))

(defmethod shared-initialize :before ((sel css-id-selector) slot-names &key id)
  (check-type id string))

(defmethod print-object ((sel css-id-selector) stream)
  (pprint-logical-block (stream nil)
    (format stream "#~A" (id-selector-id sel))))


(defclass css-class-selector ()
  ((name :initarg :name
	 :reader class-selector-name
	 :type string
	 :documentation "A string naming the target class.")))

(defmethod shared-initialize :before ((sel css-class-selector) slot-names &key name)
  (check-type name string))

(defmethod print-object ((sel css-class-selector) stream)
  (pprint-logical-block (stream nil)
    (format stream ".~A" (class-selector-name sel))))


(defclass css-pclass-selector ()
  ((name :initarg :name
	 :reader pclass-selector-name
	 :type string
	 :documentation "A string naming the target pseudo-class.")))

(defmethod shared-initialize :before ((sel css-pclass-selector) slot-names &key name)
  (check-type name string))

(defmethod print-object ((sel css-pclass-selector) stream)
  (pprint-logical-block (stream nil)
    (format stream ":~A" (pclass-selector-name sel))))


(defclass css-attribute-selector ()
  ((type :initarg :type
	 :reader attribute-selector-type
	 :type (member :set :exact :prefix :any))
   (attribute :initarg :attribute
	      :reader attribute-selector-attribute
	      :type string
	      :documentation #.(format nil "A string naming the attribute whose value ~
                                            will be compared to operand according to type"))
   (operand :initarg :operand
	    :reader attribute-selector-operand
	    :initform nil
	    :documentation "The value that we're looking for (ignored if type is :set).")))

(defmethod shared-initialize :before ((sel css-attribute-selector) slot-names
				      &key type attribute)
  (check-type type (member :set :exact :prefix :any))
  (check-type attribute string))

(defmethod print-object ((sel css-attribute-selector) stream)
  (print-unreadable-object (sel stream :type t)
    (with-readers ((type attribute-selector-type)
		   (attribute attribute-selector-attribute)
		   (operand attribute-selector-operand))
	sel
      (format stream "~A ~A ~A" type attribute operand))))

(defmethod attribute-selector-type-operator ((sel css-attribute-selector))
  (attribute-selector-type-operator (attribute-selector-type sel)))

(defmethod attribute-selector-type-operator ((type symbol))
  (ecase type
    (:set "")
    (:exact "=")
    (:prefix "|=")
    (:any "~=")))


(defclass css-compound-selector ()
  ((relation :initarg :relation
	     :reader compound-selector-relation
	     :type (member :descendant :child :adjacent))
   (left :initarg :left
	 :reader left-selector
	 :documentation "A simple or compound selector.")
   (right :initarg :right
	  :reader right-selector
	  :documentation "A simple or compound selector.")))

(defmethod shared-initialize :before ((sel css-compound-selector) slot-names
				      &key relation)
  (check-type relation (member :descendant :child :adjacent)))

(defun compound-selector-relation-string (relation)
  (ecase relation
    (:descendant ">>")
    (:child ">")
    (:adjacent "+")))

(defun compound-selector-traditional-relation-string (relation)
  (ecase relation
    (:descendant " ")
    (:child " > ")
    (:adjacent " + ")))


(defmethod print-object ((sel css-compound-selector) stream)
  (pprint-logical-block (stream nil)
    (format stream "(~A ~:I~A ~_~A)"
	    (compound-selector-relation-string (compound-selector-relation sel))
	    (left-selector sel)
	    (right-selector sel))))