(in-package #:hecss)


(defclass css-declaration ()
  ((property :initarg :property
	     :reader declaration-property
	     :type string)
   (value :initarg :value
	  :reader declaration-value)))

(defmethod shared-initialize :before ((dec css-declaration) slot-names &key property)
  (check-type property string))

(defmethod print-object ((d css-declaration) stream)
  (pprint-logical-block (stream nil)
    (format stream ":~A ~A"
	    (declaration-property d)
	    (declaration-value d))))