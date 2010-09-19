(in-package #:hecss)

(defclass css-list ()
  ((type :initarg :type
	 :reader css-list-type
	 :type (member :comma :space))
   (elements :initarg :elements
	     :reader css-list-elements
	     :type list)))

(defmethod shared-initialize :before ((list css-list) slot-names &key type elements)
  (check-type type (member :comma :space))
  (check-type elements list))