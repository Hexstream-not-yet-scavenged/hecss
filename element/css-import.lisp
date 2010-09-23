(in-package #:hecss)

(defclass css-import ()
  ((target :initarg :target
	   :reader css-import-target)
   (media-types :initarg :media-types
		:reader css-import-media-types
		:type list
		:initform nil)))

(defmethod shared-initialize :before ((import css-import) slot-names &key media-types)
  (check-list-element-type media-types list))
