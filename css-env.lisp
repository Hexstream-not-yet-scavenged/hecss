(in-package #:hecss)

(defclass css-env ()
  ((palette :initarg :palette
	    :reader css-env-palette
	    :type palette)))
