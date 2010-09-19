(in-package #:hecss)


(defclass color-reference ()
  ((name :initarg :name
	 :reader color-reference-name)))

(define-type-predicate color-reference)

(defun resolve-color-reference (color-reference css-env)
  (find-palette-mapping (css-env-palette css-env) (color-reference-name color-reference)))