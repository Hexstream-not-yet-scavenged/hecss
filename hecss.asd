;;;; -*- mode: lisp -*-

(in-package #:cl-user)
(defpackage #:hecss.system
  (:use :cl :asdf))
(in-package #:hecss.system)


(defsystem hecss
  :author "Hexstream"
  :depends-on (com.hexstreamsoft.lib
	       com.hexstreamsoft.lib.color
	       puri
	       com.hexstreamsoft.lib.shared-html-css)
  :serial t
  :components ((:file "package")
	       (:file "css-env")
	       (:module "element" :serial t :components
			((:file "list")
			 (:file "color")
			 (:file "selector")
			 (:file "declaration")
			 (:file "css-import")
			 (:file "rule")))
	       (:file "build")
	       (:file "resolve")
	       (:file "optimize")
	       (:file "render")
	       (:file "compile")))
