(asdf:defsystem #:hecss

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :depends-on (#:com.hexstreamsoft.lib
	       #:com.hexstreamsoft.lib.color
	       #:puri
	       #:com.hexstreamsoft.lib.shared-html-css)
  :serial cl:t
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
