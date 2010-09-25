(in-package #:cl-user)

(defpackage #:hecss
  (:use #:cl
	#:iterate
	#:com.hexstreamsoft.lib
	#:com.hexstreamsoft.lib.color
	#:puri
	#:com.hexstreamsoft.lib.shared-html-css)
  (:export #:template-env
	   #:template-env-reference
	   #:template-env-reference-key
	   #:resolve-template-env-reference

	   #:collect

	   #:css-list
	   #:css-list-type
	   #:css-list-elements

	   #:color-reference
	   #:color-reference-name
	   #:color-reference-p
	   #:resolve-color-reference

	   #:css-simple-selector
	   #:simple-selector-type
	   #:simple-selector-specifiers

	   #:css-id-selector
	   #:id-selector-id

	   #:css-class-selector
	   #:class-selector-name

	   #:css-pclass-selector
	   #:pclass-selector-name

	   #:css-attribute-selector
	   #:attribute-selector-type
	   #:attribute-selector-attribute
	   #:attribute-selector-operand
	   #:attribute-selector-type-operator

	   #:css-compound-selector
	   #:compound-selector-relation
	   #:left-selector
	   #:right-selector
	   #:compound-selector-relation-string
	   #:compound-selector-traditional-relation-string

	   #:css-declaration
	   #:declaration-property
	   #:declaration-value

	   #:css-import
	   #:css-import-target
	   #:css-import-media-types

	   #:css-rule
	   #:rule-selectors
	   #:rule-declarations
	   
	   #:css-env
	   #:css-env-palette

	   #:css-noprocess
	   #:with-css-identifiers
	   #:build-css
	   #:import
	   #:or
	   #:>>
	   #:>
	   #:+
	   #:multi
	   #:id
	   #:class
	   #:pclass
	   #:attribute
	   #:s
	   #:space
	   #:c
	   #:comma
	   #:p
	   #:percent
	   #:color
	   
	   #:css-scale
	   #:define-css-level

	   #:css-resolver
	   #:resolve-css
	   #:css-resolve

	   #:css-optimizer
	   #:optimize-css
	   #:css-optimize

	   #:css-renderer
	   #:render-css
	   #:css-render

	   #:css-compiler
	   #:compile-css
	   #:css-compile))
