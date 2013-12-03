;;;; package.lisp --- Package definition for parser.ini system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.ini
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions)

  (:import-from #:esrap
   #+esrap.grammar-class #:defgrammar
   #+esrap.grammar-class #:in-grammar
   #:defrule #:&bounds
   #:?
   #:text)

  ;; Conditions
  (:export
   #:ini-parse-error

   #:ini-parse-error-source
   #:ini-parse-error-location)

  ;; Variables
  (:export
   #:*builder*

   #:*name-component-separator*
   #:*assignment-operator*
   #:*value-terminating-whitespace-expression*)

  ;; Parser protocol
  (:export
   #:parse)

  ;; Builder protocol
  (:export
   #:make-node
   #:add-child)

  (:documentation
   "This package provides the main entry point

    parse SOURCE BUILDER                [generic function]

      Parse the content of SOURCE as \"ini-like\" configuration
      options, construct a parse result using BUILDER and return it.

    The builder protocol consists of

    make-node BUILDER KIND &rest ARGS   [generic function]

      Create objects representing sections and options.

    add-child BUILDER PARENT CHILD      [generic function]

      Attach options to their containing sections.

    Parsing may signal

    ini-parse-error                     [condition]

    Syntactic variants can be controlled by binding the special
    variables

    *assignment-expression*             [special variable]

      Controls assignment expression. Defaults to \"=\".

    *value-terminating-whitespace-expression* [special variable]

      Controls which whitespace terminates option values. By default,
      all whitespace terminates option values."))
