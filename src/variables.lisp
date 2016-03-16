;;;; variables.lisp --- Variables used by the parser.ini system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.ini)

;;; Syntax variation selectors

(defvar *name-component-separator* #\.
  "Controls the syntax for separating name components. The default
   is the character \".\".

   Note the value nil corresponds to \"no component separator\" which
   leads to names not being split into components.")

(defvar *assignment-operator* #\=
  "Controls the accepted assignment syntax. The default is the
   character \"=\".")

(defvar *value-terminating-whitespace-expression* :all
  "Controls which kinds of whitespace terminate option values. The
   default is :all which corresponds to any whitespace terminating the
   value of an option.

   For some values of this variable, quoting has to be used when
   whitespace in option values is required.")
