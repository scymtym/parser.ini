;;;; package.lisp --- Package definition for unit tests of the parser.ini system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.ini.test
  (:use
   #:cl
   #:let-plus

   #:lift

   #:parser.ini)

  (:export
   #:parser.ini.root)

  (:documentation
   "This package contains unit tests of the parser.ini system."))

(cl:in-package #:parser.ini.test)

(deftestsuite parser.ini.root ()
  ()
  (:documentation
   "Root unit test suite for the parser.ini system."))
