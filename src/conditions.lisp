;;;; conditions.lisp --- Conditions used by the parser.ini system.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.ini)

(define-condition ini-parse-error (parse-error
                                   chainable-condition)
  ((source   :initarg  :source
             :reader   ini-parse-error-source
             :documentation
             "Stores the source string in which the parse error
              occurred.")
   (location :initarg  :location
             :type     (or null cons)
             :reader   ini-parse-error-location
             :initform nil
             :documentation
             "Stores the location at which the parse error
              occurred. The format is

                (START . END)

              where END can be nil"))
  (:default-initargs
   :source (missing-required-initarg 'ini-parse-error :source))
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not parse ~D of ~
                     ~S~/more-conditions:maybe-print-cause/~@:>"
             (ini-parse-error-location condition)
             (ini-parse-error-source condition)
             condition)))
  (:documentation
   "This error is signaled when parsing ini input fails."))
