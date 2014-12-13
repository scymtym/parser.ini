;;;; protocol.lisp --- Protocol provided by the parser.ini system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.ini)

;;; Parser protocol

(defgeneric parse (source builder)
  (:documentation
   "Parse the content of SOURCE as \"ini-like\" configuration options,
    construct a parse result using BUILDER and return it.

    Signal a `ini-parse-error' when errors are encountered."))

;; Default behavior

(define-condition-translating-method parse ((source t) (builder t))
  ;; When an `esrap-error' is signaled, the innermost `parse' method
  ;; is specialized on `string'. It is therefore OK to assign to
  ;; SOURCE to the source slot of the condition.
  ((esrap:esrap-error ini-parse-error
    :var condition)
   :source   source
   :location (when (esrap:esrap-error-position condition)
               (esrap:esrap-error-position condition)))
  ((error ini-parse-error)
   :source source))

(defmethod parse ((source t) (builder t))
  (error "~@<Cannot parse source ~A with builder ~A.~@:>"
         source builder))

(defmethod parse ((source string) (builder t))
  (let ((*builder* builder))
    (esrap:parse 'ini source
                 #+esrap.grammar-class :grammar
                 #+esrap.grammar-class '#:parser.ini)))

(defmethod parse ((source stream) (builder t))
  (parse (read-stream-content-into-string source) builder))

(defmethod parse ((source pathname) (builder t))
  (parse (read-file-into-string source) builder))
