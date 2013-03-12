;;;; list-builder.lisp --- Construct list-based parse results.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.ini)

(defmethod make-node ((builder (eql 'list)) (kind t) &rest args &key)
  (list* kind '() args))

(defmethod add-child ((builder (eql 'list)) (parent list) (child t))
  (appendf (second parent) (list child))
  parent)
