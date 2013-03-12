;;;; grammar.lisp --- Grammar definition of the parser.ini system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.ini)

(defrule whitespace
    (+ (or #\Space #\Tab #\Newline))
  (:constant nil))

(defrule comment
    (and #\# (* (not #\Newline)))
  (:constant nil))

;; Quotes

(defrule escaped
    (and #\\ (or #\" #\\))
  (:function second))

(defrule quoted
    (and #\" (* (or escaped (not #\"))) #\")
  (:function second))

;; Names

(defrule name-component
    (+ (or quoted (not (or #\. #\] assignment whitespace))))
  (:text t))

(defrule name
    (and name-component (* (and #\. name-component)))
  (:destructure (first rest)
    (cons first (mapcar #'second rest))))

;; Sections

(defrule section
    (and #\[ name #\])
  (:destructure (open name close &bounds start end)
    (declare (ignore open close))
    (list :section (list name (cons start end)))))

;; Assignment variants

(defrule assignment/whitespace whitespace
  (:when (eq *assignment-expression* :whitespace)))

(defrule assignment/= #\=
  (:when (eq *assignment-expression* :=)))

(defrule assignment/\: #\:
  (:when (eq *assignment-expression* :\:)))

(defrule assignment
    (or assignment/whitespace assignment/= assignment/\:))

;; Options

(defrule value-whitespace/all
    whitespace
  (:when (eq *value-terminating-whitespace-expression* :all)))

(defrule value-whitespace/newline
    #\Newline
  (:when (eq *value-terminating-whitespace-expression* :newline)))

(defrule value-whitespace
    (or value-whitespace/all value-whitespace/newline))

(defrule value
    (+ (or quoted (not (or #\# #\[ value-whitespace))))
  (:text t))

(defrule option
    (and name (and (? whitespace) assignment (? whitespace)) value)
  (:destructure (name assignment value &bounds start end)
    (declare (ignore assignment))
    (list :option (make-node *builder* :option
                             :name   name
                             :value  value
                             :bounds (cons start end)))))

;; Entry point

(defrule ini
    (* (or comment whitespace section option))
  (:lambda (value)
    ;; Add all options to their respective containing sections.
    (let+ ((sections (make-hash-table :test #'equal))
           ((&flet+ ensure-section ((name bounds))
              (ensure-gethash
               name sections (apply #'make-node *builder* :section
                                    :name   name
                                    (when bounds
                                      (list :bounds bounds))))))
           (current-section-info '(() nil)))
      (mapc (lambda+ ((&optional kind node))
              (ecase kind
                ((nil)    ) ; comment and whitespace
                (:section (setf current-section-info node))
                (:option  (let ((section (ensure-section current-section-info)))
                            (add-child *builder* section node)))))
            value)
      (hash-table-values sections))))
