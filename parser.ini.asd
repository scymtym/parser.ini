;;;; parser.ini.asd --- System definition for the parser.ini system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.ini-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:parser.ini-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 2
  "Minor component of version number.")

(defparameter +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))

;;; System definitions

(defsystem :parser.ini
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Provides parsing of Ini expressions."
  :depends-on  (:alexandria
                (:version :let-plus        "0.2")
                (:version :more-conditions "0.1.0")

                (:version :esrap           "0.9"))
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "variables")
                              (:file       "protocol")
                              (:file       "grammar")
                              (:file       "list-builder")))

                (:static-file "README.org")

                (:module     "examples"
                 :components ((:static-file "etc.lisp"))))
  :in-order-to ((test-op (test-op :parser.ini-test))))

(defmethod perform :after ((operation load-op)
                           (component (eql (find-system :parser.ini))))
  ;; Since version 0.2
  (pushnew :parser.ini.name-component-separator *features*))

(defsystem :parser.ini-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Provides parsing of Ini expressions."
  :depends-on  (:alexandria
                (:version :let-plus   "0.2")
                (:version :fiveam     "1.0")

                (:version :parser.ini #.(version/string)))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "grammar")))))

(defmethod perform ((op     test-op)
                    (system (eql (find-system :parser.ini-test))))
  (funcall (find-symbol "RUN-TESTS" :parser.ini.test)))
