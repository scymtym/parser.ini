;;;; protocol.lisp --- Unit tests for the protocol of the parser.ini system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.ini.test)

(deftestsuite parser.ini.protocol (parser.ini.root)
  ()
  (:documentation
   "Unit tests for the protocol functions of the parser.ini system."))

(addtest (parser.ini.protocol
          :documentation
          "Smoke test for the generic function `parse'.")
  parse/smoke

  (ensure-cases (input builder expected)

      `((:no-such-method   list             ini-parse-error)
        (,#P"no-such-file" list             ini-parse-error)
        ("foo = bar"       :no-such-builder ini-parse-error))

    (let+ (((&flet do-it () (parse input builder))))
      (case expected
        (ini-parse-error (ensure-condition 'ini-parse-error (do-it)))
        (t               (ensure-same (do-it) expected :test #'equal))))))
