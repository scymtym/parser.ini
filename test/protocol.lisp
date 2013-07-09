;;;; protocol.lisp --- Unit tests for the protocol of the parser.ini system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.ini.test)

(in-suite parser.ini)

(test parse.smoke
  "Smoke test for the generic function `parse'."

  (mapc
   (lambda+ ((input builder expected))
     (let+ (((&flet do-it () (parse input builder))))
       (case expected
         (ini-parse-error (signals ini-parse-error (do-it)))
         (t               (is (equal expected (do-it)))))))

   `((:no-such-method   list             ini-parse-error)
     (,#P"no-such-file" list             ini-parse-error)
     ("foo = bar"       :no-such-builder ini-parse-error))))
