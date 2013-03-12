;;;; grammar.lisp --- Unit tests for the ini grammar.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.ini.test)

(deftestsuite parser.ini.grammar (parser.ini.root)
  ()
  (:documentation
   "Test suite for the grammar used by the parser.ini system."))

(addtest (parser.ini.grammar
          :documentation
          "Smoke test for the ini grammar.")
  smoke

  (ensure-cases (input expected)
      '((""             nil) ; Empty
        ("#"            nil) ; Empty comment
        ("# # comment"  nil) ; Comment in comment
        ("# no content" nil) ; Comment without content

        ;; Comment after value
        ("foo = 1 # comment"
         ((:section
           ((:option () :name ("foo") :value "1" :bounds (0 . 7)))
           :name ())))

        ;; Repeated section
        ("[foo] bar = 1 [foo] baz = 2"
         ((:section
           ((:option () :name ("bar") :value "1" :bounds (6 . 13))
            (:option () :name ("baz") :value "2" :bounds (20 . 27)))
           :name   ("foo")
           :bounds (0 . 5))))

        ;; Fancy escaping in names
        ("[foo.\"b\\az.fe\\\"z\"] bar\".whoop.di do\"o = 1"
         ((:section
           ((:option () :name ("bar.whoop.di doo") :value "1" :bounds (19 . 41)))
           :name   ("foo" "b\\az.fe\"z")
           :bounds (0 . 18))))

        ;; Fancy escaping in values
        ("bar = \"wh\\\"o\\\\op \"di\" doo\""
         ((:section
           ((:option () :name ("bar") :value "wh\"o\\op di doo" :bounds (0 . 26)))
           :name ()))))

    (let+ (((&flet do-it () (parse input 'list))))
      (case expected
        (error (ensure-condition 'error (do-it)))
        (t     (ensure-same (do-it) expected :test #'equal))))))
