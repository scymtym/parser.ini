;;;; grammar.lisp --- Unit tests for the ini grammar.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.ini.test)

(in-suite parser.ini)

(test grammar.smoke
  "Smoke test for the ini grammar."

  (mapc
   (lambda+ ((input expected))
     (let+ (((&flet do-it () (parse input 'list))))
       (case expected
         (ini-parse-error (signals ini-parse-error (do-it)))
         (t               (is (equal expected (do-it)))))))

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
     ("[foo.\"b\\az.fe\\\"z\"] frob.bar\".whoop.di do\"o = 1"
      ((:section
        ((:option () :name ("frob" "bar.whoop.di doo") :value "1"
          :bounds (19 . 46)))
        :name   ("foo" "b\\az.fe\"z")
        :bounds (0 . 18))))

     ;; Fancy escaping in values
     ("bar = \"wh\\\"o\\\\op \"di\" doo\""
      ((:section
        ((:option () :name ("bar") :value "wh\"o\\op di doo" :bounds (0 . 26)))
        :name ()))))))

(test grammar.name-component-separator
  "Tests controlling name component separator via special variable."

  (mapc
   (lambda+ ((separator input expected))
     (let+ (((&flet do-it ()
               (let ((*name-component-separator* separator))
                 (parse input 'list)))))
       (case expected
         (ini-parse-error (signals ini-parse-error (do-it)))
         (t               (is (equal expected (do-it)))))))

   '((nil "[foo.bar] baz.fez = 1"
      ((:section
        ((:option () :name ("baz.fez") :value "1" :bounds (10 . 21)))
        :name ("foo.bar") :bounds (0 . 9))))

     (:. "[foo.bar] baz.fez = 1"
      ((:section
        ((:option () :name ("baz" "fez") :value "1" :bounds (10 . 21)))
        :name ("foo" "bar") :bounds (0 . 9))))

     (:\: "[foo:bar] baz.fez = 1"
      ((:section
        ((:option () :name ("baz.fez") :value "1" :bounds (10 . 21)))
        :name ("foo" "bar") :bounds (0 . 9)))))))
