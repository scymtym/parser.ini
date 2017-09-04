;;;; grammar.lisp --- Unit tests for the ini grammar.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2017 Jan Moringen
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
        (:section-option (((:option () :name ("foo") :value "1" :bounds (0 . 7))
                           . ())))
        :name ())))

     ;; Repeated section
     ("[foo] bar = 1 [foo] baz = 2"
      ((:section
        (:section-option
         (((:option () :name ("bar") :value "1" :bounds (6 . 13)) . ())
          ((:option () :name ("baz") :value "2" :bounds (20 . 27)) . ())))
        :name   ("foo")
        :bounds (0 . 5))))

     ;; [] in section name
     ("[foo[bar]] baz=2"
      ((:section
        (:section-option
         (((:option () :name ("baz") :value "2" :bounds (11 . 16)))))
        :name   ("foo[bar]")
        :bounds (0 . 10))))

     ;; whitespace in section name
     ("[foo bar] baz=2"
      ((:section
        (:section-option
         (((:option () :name ("baz") :value "2" :bounds (10 . 15)))))
        :name   ("foo bar")
        :bounds (0 . 9))))

     ;; Fancy escaping in names
     ("[foo.\"b\\az.fe\\\"z\"] frob.bar\".whoop.di do\"o = 1"
      ((:section
        (:section-option
         (((:option () :name ("frob" "bar.whoop.di doo") :value "1"
                    :bounds (19 . 46))
           . ())))
        :name   ("foo" "b\\az.fe\"z")
        :bounds (0 . 18))))

     ;; whitespace in option name
     ("foo bar = "
      ((:section
        (:section-option
         (((:option () :name ("foo bar") :value "" :bounds (0 . 10)))))
        :name ())))

     ;; Empty value
     ("bar = "
      ((:section
        (:section-option
         (((:option () :name ("bar") :value "" :bounds (0 . 6)))))
        :name ())))

     ;; Fancy escaping in values
     ("bar = \"wh\\\"o\\\\op \"di\" doo\""
      ((:section
        (:section-option
         (((:option () :name ("bar") :value "wh\"o\\op di doo" :bounds (0 . 26))
           . ())))
        :name ())))

     ;; Multiple lines
     ("a.b=1
       a=2
       # comment
       [a]
       c = 3
       [d]
       e = 4"
      ((:section
        (:section-option
         (((:option () :name ("a" "b") :value "1" :bounds (0 . 5)) . ())
          ((:option () :name ("a") :value "2" :bounds (13 . 16)) . ())))
        :name ())
       (:section
        (:section-option
         (((:option () :name ("c") :value "3" :bounds (52 . 57)) . ())))
        :name ("a") :bounds (41 . 44))
       (:section
        (:section-option
         (((:option () :name ("e") :value "4" :bounds (76 . 81)) . ())))
        :name ("d") :bounds (65 . 68)))))))

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
        (:section-option
         (((:option () :name ("baz.fez") :value "1" :bounds (10 . 21)) . ())))
        :name ("foo.bar") :bounds (0 . 9))))

     (#\. "[foo.bar] baz.fez = 1"
      ((:section
        (:section-option
         (((:option () :name ("baz" "fez") :value "1" :bounds (10 . 21)) . ())))
        :name ("foo" "bar") :bounds (0 . 9))))

     (#\: "[foo:bar] baz.fez = 1"
      ((:section
        (:section-option
         (((:option () :name ("baz.fez") :value "1" :bounds (10 . 21)) . ())))
        :name ("foo" "bar") :bounds (0 . 9)))))))
