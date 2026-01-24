#!/usr/bin/env -S csi -script

(import srfi-1 test (chicken format))

(load "../maidenhead.scm")

(define parser-tests
  '((() "")
    (() " ")
    (() "YY00")
    (() "AB12YY")
    (() "AB12AB!")
    (() "!")
    (() "BC!")
    (() "SA00")
    ((#\Q #\R) "QR")
    ((#\A #\B #\1 #\2) " AB12 ")
    ((#\J #\K #\4 #\2 #\X #\O) "JK42XO")
    ((#\L #\O #\9 #\9 #\L #\X #\5 #\5) "LO99LX55")))

(test-group "parser"
  (let ((parser (maidenhead-parser-closure)))
    (for-each
      (lambda(el)
        (test (sprintf "\"~A\"" (cadr el)) (car el) (parser (cadr el))))
      parser-tests)))

(test '() (make-maidenhead "ZZ42"))

(parameterize
  ((current-test-epsilon 0.01))
  (test-group
    "distance bearing"
    (test 19013.87 (maidenhead-distance-km
                    (make-maidenhead "AA00AA00")
                    (make-maidenhead "AR00AX09")))
    (test 0.0 (maidenhead-bearing-degrees
               (make-maidenhead "AA00AA00")
               (make-maidenhead "AR00AX09")))))

(test-exit)
