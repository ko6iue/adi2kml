#!/usr/bin/env -S csi -script

(import srfi-1 test (chicken format))

(load "../maidenhead.scm")

(define lexer-tests
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

(test-group "lexer"
  (let ((lexer (make-maidenhead-lexer-closure)))
    (for-each
      (lambda(el)
        (test (sprintf "\"~A\"" (cadr el)) (car el) (lexer (cadr el))))
      lexer-tests)))

(parameterize
  ((current-test-epsilon 0.01))
  (test-group
    "Maidenhead to GPS"
    (test '((30 -120) (10 20))
      (maidenhead-to-gps "DM"))
    (test '(35.0 -110.0)
      (maidenhead-to-gps-center "DM"))
    (test '((38 -102) (1 2))
      (maidenhead-to-gps "DM98"))
    (test '(38.5 -101.0)
      (maidenhead-to-gps-center "DM98"))
    (test 38.54 (caar (maidenhead-to-gps "DM98dn")))
    (test -101.75 (cadar (maidenhead-to-gps "DM98dn")))
    (test 0.0416 (caadr (maidenhead-to-gps "DM98dn")))
    (test 0.08333 (cadadr (maidenhead-to-gps "DM98dn")))
    (test 0.93007 (haversine 9.96))
    (test 19013.87 (car (maidenhead-distance-bearing "AA00AA00" "AR00AX09")))
    (test 0.0 (cadr (maidenhead-distance-bearing "AA00AA00" "AR00AX09")))))

(test-exit)
