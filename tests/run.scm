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

(define mh-to-gps
  '((((30 -120) (10 20)) "DM")
    (((38 -102) (1 2)) "DM98")
    (((38.54 -101.75) (0.0416 0.0833)) "DM98dn")))

(parameterize ((current-test-epsilon 0.01))
  (test-group "maidenhead to gps"
    (for-each
      (lambda(el)
        (let ((rval (maidenhead-to-gps (cadr el)))
              (expect (car el)))
          (test
            (sprintf "lat: \"~A\"" (cadr el))
            (caar expect)
            (caar rval))
          (test
            (sprintf "lon: \"~A\"" (cadr el))
            (cadar expect)
            (cadar rval))
          (test
            (sprintf "latstep: \"~A\"" (cadr el))
            (caadr expect)
            (caadr rval))
          (test
            (sprintf "lonstep: \"~A\"" (cadr el))
            (cadadr expect)
            (cadadr rval))))
      mh-to-gps)))

(parameterize
  ((current-test-epsilon 0.01))
  (test-group
    "Maidenhead to GPS"
    (test '(35.0 -110.0)
      (maidenhead-to-gps-center "DM"))
    (test '(38.5 -101.0)
      (maidenhead-to-gps-center "DM98"))
    (test 19013.87 (maidenhead-distance "AA00AA00" "AR00AX09"))
    (test 0.0 (maidenhead-bearing "AA00AA00" "AR00AX09"))))

(test-exit)
