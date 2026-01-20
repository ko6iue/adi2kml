#!/usr/bin/env -S csi -script

(import test)

(load "../maidenhead.scm")

(parameterize
  ((current-test-epsilon 0.01))
  (test-group
    "Maidenhead to GPS"
    (test '()
      (maidenhead-to-gps "ZZ00"))
    (test '()
      (maidenhead-to-gps "DM4242"))
    (test '()
      (maidenhead-to-gps "DM42DM42DM"))
    (test '()
      (maidenhead-to-gps "00"))
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
