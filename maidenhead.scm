#!/usr/bin/env -S csi -script

; Copyright (c) 2026, Matt Massie
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are
; met:
;
; 1.  Redistributions of source code must retain the above copyright
;     notice, this list of conditions and the following disclaimer.
;
; 2.  Redistributions in binary form must reproduce the above copyright
;     notice, this list of conditions and the following disclaimer in the
;     documentation and/or other materials provided with the distribution.
;
; 3.  Neither the name of the copyright holder nor the names of its
;     contributors may be used to endorse or promote products derived from
;     this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS
; IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(import abnf
  lexgen
  srfi-1)

(define (make-maidenhead-lexer-closure)
  ;; closure to capture ABNF definitions for
  ;; a maidenhead designation
  (let* ((field-chars (repetition-n 2 (alternatives
                                       (range #\A #\R)
                                       (range #\a #\r))))
         (square-chars (repetition-n 2 decimal))
         (subsquare-chars (repetition-n 2 (alternatives
                                           (range #\A #\X)
                                           (range #\a #\x))))
         (extended-chars square-chars)
         (between-wsp (lambda (p)
                       (concatenation
                         (drop-consumed (optional-sequence wsp))
                         p
                         (drop-consumed (optional-sequence wsp)))))
         (maidenhead (between-wsp
                      (concatenation
                        field-chars
                        (optional-sequence
                          (concatenation
                            square-chars
                            (optional-sequence
                              (concatenation
                                subsquare-chars
                                (optional-sequence
                                  extended-chars)))))))))
    (lambda (text)
      (let ((parsed (lex maidenhead identity text)))
        (cond
          ((null? (cadr parsed)) (car parsed))
          (else '()))))))

(define (make-maidenhead-calculations-closure)
  (let* ((mh-lexer (make-maidenhead-lexer-closure))
         (ci char->integer)
         (char-to-step
           (lambda (c)
             (cond ; #\a > #\A > #\0 in unicode/ascii
               ((>= (ci c) (ci #\a)) (- (ci c) (ci #\a)))
               ((>= (ci c) (ci #\A)) (- (ci c) (ci #\A)))
               ((>= (ci c) (ci #\0)) (- (ci c) (ci #\0))))))
         (stepsize-degrees
           `((10 20) (1 2) (,(/ 2.5 60) ,(/ 5.0 60))
             (,(/ 2.5 600) ,(/ 5.0 600))))
         (parse-mh
           (lambda(text)
             (map char-to-step (mh-lexer text)))))
    (lambda (text)
      (let ((sw-corner-lat -90)
            (sw-corner-lon -180)
            (steps (parse-mh text)))
        (if (null? steps) '()
          (let loop ((i 0) (steps steps))
            (if (null? steps)
              `((,sw-corner-lat ,sw-corner-lon)
                ,(list-ref stepsize-degrees (- i 1)))
              (let ((lonlat (take steps 2))
                    (res (list-ref stepsize-degrees i)))
                (set! sw-corner-lat
                  (+ sw-corner-lat (*(cadr lonlat) (car res))))
                (set! sw-corner-lon
                  (+ sw-corner-lon (*(car lonlat) (cadr res))))
                (loop (+ i 1) (drop steps 2))))))))))

(define maidenhead-to-gps (make-maidenhead-calculations-closure))

(define (maidenhead-to-gps-center text)
  (let ((coord (maidenhead-to-gps text)))
    (if (null? coord) '()
      (let ((lat (caar coord))
            (lon (cadar coord))
            (latscale (caadr coord))
            (lonscale (cadadr coord)))
        `(,(exact->inexact (+ lat (/ latscale 2)))
          ,(exact->inexact (+ lon (/ lonscale 2))))))))

(define pi 3.1415926)

(define (deg->rads deg)
  (* deg (/ pi 180)))

(define (rad->degs rad)
  (* rad (/ 180 pi)))

(define (maidenhead-distance-gps from to)
  (let* ((lat1 (deg->rads (car from)))
         (lon1 (deg->rads (cadr from)))
         (lat2 (deg->rads (car to)))
         (lon2 (deg->rads (cadr to)))
         (volumetric-mean-radius-earth-km 6371.0)
         (haversine
           (lambda(theta)
             (expt (sin (/ theta 2)) 2)))
         (square-half-chord
           (+
             (haversine (- lat2 lat1))
             (* (cos lat1) (cos lat2)
               (haversine (- lon2 lon1)))))
         (angular-distance
           (* 2
             (atan
               (sqrt square-half-chord)
               (sqrt (- 1 square-half-chord))))))
    (* angular-distance
      volumetric-mean-radius-earth-km)))

(define (maidenhead-distance from to)
  (let ((from-coord (maidenhead-to-gps-center from))
        (to-coord (maidenhead-to-gps-center to)))
    (if (not (and from-coord to-coord)) '()
      (maidenhead-distance-gps from-coord to-coord))))

(define (maidenhead-bearing-gps from to)
  (let* ((lat1 (deg->rads (car from)))
         (lon1 (deg->rads (cadr from)))
         (lat2 (deg->rads (car to)))
         (lon2 (deg->rads (cadr to)))
         (delta-lon (- lon2 lon1))
         (bearing
           (rad->degs
             (atan (* (sin delta-lon) (cos lat2))
               (-
                 (* (cos lat1) (sin lat2))
                 (* (sin lat1) (cos lat2) (cos delta-lon))))))
         (bearing-final (if (< bearing 0)
                         (+ 360 bearing)
                         bearing)))
    bearing-final))

(define (maidenhead-bearing from to)
  (let ((from-coord (maidenhead-to-gps-center from))
        (to-coord (maidenhead-to-gps-center to)))
    (if (not (and from-coord to-coord)) '()
      (maidenhead-bearing-gps from-coord to-coord))))
