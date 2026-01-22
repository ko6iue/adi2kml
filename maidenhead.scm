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

(import
  (chicken format)
  abnf
  lexgen
  srfi-1)

(define-record
  mh
  text
  lat-sw-corner
  lon-sw-corner
  lat-res-degrees
  lon-res-degrees
  lat-center
  lon-center)

(define (maidenhead-print mh)
  (format #t "
           text: ~A
  lat-sw-corner: ~A
  lon-sw-corner: ~A
lat-res-degrees: ~A
lon-res-degrees: ~A
     lat-center: ~A
     lon-center: ~A
"
    (mh-text mh)
    (mh-lat-sw-corner mh)
    (mh-lon-sw-corner mh)
    (mh-lat-res-degrees mh)
    (mh-lon-res-degrees mh)
    (mh-lat-center mh)
    (mh-lon-center mh)))

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

(define (make-maidenhead-closure)
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
      (let ((mh-record (make-mh text -90 -180 0 0 0 0))
            (steps (parse-mh text))
            (calc-center (lambda(point res)
                          (exact->inexact (+ point (/ res 2))))))
        (if (null? steps)
          '() ; invalid MH text early return
          (begin
            (let loop ((i 0) (steps steps))
              (if (null? steps) ; sw corner calculation complete
                (let ((res (list-ref stepsize-degrees (- i 1))))
                  ; set lat lon resolution
                  (mh-lat-res-degrees-set! mh-record (car res))
                  (mh-lon-res-degrees-set! mh-record (cadr res))
                  ; calculate and save center point
                  (mh-lat-center-set!
                    mh-record
                    (calc-center
                      (mh-lat-sw-corner mh-record)
                      (mh-lat-res-degrees mh-record)))
                  (mh-lon-center-set!
                    mh-record
                    (calc-center
                      (mh-lon-sw-corner mh-record)
                      (mh-lon-res-degrees mh-record)))
                  ; return completed record
                  mh-record)
                (let ((lonlat (take steps 2))
                      (res (list-ref stepsize-degrees i)))
                  (mh-lat-sw-corner-set! mh-record
                    (+ (mh-lat-sw-corner mh-record)
                      (*(cadr lonlat) (car res))))
                  (mh-lon-sw-corner-set! mh-record
                    (+ (mh-lon-sw-corner mh-record)
                      (*(car lonlat) (cadr res))))
                  (loop (+ i 1) (drop steps 2)))))
            mh-record))))))

(define make-maidenhead (make-maidenhead-closure))

(define pi 3.1415926)

(define (deg->rads deg)
  (* deg (/ pi 180)))

(define (rad->degs rad)
  (* rad (/ 180 pi)))

(define (maidenhead-distance-km from to)
  (if (not (and from to)) '()
    (let* ((lat1 (deg->rads (mh-lat-center from)))
           (lon1 (deg->rads (mh-lon-center from)))
           (lat2 (deg->rads (mh-lat-center to)))
           (lon2 (deg->rads (mh-lon-center to)))
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
        volumetric-mean-radius-earth-km))))

(define (maidenhead-bearing-degrees from to)
  (if (not (and from to)) '()
    (let* ((lat1 (deg->rads (mh-lat-center from)))
           (lon1 (deg->rads (mh-lon-center from)))
           (lat2 (deg->rads (mh-lat-center to)))
           (lon2 (deg->rads (mh-lon-center to)))
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
      bearing-final)))
