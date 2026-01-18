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
  (regex)
  (srfi-1)) ; filter

; regular expression for parsing maidenhead coordinates
(define mh-regex
  (regexp
    (string-append
      "^([a-rA-R]{2})([0-9]{2})?|"
      "^([a-rA-R]{2})([0-9]{2})([a-xA-X]{2})([0-9]{2})?$")))

(define (maidenhead-to-gps text)
  ; input: maidenhead text coordinates
  ; returns ((lat lon) (lat-width lon-width))
  (let* ((mh-pairs (string-match mh-regex text))
         (ci char->integer)
         (char->step
           (lambda (char)
             (cond ; #\a > #\A > #\0 in unicode
               ((>= (ci char) (ci #\a))
                 (- (ci char) (ci #\a)))
               ((>= (ci char) (ci #\A))
                 (- (ci char) (ci #\A)))
               ((>= (ci char) (ci #\0))
                 (- (ci char) (ci #\0))))))
         (mh-pair->steps
           (lambda (mh-pair)
             (map char->step
               (string->list mh-pair))))
         (steps
           (if mh-pairs
             (map mh-pair->steps
               (filter identity
                 (cdr mh-pairs)))
             #f))
         (sw-corner-lat -90)
         (sw-corner-lon -180)
         (update-corner
           (lambda (steps widths)
             (set! sw-corner-lat
               (+ sw-corner-lat
                 (*
                   (cadr steps)
                   (car widths))))
             (set! sw-corner-lon
               (+ sw-corner-lon
                 (*
                   (car steps)
                   (cadr widths))))))
         (resolution-degrees
           `((10 20) (1 2) (,(/ 2.5 60) ,(/ 5.0 60))
             (,(/ 2.5 600) ,(/ 5.0 600)))))
    (if steps
      (begin
        (do ((i 0 (add1 i)))
          ((= i (length steps)))
          (update-corner
            (list-ref steps i)
            (list-ref resolution-degrees i)))
        (list (list sw-corner-lat sw-corner-lon)
          (list-ref resolution-degrees
            (- (length steps) 1))))
      #f)))

(define (maidenhead-to-gps-center text)
  (let ((coord (maidenhead-to-gps text)))
    (if coord
      (let ((lat (caar coord))
            (lon (cadar coord))
            (latscale (caadr coord))
            (lonscale (cadadr coord)))
        (list
          (exact->inexact (+ lat (/ latscale 2)))
          (exact->inexact (+ lon (/ lonscale 2)))))
      #f)))

(define pi 3.1415926)

(define (deg->rads deg)
  (* deg (/ pi 180)))

(define (rad->degs rad)
  (* rad (/ 180 pi)))

(define (haversine theta) ; in radians
  (expt (sin (/ theta 2)) 2))

(define (maidenhead-distance-bearing from to)
  (let* ((from-coord (maidenhead-to-gps-center from))
         (lat1 (deg->rads (car from-coord)))
         (lon1 (deg->rads (cadr from-coord)))
         (to-coord (maidenhead-to-gps-center to))
         (lat2 (deg->rads (car to-coord)))
         (lon2 (deg->rads (cadr to-coord)))
         (volumetric-mean-radius-earth-km 6371.0)
         (delta-lat (- lat2 lat1))
         (delta-lon (- lon2 lon1))
         (square-half-chord
           (+
             (haversine delta-lat)
             (*
               (cos lat1)
               (cos lat2)
               (haversine delta-lon))))
         (angular-distance
           (*
             2
             (atan (sqrt square-half-chord)
               (sqrt (- 1 square-half-chord)))))
         (distance-km (* angular-distance
                       volumetric-mean-radius-earth-km))
         (bearing
           (rad->degs
             (atan
               (*
                 (sin delta-lon)
                 (cos lat2))
               (-
                 (*
                   (cos lat1)
                   (sin lat2))
                 (*
                   (sin lat1)
                   (cos lat2)
                   (cos delta-lon))))))
         (bearing-final (if (< bearing 0)
                         (+ 360 bearing)
                         bearing)))
    (list distance-km bearing-final)))
