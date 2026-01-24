#!/usr/bin/env -S csi -script

; Copyright (c) 2026, Matt Massie
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;
; 1. Redistributions of source code must retain the above copyright notice, this
;    list of conditions and the following disclaimer.
;
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
;
; 3. Neither the name of the copyright holder nor the names of its
;    contributors may be used to endorse or promote products derived from
;    this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(load "maidenhead.scm")

(import
  (scheme)
  (chicken irregex)
  (chicken format)
  (chicken io)
  (chicken process-context)
  (chicken flonum)
  (chicken string))

(define-record
  qso
  callsign
  name
  country
  qth
  gridsquare)

(define (make-empty-qso)
  (make-qso "" "" "" "" ""))

(define (qso-printer qso)
  (format #t "
  callsign: ~A
      name: ~A
   country: ~A
       QTH: ~A
gridsquare: ~A
"
    (qso-callsign qso)
    (qso-name qso)
    (qso-country qso)
    (qso-qth qso)
    (qso-gridsquare qso)))

(define (kml-header-write port)
  (format port "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n<Document>\n"))

(define (generate-kml-description my-mh their-mh adi-record)
  (if (not (and my-mh their-mh)) '()
    (sprintf
      "<h1>~A</h1>
<a href=\"https://www.qrz.com/db/~A\">QRZ Page</a><br/>
<b>QTH</b>: ~A<br/>
<b>Grid</b>: ~A<br/>
<b>Country</b>: ~A<br/>
<b>Distance</b>: ~A km<br/>
<b>Bearing</b>: ~A&deg;"
      (qso-name adi-record)
      (qso-callsign adi-record)
      (qso-qth adi-record)
      (qso-gridsquare adi-record)
      (qso-country adi-record)
      (maidenhead-distance-km my-mh their-mh)
      (maidenhead-bearing-degrees my-mh their-mh))))

(define (kml-record-write port my-mh adi-record)
  (let ((their-mh (make-maidenhead
                   (qso-gridsquare adi-record)))
        (cdata-open "<![CDATA[")
        (cdata-close "]]>"))
    (if (null? their-mh) '()
      (begin
        (format port "<Placemark><name>~A</name><description>~A~A~A</description>
<Point><coordinates>~A,~A,0</coordinates></Point></Placemark>
<Placemark><name>~A</name><LineString><tessellate>1</tessellate>
<coordinates>~A,~A,0\n~A,~A,0\n~A,~A,0\n~A,~A,0\n~A,~A,0</coordinates>
</LineString></Placemark>"
          (qso-callsign adi-record)
          cdata-open
          (generate-kml-description my-mh their-mh adi-record)
          cdata-close
          (mh-lon-center their-mh)
          (mh-lat-center their-mh)
          (string-append (qso-callsign adi-record) " maidenhead box")
          ;; sw corner
          (mh-lon-sw-corner their-mh)
          (mh-lat-sw-corner their-mh)
          ;; nw corner
          (mh-lon-sw-corner their-mh)
          (+ (mh-lat-sw-corner their-mh)
            (mh-lat-res-degrees their-mh))
          ;; ne corner
          (+ (mh-lon-res-degrees their-mh)
            (mh-lon-sw-corner their-mh))
          (+ (mh-lat-sw-corner their-mh)
            (mh-lat-res-degrees their-mh))
          ;; se corner
          (+ (mh-lon-res-degrees their-mh)
            (mh-lon-sw-corner their-mh))
          (mh-lat-sw-corner their-mh)
          ;; sw corner
          (mh-lon-sw-corner their-mh)
          (mh-lat-sw-corner their-mh))))))

(define (kml-footer-write port)
  (format port "\n</Document>\n</kml>\n"))

(define (update-adi-record record key value)
  (cond
    ((equal? key "call")
      (qso-callsign-set! record value))
    ((equal? key "name")
      (qso-name-set! record value))
    ((equal? key "country")
      (qso-country-set! record value))
    ((equal? key "qth")
      (qso-qth-set! record value))
    ((equal? key "gridsquare")
      (qso-gridsquare-set! record value))))

(define processed-callsigns '())

(define (process-adi-record kml-port my-gridsquare record)
  (let ((callsign (qso-callsign record)))
    (if (not (member callsign processed-callsigns))
      (begin
        (when (not (null?
                    (kml-record-write
                      kml-port
                      my-gridsquare
                      record)))
          (set! processed-callsigns
            (cons callsign processed-callsigns)))))))

(define (final-output)
  (format #t "Processed ~A unique callsigns with maidenhead data\n"
    (length processed-callsigns)))

(define (adi2kml my-gridsquare adi-filename kml-filename)
  (let ((my-mh (make-maidenhead my-gridsquare))
        (kml-port (open-output-file kml-filename))
        (adi-kv-regex (irregex "^<(.*):[0-9:]+>(.*)$")))
    (if (null? my-mh)
      (begin
        (format #t "Invalid maidenhead: ~A\n" my-gridsquare)
        (final-output))
      (begin
	; setup float number print precision
	(flonum-print-precision 5)

        ; header
        (kml-header-write kml-port)
        ; process adi records
        (call-with-input-file
          adi-filename
          (lambda (port)
            (let ((last-record '#f))
              (do
                ((record (make-empty-qso)))
                ((eq? #t last-record))

                (do
                  ((line (read-line port) (read-line port)))
                  ((eq? #!eof line) (set! last-record #t))

                  (let ((kv (irregex-match adi-kv-regex line)))
                    (if kv
                      (update-adi-record record
                        (irregex-match-substring kv 1)
                        (irregex-match-substring kv 2))
                      (when (substring-index "<eor>" line)
                        (process-adi-record
                          kml-port
                          my-mh
                          record)
                        (set! record (make-empty-qso))))))))))
        ; footer
        (kml-footer-write kml-port)
        (final-output)))))

(define (adi2kml-args args)
  (if (= 3 (length args))
    (adi2kml (car args)
      (cadr args)
      (caddr args))
    (begin
      (format #t "\nERROR invalid arguments: ~A\n\n" args)
      (display "adi2kml reads ADIF files and writes KML files\n\n")
      (display "  Usage: ./adi2kml.scm <your maidenhead grid> <adi file> <kml file>\n")
      (display "Example: ./ad2kml.scm DM45NM mylog.adi myworld.kml\n\n")
      #f)))

(adi2kml-args (command-line-arguments))
