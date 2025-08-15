#lang info
(define collection "stamen")
(define deps '("base" "pollen"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/stamen.scrbl" ())))
(define pkg-desc "Library of standard typographic features for the Pollen typesetting framework.")
(define version "0.0")
(define pkg-authors '(TheJoyfulInquiries))
(define license '(Apache-2.0 OR MIT))
