#lang info

(define collection "scribble")

(define deps '("base" "scribble-lib" "at-exp-lib"))
(define build-deps '("racket-doc" "scribble-doc"))

(define pkg-desc "Port of the acmsmall style to Scribble")
(define pkg-authors '(stamourv))

(define scribblings '(("acmsmall.scrbl" () ("Scribble Libraries"))))
