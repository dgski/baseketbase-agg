#lang racket

(require web-server/servlet)

(require "page.rkt")

; consume request and return the 'not found'
; request -> X-expr
(define (page-not-found r)
  (render-gnr-page r
   "Not Found"
   `(div ((class "items") (style "text-align: left;padding-top: 25px;"))
         (h3 "Wrong Turn, Bro!")
         (p ((style "line-height: 1.5em"))
            "The page you requested does not exist..."))))

; consume request and return the about page
; request -> X-expr
(define (about-page r)
  (render-gnr-page r
                   "About"
                   `(div ((class "items") (style "text-align: left;padding-top: 25px;"))
                         (h3 "About This Site")
                         (p ((style "line-height: 1.5em"))
                            "Basketbase is a link aggregator/simple publishing platform ala Reddit. It is light and simple deploy. It's primary design goal is to be minimalist and an example of timeless design, such as the works of Deiter Rams."
                            (br)(br)
                            "Designed and developed by David Gorski."))))


(provide (all-defined-out))
