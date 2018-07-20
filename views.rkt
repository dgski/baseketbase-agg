#lang racket


(require web-server/servlet
         web-server/servlet-env)

(define (render-page x)
  (response/xexpr '(html x)))

(define (yell)
  (write "YELLING!"))

(provide render-page)
(provide yell)