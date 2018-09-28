#lang racket

(require db
         web-server/servlet)

(require "dbconn.rkt"
         "model.rkt"
         "sessions.rkt"
         "views.rkt")

; PAGE RENDERING

; consume a title and an X-expr and return a X-expr for a general page
; string X-expr -> X-expr
(define (render-gnr-page r page-title content #:sorter [sorter #f] #:order [order "hot"])
  (response/xexpr
   #:preamble #"<!doctype html>"
   `(html (head (title ,page-title)
                (link ((rel "stylesheet")
                       (type "text/css")
                       (href "/static/style.css"))))
          (body        
           ,(if (user-logged-in? db r)
                (render-logged-heading (current-user db r) sorter order)
                (render-less-heading sorter order))
           ,content))))

(provide render-gnr-page)


