#lang racket

(require db
         web-server/servlet)

(require "dbconn.rkt"
         "model.rkt"
         "sessions.rkt")

; PAGE RENDERING

; consume a title and an X-expr and return a X-expr for a general page
; string X-expr -> X-expr
(define (page r page-title content #:sorter [sorter? #f] #:order [order "hot"])
  (response/xexpr
   #:preamble #"<!doctype html>"
   
   `(html (head (title ,page-title)
                (link ((rel "stylesheet") (type "text/css") (href "/static/style.css"))))
          
          (body ,(render-according-header db r sorter? order)
                ,content))))

; consume db, request, sorter and order and return appropriate header
(define (render-according-header db r sorter? order)
  (if (user-logged-in? db r)
      (render-logged-heading (current-user db r) sorter? order)
      (render-less-heading sorter? order)))

; render website sorter
(define (render-sorter order)
  (let ([hot (if (equal? order "hot") "sorter-link-active" "")]
        [new (if (equal? order "new") "sorter-link-active" "")]
        [top (if (equal? order "top") "sorter-link-active" "")])
    `((a ((class ,(string-append "sorter-link" " " hot)) (href "/?sort=hot")) "hot")
      (a ((class ,(string-append "sorter-link" " " new)) (href "/?sort=new")) "new")
      (a ((class ,(string-append "sorter-link" " " top)) (href "/?sort=top")) "top"))))

; render website heading
(define (render-heading sorter? order links)
  `(div ((class "heading"))
        (div ((class "heading-holder"))
             (div ((class "heading-helper"))

                  ; logo
                  (div ((class "heading-text"))
                       (a ((class "heading-link-main") (href "/"))
                          "#" (span ((class "base")) ".") "minimal"))

                  ; sort posts
                  ,(if sorter? `(div ((class "heading-sorter")) ,@(render-sorter order)) "")

                  ; render links
                  (div ((class "heading-links"))
                       (div ((style "width: 100%; text-align: right")) ,@links))))))

; links visible to anonymous user
(define ANON_LINKS
  '(("submit" "submit")
   ("about" "about")
   ("login" "sign in")
   ("signup" "sign up")))

; links visible to known user
(define KNOWN_LINKS
  '(("submit" "submit")
   ("about" "about")
   ("account" "account")
   ("do-logout" "sign out")))

; consume a list and a class type and return an X-expr representing an anchor tag
; list, string -> X-expr
(define (create-heading-link lat [css-class "heading-link"])
  `(a ((class ,css-class)
       (href ,(string-append "/" (car lat))))
      ,(cadr lat)))
  
; render website heading for logged in user
(define (render-logged-heading user sorter? order)
  (let* ([user-link-string (string-append "user/" (number->string (user-id user)))]
         [username (user-username user)]
         [links (map create-heading-link KNOWN_LINKS)]
         [user-link (list (create-heading-link (list user-link-string username) "username"))])
    
    (render-heading sorter? order (append links user-link))))

; render website heading for new user
(define (render-less-heading sorter? order)
  (render-heading sorter? order (map create-heading-link ANON_LINKS)))

; render website footer
(define (render-footer order start end posts-length)
  `(div ((class "footer"))
        (div ((class "controls"))

             ; render previous button
             ,(if [not (= start 0)]
                  `(a ((class "control-link")
                       (href ,(string-append "?sort="
                                             order
                                             "?start="
                                             (number->string (- start POSTS_PER_PAGE))
                                             "&end="
                                             (number->string (- end POSTS_PER_PAGE))))) "< prev") "     ")

             ; render next button
             ,(if [>= posts-length POSTS_PER_PAGE]
                  `(a ((class "control-link")
                       (href ,(string-append "?sort="
                                             order
                                             "&start="
                                             (number->string (+ start POSTS_PER_PAGE))
                                             "&end="
                                             (number->string (+ end POSTS_PER_PAGE))))) "next >") "     "))))

; render post voters
(define (render-voters type id vote)
  `(span ((class "voters"))
         (a ((class ,(string-append "voter-link " (if (and vote (= (vote-dir vote) 1)) "voted" "")))
             (href ,(string-append "/vote?type=" type "&id=" (number->string id) "&dir=1")))
            (span ((class "voter")) "▲"))
         (a ((class ,(string-append "voter-link " (if (and vote (= (vote-dir vote) 0)) "voted" "")))
             (href ,(string-append "/vote?type=" type "&id=" (number->string id) "&dir=0")))
            (span ((class "voter")) "▼"))))


; consume request and return the 'not found'
; request -> X-expr
(define (page-not-found r)
  (page r
        "Not Found"
        `(div ((class "items") (style "text-align: left;padding-top: 25px;"))
              (h3 "Wrong Turn, Bro!")
              (p ((style "line-height: 1.5em"))
                 "The page you requested does not exist..."))))

; consume request and return the about page
; request -> X-expr
(define (about-page r)
  (page r
        "About"
        `(div ((class "items") (style "text-align: left;padding-top: 25px;"))
              (h3 "About This Site")
              (p ((style "line-height: 1.5em"))
                 "Basketbase is a link aggregator/simple publishing platform ala Reddit. It is light and simple deploy. It's primary design goal is to be minimalist and an example of timeless design."
                 (br)(br)
                 "Designed and developed by David Gorski."))))

(provide (all-defined-out))