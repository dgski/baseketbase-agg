#lang racket

(require db
         web-server/servlet)

(require "dbconn.rkt"
         "model.rkt"
         "sessions.rkt")

; PAGE RENDERING

; consume a title and an X-expr and return a X-expr for a general page
; string X-expr -> X-expr
(define (page r page-title content #:sorter [sorter #f] #:order [order "hot"])
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



; render website heading
(define (render-heading sorter? order links)
  `(div ((class "heading"))
       (div ((class "heading-holder"))
            (div ((class "heading-helper"))
                 (div ((class "heading-text"))
                      (a ((class "heading-link-main") (href "/"))
                         "#" (span ((class "base")) ".") "minimal"))
                 ,(if sorter? `(div ((class "heading-sorter")) ,@(render-sorter order)) "")
                 (div ((class "heading-links"))
                      (div ((style "width: 100%; text-align: right"))
                           ,@(map (lambda (x) x) links)))))))

; render website heading for logged in user
(define (render-logged-heading user sorter? order)
  (render-heading sorter? order (append (map (lambda (x) `(a ((class "heading-link")
                                                (href ,(string-append "/" (car x)))) ,(cadr x)))
                               '(("submit" "submit")
                                 ("about" "about")
                                 ("account" "account")
                                 ("do-logout" "sign out"))) `((a ((href ,(string-append "/user/" (number->string (user-id user)))) (class "username")) ,(user-username user)))   )))

; render website heading for new user
(define (render-less-heading sorter? order)
  (render-heading sorter? order (map (lambda (x) `(a ((class "heading-link")
                                                (href ,(string-append "/" (car x)))) ,(cadr x)))
                               '(("submit" "submit")
                                 ("about" "about")
                                 ("login" "sign in")
                                 ("signup" "sign up")))))

; render website footer
(define (render-footer order start end posts-length)
  `(div ((class "footer"))
        (div ((class "controls"))
             ,(if (not (= start 0)) `(a ((class "control-link") (href ,(string-append "?start=" (number->string (- start POSTS_PER_PAGE)) "&end=" (number->string (- end POSTS_PER_PAGE))))) "< prev") "     ")
             ,(if (>= posts-length POSTS_PER_PAGE) `(a ((class "control-link") (href ,(string-append "?sort=" order "&start=" (number->string (+ start POSTS_PER_PAGE)) "&end=" (number->string (+ end POSTS_PER_PAGE))))) "next >") "     "))))

; render website sorter
(define (render-sorter order)
  (let ([hot (if (equal? order "hot") "sorter-link-active" "")]
        [new (if (equal? order "new") "sorter-link-active" "")]
        [top (if (equal? order "top") "sorter-link-active" "")])
  `((a ((class ,(string-append "sorter-link" " " hot)) (href "/?sort=hot")) "hot")
    (a ((class ,(string-append "sorter-link" " " new)) (href "/?sort=new")) "new")
    (a ((class ,(string-append "sorter-link" " " top)) (href "/?sort=top")) "top"))))

; render post voters
(define (render-voters type id vote)
  `(span ((class "voters"))
       (a ((class ,(string-append "voter-link " (if (and vote (= (vote-dir vote) 1)) "voted" "")))
           (href ,(string-append "/vote?type=" type "&id=" (number->string id) "&dir=up")))
          (span ((class "voter")) "▲"))
       (a ((class ,(string-append "voter-link " (if (and vote (= (vote-dir vote) 0)) "voted" "")))
           (href ,(string-append "/vote?type=" type "&id=" (number->string id) "&dir=down")))
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
                            "Basketbase is a link aggregator/simple publishing platform ala Reddit. It is light and simple deploy. It's primary design goal is to be minimalist and an example of timeless design, such as the works of Deiter Rams."
                            (br)(br)
                            "Designed and developed by David Gorski."))))



(provide (all-defined-out))


