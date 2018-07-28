#lang racket

(require "model.rkt")

; render website heading
(define (render-heading sorter? order links)
  `(div ((class "heading"))
       (div ((class "heading-holder"))
            (div ((class "heading-helper"))
                 (div ((class "heading-text"))
                      (a ((class "heading-link-main") (href "/"))
                         "basket" (span ((class "base")) ".") "base"))
                 ,(if sorter? `(div ((class "heading-sorter")) ,@(render-sorter order)) "")
                 (div ((class "heading-links"))
                      (div ((style "width: 100%; text-align: right"))
                           ,@(map (lambda (x) x) links)))))))

; render website heading for logged in user
(define (render-logged-heading username sorter? order)
  (render-heading sorter? order (append (map (lambda (x) `(a ((class "heading-link")
                                                (href ,(string-append "/" (car x)))) ,(cadr x)))
                               '(("submit" "submit")
                                 ("about" "about")
                                 ("inbox" "inbox")
                                 ("account" "account")
                                 ("do-logout" "sign out"))) `((b ((class "username"))
                                                                 ,username)))))

; render website heading for new user
(define (render-less-heading sorter? order)
  (render-heading sorter? order (map (lambda (x) `(a ((class "heading-link")
                                                (href ,(string-append "/" (car x)))) ,(cadr x)))
                               '(("submit" "submit")
                                 ("about" "about")
                                 ("login" "sign in")))))

; render website footer
(define (render-footer)
  `(div ((class "footer"))
             (div ((class "controls"))
                  (a ((class "control-link") (href "prev")) "< prev")
                  (a ((class "control-link") (href "next")) "next >"))))

; render website sorter
(define (render-sorter order)
  (let ([hot (if (equal? order "hot") "sorter-link-active" "")]
        [new (if (equal? order "new") "sorter-link-active" "")]
        [top (if (equal? order "top") "sorter-link-active" "")])
  `((a ((class ,(string-append "sorter-link" " " hot)) (href "/?sort=hot")) "hot")
    (a ((class ,(string-append "sorter-link" " " new)) (href "/?sort=new")) "new")
    (a ((class ,(string-append "sorter-link" " " top)) (href "/?sort=top")) "top"))))

; render post voters
(define (render-voters x)
  `(div ((class "voters"))
       (a ((class "voter-link")
           (href ,(string-append "/vote?type=post&id=" (number->string (post-id x)) "&dir=up")))
          (span ((class "voter")) "▲"))
       (a ((class "voter-link")
           (href ,(string-append "/vote?type=post&id=" (number->string (post-id x)) "&dir=downv")))
          (span ((class "voter")) "▼"))))


; consume item x and return X-expr representing data
; item -> X-expr
(define (render-post x)
  `(div ((class "item"))
        (div ((class "heat-level"))
             (div ((class "heat-level-cont"))
                  ,(render-voters x)
                  ,(number->string (post-score x))))
        (a ((class "item-link")(href ,(post-url x))) (div ((class "content"))
             (div ((class "title")) ,(post-title x))
             (div ((class "url-sample")) ,(post-url x))))
        (div ((class "comments"))
             (div ((class "comment-container"))
                  (a ((class "comment-link")
                      (href ,(string-append  "/post/" (number->string (post-id x)))))
                     ,(string-append (number->string (post-numcom x)) " comments"))))))





; consume a list of items and return X-expr representing it
; list of item -> X-expr
(define (render-posts posts order)
    `(div ((class "items"))
        ,(render-top-posts (take posts 3))
        ,@(map render-post (cdddr posts))
        ,(render-footer)))

; consume a list of three items and return an X-exp representing it
; list of item -> X-expr
(define (render-top-posts lat)
  `(div ((class "top-items"))
        (div ((class "top-item"))
             (div ((class "img-crop") (style "height: 252px;")))
             ,(render-post (list-ref lat 0)))
        (div ((class "second-items"))
             (div ((class "img-crop") (style "height: 104px")))
             ,(render-post (list-ref lat 1))
             (div ((class "img-crop") (style "height: 104px")))
             ,(render-post (list-ref lat 2)))))


; # EXPORTS
(provide (all-defined-out))