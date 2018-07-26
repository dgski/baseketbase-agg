#lang web-server

; # REQUIRE
(require web-server/servlet
         web-server/servlet-env
         racket/port
         racket/date
         threading
         )

; # REQUIRE LOCAL
(require "views.rkt")
(require "model.rkt")
(require "sessions.rkt")

; # CONSTANT
(define POST_DECAY_RATE (expt 0.5 (/ 1 86400)))

; # CREATE DATABASE CONNECTION
(define db
  (sqlite3-connect #:database "baseketbase.db" #:mode 'create))

; # SETUP CRYPTOGRAPHY
(setup-crypto)

; # RENDER FUNCTIONS

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


; Calculate the heat level of the post
(define (calc-post-heat x)
  (* (post-score x) (expt POST_DECAY_RATE (- (current-datetime) (post-datetime x)))))


; consume a string and return list
(define (get-sorted-posts db type)
  (let ([posts (get-posts db)])
    (cond
      [(equal? type "hot")
       (~> posts
           (map (lambda (x) (cons (calc-post-heat x) x)) _)
           (sort _ (lambda (a b) (if (< (post-score (cdr a)) (post-score (cdr b))) #f #t)))
           (map (lambda (x) (cdr x)) _))]
      [(equal? type "top")
       (sort posts (lambda (a b) (if (< (post-score a) (post-score b)) #f #t)))]
      [(equal? type "new")
       (sort posts (lambda (a b) (if (< (post-datetime a) (post-datetime b)) #f #t)))]))
  )
    

; consume a list of items and return X-expr representing it
; list of item -> X-expr
(define (render-posts order)
  (let ([posts (get-sorted-posts db order)])
    `(div ((class "items"))
        ,(render-top-posts (take posts 3))
        ,@(map render-post (cdddr posts))
        ,(render-footer))))

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

; consume a title and an X-expr and return a X-expr for a general page
; string X-expr -> X-expr
(define (render-gnr-page r title content #:sorter [sorter #f] #:order [order "hot"])
  (response/xexpr
   #:preamble #"<!doctype html>"
   `(html
     (head
      (title "basketbase")
      (link ((rel "stylesheet") (type "text/css") (href "/static/style.css"))))
     (body        
      ;,(render-heading #f)
      ,(if (user-logged-in? db r)
           (render-logged-heading (user-username (current-user db r)) sorter order)
           (render-less-heading sorter order))
      ,content))))

; consume a comment and a depth and return a X-expr representing it and all of it's children
; comment number -> X-expr
(define (render-comment x depth)
  (let ([current (car x)]
        [replies (cadr x)])
  `(div ((class"comment" ))
        (div ((class "comment-aligner"))
             (div ((class "comment-content"))
                  (div ((class "comment-username")) (span ((class "voters"))
                                                          (span ((class "voter")) "▼")
                                                          (span ((class "voter")) "▲"))
                       ,(uid->db->string db (comment-uid current)))
                  (div ((class "comment-body")) ,(comment-body current)))
             (div ((class "comment-datetime"))
                  (div ((class "datetime-container"))
                       ,(date->string (seconds->date (comment-datetime current)) #t))))
        
         ,(if [> 4 depth]`(div ((class "comment-replies"))
         ,@(map (lambda (x) (render-comment x (+ 1 depth))) replies))""))))

; consume a list of comments and return a X-expr representing it
(define (render-comments comms)
  (map (lambda (x) (render-comment x 0)) comms))

; # RECIEVING UPDATES

; Consume a request containing new post information and add it to database  if valid
; request -> redirect to "/"
(define (submit-post r)
  (post->db db (parse-post (current-user db r) (request-bindings r)))
  (redirect-to "/"))

; consume request, pid and return the post page (after adding comment)
; request -> redirect to "/post/pid"
(define (add-comment r pid)
  (let ([bindings (request-bindings r)])
    (comment->db db (comment 0
                             (user-id (current-user db r))
                             pid
                             1
                             0
                             1
                             "2018-07-20"
                             (extract-binding/single 'body bindings)
                             -1)))
  (inc-comment-db db pid)
  (redirect-to (string-append "/post/" (number->string pid))))

; consume request return previous page


(define (submit-vote r)
  (let* ([bindings (request-bindings r)]
         [type (extract-binding/single 'type bindings)]
         [pid (string->number (extract-binding/single 'id bindings))]
         [dir (extract-binding/single 'dir bindings)]
         [uid (user-id (current-user db r))])

    (cond
      [(equal? type "post")
       (if (user-voted-on-post db uid pid)
            (redirect-to "/")
           (begin (vote->db db (vote 0
                                     uid
                                     pid
                                     -1
                                     0 ;post
                                     (if (equal? dir "up") 1 0)))
                  (alter-post-vote db pid dir) ; Change post score
                  (redirect-to "/")))])))
       
    
    
                    



; # PAGE RESPONSE FUNCTIONS

; consume request and return the front page
; request -> X-expr
(define (front-page r)
  (let* ([bindings (request-bindings r)]
         [order (if (exists-binding? 'sort bindings) (extract-binding/single 'sort bindings) "hot")])
    (render-gnr-page  #:order order #:sorter #t r "basketbase - Front Page" (render-posts order))))

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

; consume request and return the account page
; request -> X-expr
(define (account-page r)
  (render-gnr-page r
   (string-append "id" "'s Profile")
   (let ([u (current-user db r)])
   `(div ((class "items") (style "margin-bottom: 70px;"))
         (div ((style "padding-top: 25px; text-align: left"))
              (h3 ,(string-append "Account Information for '" (user-username u) "'"))
              (br)
              "Change email:"(br)(br)
              (input ((class "our-input") (value ,(user-email u)) (type "email") (name "email")))
              (br)(br)
              "Change profile:"(br)(br)
              (textarea ((width "fill")
                         (placeholder "body")
                         (style "margin-bottom: 30px")
                         (class "our-input submit-input submit-text-area"))
                        ,(user-profile u))
              (br)
              "Change password:"(br)(br)
              (input ((class "our-input") (type "password") (placeholder "old password") (name "old-password")))
              (br)
              (input ((class "our-input") (type "password") (placeholder "new password") (name "new-password-1")))
              (br)
              (input ((class "our-input") (type "password") (placeholder "re-type new password") (name "new-password-2")))
              (br)
              (button ((class "our-button")) "save changes")
              (button ((class "our-button") (style "background-color: brown")) "delete account"))))))
              
   
; consume request and return the post being requested along with comments
; request -> X-expr
(define (post-page r id)
  (render-gnr-page r
   "Post Page"
   `(div ((class "items") (style "padding-top: 35px; padding-bottom: 35px"))
         ,(render-post (pid->db->post db id))
         (div ((class "comment-box"))
              ,(if (user-logged-in? db r)
                   `(form ((class "reply-box")
                           (action ,(string-append "/add-comment/" (number->string id))))
                          (div ((class "reply-box-textarea"))
                               (textarea ((placeholder "New Commment...")
                                          (class "our-input submit-input submit-text-area")
                                          (style "height: 50px;")
                                          (name "body"))))
                          (div ((class "reply-box-button"))
                               (button ((class "our-button") (style "margin-right: 0px;")) "Post"))) ""))
               ,@(render-comments (pid->db->comms db id)))))

; consume request and return the submit page
; request -> X-expr
(define (submit-page r)
  (render-gnr-page r
   "basketbase - Submit Page"
   `(div ((class "items"))
         (div ((class "submit"))
              (form ((action "submit-new-post"))
                    (p "Submit something new:")
                    (input ((class "our-input submit-input") (type "text") (placeholder "title") (name "title")))
                    (br)
                    (p "Link:")
                    (input ((class "our-input submit-input") (type "text") (placeholder "url") (name "url")))
                    (br)
                    (p "Or Write Something Yourself:")
                    (textarea ((width "fill")
                               (placeholder "body")
                               (class "our-input submit-input submit-text-area")
                               (name "body")))
                    (br)
                    (button ((class "our-button")) "submit"))))))

;consumes request and produces X-xexpr representing the login page
(define (login-page r)
  (render-gnr-page r
   "Login"
   '(div ((class "items"))
         (div ((class "top-items"))
              (div ((class "top-item login") (style "width: 85px"))
                   (form ((action "do-login"))
                         (input ((class "our-input") (type "text") (placeholder "username") (name "username")))
                         (input ((class "our-input") (type "password") (placeholder "password") (name "password")))
                         (button ((class "our-button")) "continue")))
              (div ((class "second-items info"))
                   "You can sign up instantly by entering your desired username and password in the log in fields."
                   (br)(br)
                   "Enjoy being a part of this community!")))))

;consumes request and logs user in
(define (do-login r)
  (attempt-user-login r db))

; consumes request and logs user out
(define (do-logout r)
  (attempt-user-logout r db))

; Login required
; consumes function and returns wrapping lambda which verifies request contains valid session information before running function
; function -> function
(define (logreq f)
  (lambda args (if (user-logged-in? db (car args)) (apply f args) (redirect-to "login"))))

; # STATIC FILE SERVING

; consume request and filename and send that file back to use
; request -> X-expr
; Future : make sure that MIME type is correct!!!!!
(define (serve-asset r f)
 (response 200 #"OK" 0 #"text/css" empty (lambda (op)
(with-input-from-file (string-append "static/" f) (lambda () (copy-port
(current-input-port) op))))))


; # REQUEST DISPATCHING

; Consume request and return the right thing
; request -> X-expr
(define (start r)
  (dispatch r))

; Request dispatching Table
(define-values (dispatch url)
  (dispatch-rules

   ; Main pages
   [("") front-page]
   [("about") about-page]
   [("post" (integer-arg)) post-page]
   
   [("account") (logreq account-page)]
   [("submit-new-post") (logreq submit-post)]

   ; Posting
   [("submit") (logreq submit-page)]
   [("add-comment" (integer-arg)) (logreq add-comment)]
   [("vote") (logreq submit-vote)]

   ; Login management
   [("login") login-page]
   [("do-login") do-login]
   [("do-logout") do-logout]
   
   [("static" (string-arg)) serve-asset]                              
   [else (lambda (x) (response/xexpr "WRONG TURN, BRO"))]))


; Start the server
(serve/servlet start
               #:extra-files-paths (list (build-path (current-directory) "static"))  ; directory for static files
               #:launch-browser? #f
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:port 8080)
