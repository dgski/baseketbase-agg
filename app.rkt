#lang web-server

(require web-server/servlet
         web-server/servlet-env
         racket/port
         web-server/http/id-cookie
         (prefix-in netcookies: net/cookies/server))

(require "views.rkt")
(require "model.rkt")


; # CREATE DATABASE CONNECTION
(define db
  (sqlite3-connect #:database "test-r.db" #:mode 'create))

; # RENDER FUNCTIONS

; render website heading
(define (render-heading sorter? links)
  `(div ((class "heading"))
       (div ((class "heading-holder"))
            (div ((class "heading-helper"))
                 (div ((class "heading-text"))
                      (a ((class "heading-link-main") (href "/"))
                         "basket" (span ((class "base")) ".") "base"))
                 ,(if sorter? `(div ((class "heading-sorter")) ,(render-sorter)) "")
                 (div ((class "heading-links"))
                      (div ((style "width: 100%; text-align: right"))
                           ,@(map (lambda (x) x) links)))))))




; render website heading for logged in user
(define (render-logged-heading sorter?)
  (render-heading sorter? (map (lambda (x) `(a ((class "heading-link") (href ,(string-append "/" (car x)))) ,(cadr x))) '(("submit" "submit")
                                                                                                                        ("about" "about")
                                                                                                                        ("inbox" "inbox")
                                                                                                                        ("account" "account")
                                                                                                                        ("do-logout" "sign out")))))
; render website heading for new user
(define (render-less-heading sorter?)
  (render-heading sorter? (map (lambda (x) `(a ((class "heading-link") (href ,(string-append "/" (car x)))) ,(cadr x))) '(("submit" "submit")
                                                                                                                        ("about" "about")
                                                                                                                        ("login" "sign in")))))



; render website footer
(define (render-footer)
  `(div ((class "footer"))
             (div ((class "controls"))
                  (a ((class "control-link") (href "prev")) "< prev")
                  (a ((class "control-link") (href "next")) "next >"))))

; redner website sorter
(define (render-sorter)
  '(select (option ((value "hot")) "hot")
           (option ((value "new")) "new")
           (option ((value "top")) "top")))

; consume item x and return X-expr representing data
; item -> X-expr
(define (render-item x)
  `(div ((class "item"))
        (div ((class "heat-level"))
             (div ((class "heat-level-cont"))
                  (div ((class "voters")) (span ((class "voter")) "▼") (span ((class "voter")) "▲")) ,(number->string (- (post-pos x) (post-neg x)))))
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
(define (render-items)
  (let ([items (get-posts db)])
    `(div ((class "items"))
        ,(render-top-items (take items 3))
        ,@(map render-item (cdddr items))
        ,(render-footer))))

; consume a list of three items and return an X-exp representing it
; list of item -> X-expr
(define (render-top-items lat)
  `(div ((class "top-items"))
        (div ((class "top-item"))
             (div ((class "img-crop") (style "height: 252px;")))
             ,(render-item (list-ref lat 0)))
        (div ((class "second-items"))
             (div ((class "img-crop") (style "height: 104px")))
             ,(render-item (list-ref lat 1))
             (div ((class "img-crop") (style "height: 104px")))
             ,(render-item (list-ref lat 2)))))

; consume a title and an X-expr and return a X-expr for a general page
; string X-expr -> X-expr
(define (render-gnr-page r title content)
  (response/xexpr
   #:preamble #"<!doctype html>"
   `(html
     (head
      (title "basketbase")
      (link ((rel "stylesheet") (type "text/css") (href "/static/style.css"))))
     (body        
      ;,(render-heading #f)
      ,(if (user-logged-in? db r) (render-logged-heading #f) (render-less-heading #f))
      ,content))
   
   ))


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
                  (div ((class "datetime-container")) ,(comment-datetime current))))
        
         ,(if [> 4 depth]`(div ((class "comment-replies"))
         ,@(map (lambda (x) (render-comment x (+ 1 depth))) replies))""))))

; consume a list of comments and return a X-expr representing it
(define (render-comments comms)
  `(div ((class "comment-box"))
        ,@(map (lambda (x) (render-comment x 0)) comms)))



; # RECIEVING UPDATES
(define (submit-post r)
  ; Insert post into database
  (post->db db (parse-post (request-bindings r)))
  (redirect-to "/"))


; # PAGE RESPONSE FUNCTIONS

; consume request and return the front page
; request -> X-expr
(define (front-page r)
  (render-gnr-page  r "basketbase - Front Page" (render-items)))

; consume request and return the about page
; request -> X-expr
(define (about-page r)
  (render-gnr-page
   r
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
  (render-gnr-page
   r
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
  (render-gnr-page
   r
   "Post Page"
   `(div ((class "items") (style "padding-top: 35px; padding-bottom: 35px"))
         ,(render-item (pid->db->post db (string->number id)))
         ,(render-comments (pid->db->comms db (string->number id)))
         )))

; consume request and return the submit page
; request -> X-expr
(define (submit-page r)
  (render-gnr-page
   r
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


(define (parse-login-info b)
  (list (extract-binding/single 'username b) (extract-binding/single 'password b)))


; Generate session_id
(define (gen-sid)
  (foldr (lambda (next prev) (string-append prev (number->string next 16))) "" (for/list ((i 32)) (add1 (random 256)))))

;consumes request and produces X-xexpr representing the login page
(define (login-page r)
  (render-gnr-page
   r
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
  (match-let ([(list username password) (parse-login-info (request-bindings r))])
    (let ([curr_user (username->db->user db username)])
      (if curr_user
          (let ([sid (gen-sid)])
            (begin (session->db db (session sid
                                            (user-id curr_user)
                                            (request-client-ip r)
                                            "Mozilla"
                                            "2018-03-10"))
                   (redirect-to "/" #:headers (list (cookie->header (make-id-cookie "sid"
                                                                  (make-secret-salt/file "salt.key")
                                                                  sid))))))
          (redirect-to "login")))))

; consumes request and logs user out
(define (do-logout r)
  (delete-session-db db (request-id-cookie "sid" (make-secret-salt/file "salt.key") r))
  (redirect-to "/" #:headers (list (cookie->header (logout-id-cookie "sid")))))


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
  ; session verification will go here
  (write (request-id-cookie "sid" (make-secret-salt/file "salt.key") r))
  (newline)
  ; dispatch request to right function
  (dispatch r))

; Login required
; consumes function and returns wrapping lambda which verifies request contains valid session information before running function
; function -> function
(define (logreq f)
  (lambda (r) (if (user-logged-in? db r) (f r) (redirect-to "login"))))


;Parse request for session if cookie
(define (parse-session-info r)
  (request-id-cookie "sid" (make-secret-salt/file "salt.key") r))

; consumes request and determines whether request contains valid active session
; request -> bool
(define (user-logged-in? db r)
  (let ([session_id (parse-session-info r)])
    (and session_id (session-exists? db session_id))))

; get the currently logged in user
(define (current-user db r)
  (let ([session_id (parse-session-info r)])
    (id->db->user db
                  (session-uid (sid->db->session db
                                                 session_id)))))


; Request dispatching Table
(define-values (dispatch url)
  (dispatch-rules
   [("") front-page]
   [("about") about-page]
   [("post" (string-arg)) post-page]
   [("submit") (logreq submit-page)]
   [("account") (logreq account-page)]
   [("submit-new-post") (logreq submit-post)]
   [("login") login-page]
   [("do-login") do-login]
   [("do-logout") do-logout]
   [("static" (string-arg)) serve-asset]
   [("test-session") (lambda (r) (cond
                                    [(not (session-exists? db "1234")) (session->db db (session "1234"
                                                                                            1
                                                                                            (request-client-ip r)
                                                                                            "Mozilla"
                                                                                            "2018-03-10"))])
                                  (response/xexpr "TEST SESSION CREATED" #:cookies (list (make-id-cookie "sid"
                                                                                  (make-secret-salt/file "salt.key")
                                                                                  "1234"))))]
                                  
   [else (lambda (x) (response/xexpr "WRONG TURN, BRO"))]))


; Start the server
(serve/servlet start
               #:extra-files-paths (list (build-path (current-directory) "static"))  ; directory for static files
               #:launch-browser? #f
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:port 8080)
