#lang web-server

; # REQUIRE MODULES
(require web-server/servlet
         web-server/servlet-env
         racket/port
         racket/date
         threading)

; # REQUIRE LOCAL
(require "views.rkt")
(require "model.rkt")
(require "sessions.rkt")


; # CREATE DATABASE CONNECTION
(define db
  (sqlite3-connect #:database "baseketbase.db" #:mode 'create))

; # SETUP CRYPTOGRAPHY
(setup-crypto)

; # RENDER FUNCTIONS

; consume a comment and a depth and return a X-expr representing it and all of it's children
; comment number -> X-expr
(define (render-comment x depth render-reply u)
  (let ([current (car x)]
        [replies (cadr x)])
    `(div ((class"comment" ))
          (div ((class "comment-aligner"))
               (div ((class "comment-content"))
                    (div ((class "comment-username")) ,(render-voters "comment" (comment-id current) (if u (get-comm-vote db (user-id u) (comment-id current)) #f))
                         (a ((class "user-link") (href ,(string-append "/user/" (number->string (comment-uid current)))))
                            ,(uid->db->string db (comment-uid current)))
                         
                         ,(if render-reply `(a ((class "reply-link") (href ,(string-append "/reply-comment?cid="
                                                                                           (number->string (comment-id current))
                                                                                           "&pid="
                                                                                           (number->string (comment-pid current))))) "reply") "")

                         ,(if (and render-reply (= (comment-uid current) (user-id u)))
                              `(a ((style "padding-left: 0px") (class "reply-link") (href ,(string-append "/delete-comment?cid="
                                                                                                          (number->string (comment-id current))))) "delete") ""))
                    (div ((class "comment-body")) ,(comment-body current)))
               (div ((class "comment-datetime"))
                    (div ((class "datetime-container"))
                         (a ((href ,(string-append "/comment/" (number->string (comment-id current)))) (class "comment-link"))
                            ,(date->string (seconds->date (comment-datetime current)) #t)))))
        
          ,(if [> 4 depth] `(div ((class "comment-replies"))
                                 ,@(map (lambda (x) (render-comment x (+ 1 depth) render-reply u)) replies))
               `(div ,@(map (lambda (x) (render-comment x (+ 1 depth) render-reply u)) replies) )))))

; consume a list of comments and return a X-expr representing it
(define (render-comments comms render-reply u)
    (map (lambda (x) (render-comment x 0 render-reply u)) comms))

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

; # RECIEVING UPDATES

; Consume a request containing new post information and add it to database  if valid
; request -> redirect to "/"
(define (submit-post r)
  (post->db db (parse-post (current-user db r) (request-bindings r)))
  (redirect-to "/"))

; consume request, pid and return the post page (after adding comment)
; request -> redirect to "/post/pid"
(define (add-comment r pid)
  (let* ([bindings (request-bindings r)]
         [body (extract-binding/single 'body bindings)]
         [replyto (if (exists-binding? 'replyto bindings) (string->number (extract-binding/single 'replyto bindings)) -1)])
    (comment->db db (comment 0
                             (user-id (current-user db r))
                             pid
                             1
                             0
                             1
                             "2018-07-20"
                             body
                             replyto)))
  (inc-comment-db db pid) ; Increment number of comments
  (redirect-to (string-append "/post/" (number->string pid))))

; change vote status in database
(define (handle-vote-change v dir id uid alter-vote new-vote-func)
  (match (list (vote-dir v) dir)
    [(list 1 "up")
     (alter-vote db id "down")]
    [(list 1 "down")
     (for ([i 2]) (alter-vote db id "down"))
     (new-vote-func)]
    [(list 0 "up")
     (for ([i 2]) (alter-vote db id "up"))
     (new-vote-func)]
    [(list 0 "down")
     (alter-vote db id "up")]))

; consume request return previous page
(define (submit-vote r)
  (let* ([bindings (request-bindings r)]
         [type (extract-binding/single 'type bindings)]
         [id (string->number (extract-binding/single 'id bindings))]
         [dir (extract-binding/single 'dir bindings)]
         [uid (user-id (current-user db r))])
    (cond
      [(equal? type "post")
       (if (user-voted-on-post db uid id)
           (let ([v (get-post-vote db uid id)])
             (pid-delete-vote db uid id)
             (handle-vote-change v dir id uid alter-post-vote (lambda () (vote->db db (vote 0 uid id -1 POST (if (equal? dir "up") UP DOWN))))))
           (begin (vote->db db (vote 0 uid id -1 POST (if (equal? dir "up") UP DOWN))) ; New Vote
                  (alter-post-vote db id dir)))]
      [else
       (if (user-voted-on-comm? db uid id)
           (let ([v (get-comm-vote db uid id)])
             (cid-delete-vote db uid id)
             (handle-vote-change v dir id uid alter-comm-vote (lambda () (vote->db db (vote 0 uid -1 id COMMENT (if (equal? dir "up") UP DOWN))))))
           (begin (vote->db db (vote 0 uid -1 id COMMENT (if (equal? dir "up") UP DOWN))) ; New Vote
                  (alter-post-vote db id dir)))]))
  (redirect-to (bytes->string/utf-8 (header-value (headers-assq #"Referer" (request-headers/raw r))))))

; Consume HTTP bindings and return a list of user-specific bindings
; bindings -> list
(define (extract-user-bindings bindings)
  (for/list ([b '(email profile old-password new-password-1 new-password-2)])
    (extract-binding/single b bindings)))

; consume a request filled with user's updated information, write to database and redirect
; request -> redirect
(define (update-user r)
  (match-let* ([curruser (current-user db r)]
               [(list email profile old-pass new-pass-1 new-pass-2) (extract-user-bindings (request-bindings r))])
    (user->db! db (user (user-id curruser)
                        (user-username curruser)
                        (if (non-empty-string? email) email (user-email curruser))
                        profile
                        (if (and (valid-password? old-pass (user-passhash curruser))
                                 (equal? new-pass-1 new-pass-2))
                            (hashpass new-pass-1)
                            (user-passhash curruser)))))
  (redirect-to "/account"))
         
;consumes request and logs user in
(define (do-login r)
  (attempt-user-login r db))

; consumes request and logs user out
(define (do-logout r)
  (attempt-user-logout r db))

; # PAGE RESPONSE FUNCTIONS

; consume request and return the 'not found'
; request -> X-expr
(define (page-not-found r)
  (render-gnr-page r
   "Not Found"
   `(div ((class "items") (style "text-align: left;padding-top: 25px;"))
         (h3 "Wrong Turn, Bro!")
         (p ((style "line-height: 1.5em"))
            "The page you requested does not exist..."))))

; consume request and return the front page
; request -> X-expr
(define (front-page r)
  (let* ([bindings (request-bindings r)]
         [order (if (exists-binding? 'sort bindings) (extract-binding/single 'sort bindings) "hot")]
         [u (if (user-logged-in? db r) (current-user db r) #f)])
    (render-gnr-page  #:order order #:sorter #t r "basketbase - Front Page" (render-posts
                                                                             (map (lambda (x)
                                                                                    (cons x (if (user-logged-in? db r) (get-post-vote db (user-id (current-user db r)) (post-id x)) #f))) (get-sorted-posts db order)) order))))

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
              (form ((action "/update-user-information"))
                    (br)
                    "Change email:"(br)(br)
                    (input ((class "our-input") (value ,(user-email u)) (type "email") (name "email")))
                    (br)(br)
                    "Change profile:"(br)(br)
                    (textarea ((width "fill")
                               (placeholder "body")
                               (style "margin-bottom: 30px")
                               (class "our-input submit-input submit-text-area")
                               (name "profile"))
                               ,(user-profile u))
                    (br)
                    "Change password:"(br)(br)
                    (input ((class "our-input") (type "password") (placeholder "old password") (name "old-password")))
                    (br)
                    (input ((class "our-input") (type "password") (placeholder "new password") (name "new-password-1")))
                    (br)
                    (input ((class "our-input") (type "password") (placeholder "re-type new password") (name "new-password-2")))
                    (br)
                    (button ((class "our-button") (style "width: 125px")) "save changes"))(br)
              (a ((href "/delete-account"))(button ((class "our-button") (style "background-color: brown; width: 125px")) "delete account")))))))
              
   
; consume request and return the post being requested along with comments
; request -> X-expr
(define (post-page r id)
  (let ([render-reply (user-logged-in? db r)]
        [post (pid->db->post db id)])
    (render-gnr-page r
                     "Post Page"
                     `(div ((class "items") (style "padding-top: 35px; padding-bottom: 35px"))
                           ,(render-post (cons post (if (user-logged-in? db r) (get-post-vote db (user-id (current-user db r)) (post-id post)) #f)))
                           ,(if (equal? (post-body post) "") "" `(div ((class "body-box")) ,(post-body post)))

                           (div ((style "text-align: left; border-top-width: 1px; border-top-color: gainsboro; border-top-style: solid; padding: 5px; margin-top: 10px; font-size: 12px; color: #858cac"))
                                "posted by "
                                (a ((class "user-link") (href ,(string-append "/user/" (number->string (post-uid post))))) (b ,(uid->db->string db (post-uid post))))
                                " on "
                                ,(date->string (seconds->date (post-datetime post)) #t)
                                
                                ,(if (and (user-logged-in? db r) (equal? (user-id (current-user db r)) (post-uid post)))
                                     `(a ((class "user-link") (style "float: right") (href ,(string-append "/delete-post?pid=" (number->string (post-id post))))) "delete")
                                     ""))
                           
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
                           ,@(render-comments (pid->db->hotcomms db id) render-reply (if (user-logged-in? db r) (current-user db r) #f))))))

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

; Login required
; consumes function and returns wrapping lambda which verifies request contains valid session information before running function
; function -> function
(define (logreq f)
  (lambda args (if (user-logged-in? db (car args)) (apply f args) (redirect-to "login"))))

; consume a request, return a page that allows replying to the given comment
(define (reply-comment r)
  (let* ([bindings (request-bindings r)]
         [pid (extract-binding/single 'pid bindings)]
         [cid (extract-binding/single 'cid bindings)])
    (render-gnr-page r "Reply to:" `(div ((class "items") (style "text-align: left;padding-top: 25px;"))
                                         (h3 "Replying to:")
                                         ,(render-comment (list(id->db->comment db cid) '()) 0 #f (if (user-logged-in? db r) (current-user db r) #f))
                                         (br)(br)
                                         (form ((class "reply-box")
                                                (action ,(string-append "/add-comment/" pid)))
                                               (div ((class "reply-box-textarea"))
                                                    (textarea ((placeholder "New Commment...")
                                                               (class "our-input submit-input submit-text-area")
                                                               (style "height: 50px;")
                                                               (name "body")
                                                               (active "")))
                                                    (input ((type "hidden") (name "replyto") (value ,cid))))
                                               (div ((class "reply-box-button"))
                                                    (button ((class "our-button") (style "margin-right: 0px;")) "Post")))))))

; consume a request, return a page that describes a user account
(define (user-page r id)
  (let ([user (id->db->user db id)]
        [comments (~> (uid->db->comms db id)
                      ((lambda (x) (if (< (length x) 5) x (take x 5))) _))]
        [posts (~> (uid->db->posts db id)
                   ((lambda (x) (if (< (length x) 5) x (take x 5))) _)
                   (map (lambda (x)
                          (cons x (if (user-logged-in? db r)
                                      (get-post-vote db (user-id (current-user db r)) (post-id x))
                                      #f))) _))])
    (render-gnr-page r
                     "User"
                     `(div ((class "items"))
                           (div ((class "userpage-holder"))
                                (h3 ,(string-append "Profile for '" (user-username user) "'"))
                                (p ((class "our-paragraph"))
                                   ,(if (non-empty-string? (user-profile user))
                                        (user-profile user)
                                        "This user has not filled out their profile.")
                                   (br)
                                   (br)
                                   #|(a ((href "/message"))
                                      (button ((class "our-button")) "send a message"))|#
                                   (a ((href "/report"))
                                      (button ((class "our-button")) "report")))
                                (h3 "Submissions")
                                ,@(map render-post posts)
                                ,(if (null? posts) "This user has not submitted any content yet." "")
                               (div ((class "comment-box") (style "padding-top: 25px;"))
                                     (h3 "Comments")
                                     ,@(if (null? comments) `("This user has not posted any comments yet.") (render-comments comments #f (if (user-logged-in? db r) (current-user db r) #f)))))))))

; consume request and cid, return X-expr representing comment page
; request, int -> X-expr
(define (comment-page r cid)
  (let* ([currcomm (id->db->comment db cid)]
         [currpost (pid->db->post db (comment-pid currcomm))]
         [render-reply (user-logged-in? db r)])
    (render-gnr-page r
                     "Comment Page"
                     `(div ((class "items") (style "padding-top: 35px; padding-bottom: 35px"))
                           ,(render-post (cons currpost (if (user-logged-in? db r) (get-post-vote db (user-id (current-user db r)) (post-id currpost)) #f)))
                           (div ((style "padding-top: 50px"))
                                (div ((style "padding-bottom: 20px; text-align: left"))
                                     (a ((class "comments-back") (href ,(string-append "/post/" (number->string (post-id currpost))))) "< back to post"))
                                ,@(render-comments (list (list currcomm (get-comment-replies db (comment-id currcomm)))) render-reply (if (user-logged-in? db r) (current-user db r) #f)))))))

; consume request, return page asking whether user wants to delete their account
;
(define (delete-account r )
  (let ([u (current-user db r)])
  (render-gnr-page r "Delete Account"
                   `(div ((class "items"))
                         (div ((class "userpage-holder"))
                              (h3 ,(string-append "Deleting account '" (user-username u) "'"))
                              (p ((class "our-paragraph"))
                                 "Are you sure you want to delete this account?")
                              (a ((href "/account"))(button ((class "our-button") (style "width: 125px")) "no"))
                              (a ((href "/delete-account-now"))(button ((class "our-button") (style "background-color: brown; width: 125px")) "yes")))))))

(define (delete-comment r)
  (let* ([bindings (request-bindings r)]
         [cid (extract-binding/single 'cid bindings)]
         [currcomm (id->db->comment db cid)])
    (begin
      (when (= (user-id (current-user db r)) (comment-uid currcomm)) (delete-comment-db db cid))
      (redirect-to (bytes->string/utf-8 (header-value (headers-assq #"Referer" (request-headers/raw r))))))))


(define (delete-post r)
  (let* ([bindings (request-bindings r)]
         [pid (extract-binding/single 'pid bindings)]
         [currpost (pid->db->post db pid)])
    (begin
      (when (= (user-id (current-user db r)) (post-uid currpost)) (delete-post-db db pid))
      (redirect-to "/"))))




; # STATIC FILE SERVING

; consume request and filename and send that file back to use
; request -> X-expr
; Future : make sure that MIME type is correct!!!!!
(define (serve-asset r f)
 (response 200 #"OK" 0 #"text/css" empty (lambda (op)
                                           (with-input-from-file (string-append "static/" f)
                                             (lambda () (copy-port (current-input-port) op))))))


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
   [("user" (integer-arg)) user-page]
   [("comment" (integer-arg)) comment-page]
   [("delete-account") (logreq delete-account)]

   ; Receiving Data
   [("submit") (logreq submit-page)]
   [("add-comment" (integer-arg)) (logreq add-comment)]
   [("vote") (logreq submit-vote)]
   [("reply-comment") (logreq reply-comment)]
   [("update-user-information") (logreq update-user)]
   [("delete-comment") (logreq delete-comment)]
   [("delete-post") (logreq delete-post)]

   ; Login management
   [("login") login-page]
   [("do-login") do-login]
   [("do-logout") do-logout]

   ; Utilities
   [("static" (string-arg)) serve-asset]

   [else page-not-found]))


; Start the server
(serve/servlet start
               #:extra-files-paths (list (build-path (current-directory) "static"))  ; directory for static files
               #:launch-browser? #f
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:port 8080)
