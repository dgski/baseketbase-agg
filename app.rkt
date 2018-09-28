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
(require "utils.rkt")
(require "dbconn.rkt")
(require "comments.rkt")
(require "posts.rkt")
(require "page.rkt")

; # SETUP CRYPTOGRAPHY
(setup-crypto)

; # RECIEVING UPDATES





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
         [start (string->number (or (check-and-extract-binding 'start bindings) "0"))]
         [end (string->number (or (check-and-extract-binding 'end bindings) (number->string POSTS_PER_PAGE)))]
         [u (if (user-logged-in? db r) (current-user db r) #f)]
         [cookies (request-cookies r)])
    (write cookies) (newline)
    (render-gnr-page
     #:order order
     #:sorter
     #t
     r
     "basketbase - Front Page"
     (render-posts
      (map (lambda (x)
             (cons x (if (user-logged-in? db r) (get-post-vote db (user-id (current-user db r)) (post-id x)) #f)))
           (get-sorted-posts db order start end))
      order
      start
      end
      (not (or (user-logged-in? db r) (cookie-exists? cookies "no-banner")))))))

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
                   "Welcome to #.minimal!"
                   (br)(br)
                   "We hope you enjoy being a part of this community!")))))



; consumes a test and a redirection destination and returns a wrapping function which checks test, and if not valid redirects to destination
; cond,string -> function
(define (page-gate-keeper-factory test redirect-dest)
  (lambda (f)
    (lambda args (if test (apply f args) (redirect-to redirect-dest)))))

;(define logreq (page-gate-keeper-factory 



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
                                   (a ((href "/report"))
                                      (button ((class "our-button")) "report")))
                                (h3 "Submissions")
                                ,@(map render-post posts)
                                ,(if (null? posts) "This user has not submitted any content yet." "")
                                (div ((class "comment-box") (style "padding-top: 25px;"))
                                     (h3 "Comments")
                                     ,@(if (null? comments) `("This user has not posted any comments yet.") (render-comments comments #f (if (user-logged-in? db r) (current-user db r) #f)))))))))



; consume request, return X-expr representing sign-up page
; request -> X-expr
(define (sign-up-page r)
  (let* ([bindings (request-bindings r)]
         [message (check-and-extract-binding 'message bindings)])
    (render-gnr-page r
                     "Sign Up"
                     `(div ((class "items"))
                           (div ((class "top-items"))
                                (div ((class "top-item login") (style "width: 85px"))
                                     (form ((action "do-signup"))
                                           (input ((class "our-input") (type "text") (placeholder "username") (name "username")))
                                           (input ((class "our-input") (type "password") (placeholder "password") (name "password")))
                                           (button ((class "our-button")) "continue")))
                                ,(let ([contents (or message "Sign up today to join our interesting community!")])
                                   `(div ((class "second-items info")) ,contents)))))))
   

; consume request, return page asking whether user wants to delete their account
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





; consume a request and return a redirect with a new cookie header to prevent banner display
; request -> redirect
(define (hide-banner r)
  (redirect-to "/" #:headers (list (cookie->header (make-cookie "no-banner" "true")))))


; # STATIC FILE SERVING

; consume request and filename and send that file back to use
; request -> X-expr
; Future : make sure that MIME type is correct!!!!!
(define (serve-asset r f)
  (response 200 #"OK" 0 #"text/css" empty (lambda (op)
                                            (with-input-from-file (string-append "static/" f)
                                              (lambda () (copy-port (current-input-port) op))))))



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
   [("signup") (nonlogreq sign-up-page)]
   [("do-signup") (nonlogreq do-signup)]
   [("hide-banner") (nonlogreq hide-banner)]

   ; Utilities
   [("static" (string-arg)) serve-asset]

   [else page-not-found]))


; Start the server
(serve/servlet start
               #:extra-files-paths (list (build-path (current-directory) "static"))  ; directory for static files
               #:launch-browser? #f
               #:servlet-path "/"
               #:servlet-regexp #rx""
               ;#:listen-ip "0.0.0.0"
               #:port 8080)


