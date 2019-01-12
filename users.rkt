#lang racket

(require web-server/servlet
         threading)

(require "page.rkt"
         "dbconn.rkt"
         "sessions.rkt"
         "model.rkt"
         "utils.rkt"
         "content.rkt")

; consume request and return the account page
; request -> X-expr
(define (account-page r)
  (page r
        (string-append "id" "'s Profile")
        (let ([u (current-user db r)])
          `(div ((class "items"))
                (div ((class "account-info"))
                     (h3 ,(string-append "Account Information for '" (user-username u) "'"))
                     (form ((action "/update-user-information"))
                           (br)
                           "Change email:"
                           (br)(br)
                           (input ((class "our-input") (value ,(user-email u)) (type "email") (name "email")))
                           (br)(br)
                           "Change profile:"
                           (br)
                           (br)
                           (textarea ((width "fill")
                                      (placeholder "body")
                                      (class "our-input submit-input submit-text-area")
                                      (name "profile"))
                                     ,(user-profile u))
                           (br)(br)
                           "Change password:"
                           (br)(br)
                           (input ((class "our-input") (type "password") (placeholder "new password") (name "new-password-1")))
                           (br)
                           (input ((class "our-input") (type "password") (placeholder "re-type new password") (name "new-password-2")))
                           (br)
                           "Old password to validate changes:"
                           (br)(br)
                           (input ((class "our-input") (type "password") (placeholder "old password") (name "old-password")))
                           (br)
                           (button ((class "our-button standard-btn")) "save changes"))
                     (br)
                     (a ((href "/delete-account"))(button ((class "our-button standard-btn danger-btn")) "delete account")))))))



; consume request and return the inbox page
; request -> x-expression
(define (inbox-page r)
  (let* ([user (current-user db r)]
         [uid (user-id user)]
         [comments (get-inbox-comments db uid)]
         [rendered-comments (render-comments comments #f (if (user-logged-in? db r) user #t) #t)])
    (see-all-inbox-items db uid)
    (page r
          "inbox"

          `(div ((class "items"))
                (div ((class "userpage-holder"))
                     (div ((class "inbox"))
                          (h3 "Inbox")
                          ,@(if (null? comments) `("No replies yet.") rendered-comments)))))))
              
  

;consumes request and produces X-xexpr representing the login page
; request -> x-expression
(define (login-page r)
  (page r
        "Login"
        '(div ((class "items"))
              (div ((class "user-input-page"))
                   (div ((class "login"))
                        (form ((action "do-login"))
                              (input ((class "our-input") (type "text") (placeholder "username") (name "username")))
                              (br)
                              (input ((class "our-input") (type "password") (placeholder "password") (name "password")))
                              (br)
                              (button ((class "our-button")) "continue")))
                   (div ((class "info login-banner"))
                        "Welcome to #.minimal!"
                        (br)(br)
                        "We hope you enjoy being a part of this community!")))))


; consume a request filled with user's updated information, write to database and redirect
; request -> redirect
(define (update-user r)
  (match-let* ([curruser (current-user db r)]
               [(list email
                      profile
                      old-pass
                      new-pass-1
                      new-pass-2) (extract-user-bindings (request-bindings r))]
               [old-password-valid (valid-password? old-pass (user-passhash curruser))]
               [new-passwords-match (equal? new-pass-1 new-pass-2)]
               [email (if (non-empty-string? email) email (user-email curruser))]
               [password (if [and old-password-valid new-passwords-match (non-empty-string? new-pass-1)]
                             (hashpass new-pass-1)
                             (user-passhash curruser))])
    
    (when old-password-valid (user->db! db (user (user-id curruser)
                                                 (user-username curruser)
                                                 email
                                                 profile
                                                 password
                                                 0 ))) ;deleted
    (redirect-to "/account")))

; consume request, return X-expr representing sign-up page
; request -> x-expression
(define (sign-up-page r)
  (let* ([bindings (request-bindings r)]
         [message (check-and-extract-binding 'message bindings)]
         [contents (or message "Sign up today to join our interesting community!")])
    (page r
          "Sign Up"
          `(div ((class "items"))
                (div ((class "user-input-page"))
                     (div ((class "login"))
                          (form ((action "do-signup"))
                                (input ((class "our-input") (type "text") (placeholder "desired username") (name "username")))
                                (br)
                                (input ((class "our-input") (type "password") (placeholder " desired password") (name "password")))
                                (br)
                                (button ((class "our-button")) "continue")))
                     (div ((class "info login-banner")) ,contents))))))
   
; consume request, return page asking whether user wants to delete their account
; request -> x-expression
(define (delete-account r )
  (let ([u (current-user db r)])
    (page r
          "Delete Account"
          `(div ((class "items"))
                (div ((class "userpage-holder"))
                     (h3 ,(string-append "Deleting account '" (user-username u) "'"))
                     (p ((class "our-paragraph"))
                        "Are you sure you want to delete this account?")
                     (a ((href "/account"))(button ((class "our-button standard-btn")) "no"))
                     (a ((href "/do-delete-account"))(button ((class "our-button standard-btn danger-btn")) "yes")))))))

; consume a database connection and a user id, and return the users most recent comments
; db, number -> list
(define (get-recent-user-comments db uid)
  (~> (uid->db->comms db uid)
      ((lambda (x) (if (< (length x) 5) x (take x 5))) _)))

; consume a database connection and a user id, and return the users most recent posts
; db, numbers -> list
(define (get-recent-user-posts r db uid)
  (~> (uid->db->posts db uid)
      ((lambda (x) (if (< (length x) 5) x (take x 5))) _)
      (map (attach-comments-to-post r) _)))

; consume a request, return a page that says user does not exist
; r -> x-expression
(define (no-user-page r)
  (page r
        "User Does Not Exist"
        `(div ((class "items about"))
              (h1 "User Does Not Exist")
              (p "This user either deleted their account or never existed in the first place!"))))

; consume a request, return a page that describes a user account
; request, number -> x-expression
(define (user-page r uid)
  (let ([user (id->db->user db uid)]
        [comments (get-recent-user-comments db uid)]
        [posts (~> (get-recent-user-posts r db uid)
                   (map render-post _))])
    (if (equal? (user-deleted user) 1)
        (no-user-page r)
        (page r
              "User"
              `(div ((class "items"))
                    (div ((class "userpage-holder"))
                     
                         (h3 ,(string-append "Profile for '" (user-username user) "'"))
                         (p ((class "our-paragraph"))
                            ,(if (non-empty-string? (user-profile user)) (user-profile user) "This user has not filled out their profile.")
                            (br)(br)
                            (a ((href ,(string-append "/report-user/" (number->string (user-id user)))))
                               (button ((class "our-button")) "report")))
                     
                         (h3 "Submissions")
                         ,@posts
                         ,(if (null? posts) "This user has not submitted any content yet." "")
                         (div ((class "comment-box-special"))
                              (h3 "Comments")
                              ,@(if (null? comments)
                                    `("This user has not posted any comments yet.")
                                    (render-comments comments #f (if (user-logged-in? db r) (current-user db r) #f))))))))))

; consume a request, return a page that allows reporting of user
; request, number -> x-expression
(define (report-user r uid)
  (let ([u (id->db->user db uid)])
  (page r
        "Reporting User"
        `(div ((class "items about"))
              (h1 ,(string-append "Reporting User '" (user-username u) "'"))
              (p "Please explain below why would like to report this user?")
              (form ((action ,(string-append "/do-report-user/" (number->string uid))))
                    (textarea ((width "fill")
                                      (placeholder "body")
                                      (class "our-input submit-input submit-text-area")
                                      (name "why")))
                    (br)(br)
                    (button ((class "our-button standard-btn")) "report"))))))

; consume a request, report given user
; request, number -> redirect
(define (do-report-user r uid)
  (let* ([bindings (request-bindings r)]
         [why (extract-binding/single 'why bindings)])
  (report-user-db db uid why (current-datetime)))
  (redirect-to "/"))

; consume a reported user and return a x-expression representing it
; reported -> x-expression
(define (render-reported-user r)
  `(tr (td ,(number->string (reported-id r)))
       (td ,(posix->string (reported-datetime r) DEFAULT_DATETIME_FORMAT))
       (td ,(number->string (reported-uid r)))
       (td ((class "why-cell")) ,(reported-why r))))

; consume a request, return page representing list of reported users
(define (reported-users r)
  (let ([reports (get-reported-users db)])
    (response/xexpr
     #:preamble #"<!doctype html>"
     `(html
       (head (title "Reported Users")
                (link ((rel "stylesheet") (type "text/css") (href "/static/style.css"))))
       (body (h1 "Reported Users")
             (table ((class "reports-table"))
              (tr (th "Report Id") (th "DateTime Reported") (th "User Id") (th "Reason"))
              ,@(map render-reported-user reports)))))))


(provide (all-defined-out))