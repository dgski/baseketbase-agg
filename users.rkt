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
                           (input ((class "our-input") (type "password") (placeholder "new password") (name "new-password-1")))
                           (br)
                           (input ((class "our-input") (type "password") (placeholder "re-type new password") (name "new-password-2")))
                           (br)
                           "Old password to validate changes:"(br)(br)
                           (input ((class "our-input") (type "password") (placeholder "old password") (name "old-password")))
                           (br)
                           (button ((class "our-button") (style "width: 125px")) "save changes"))
                     (br)
                     (a ((href "/delete-account"))(button ((class "our-button") (style "background-color: brown; width: 125px")) "delete account")))))))



; consume request and return the inbox page
(define (inbox-page r)
  (let* ([user (current-user db r)]
         [uid (user-id user)]
         [comments (get-inbox-comments db uid)])
    (page r
          "inbox"

          `(div ((class "items"))
                (div ((class "userpage-holder"))
                     (div ((class "comment-box") (style "padding-top: 25px;"))
                          (h3 "Inbox")
                          ,@(if (null? comments)
                                `("No replies yet.")
                                (render-comments comments #f (if (user-logged-in? db r) user #t) #t))))))))
              
  

;consumes request and produces X-xexpr representing the login page
(define (login-page r)
  (user->db! db (user 0 "gonzalez" "jose@gonzalez.com" "" (hashpass "1234")))
  (page r
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
               [password (if (and old-password-valid
                                  new-passwords-match
                                  (non-empty-string? new-pass-1))
                             (hashpass new-pass-1)
                             (user-passhash curruser))])
    
    (when old-password-valid (user->db! db (user (user-id curruser)
                                                 (user-username curruser)
                                                 email
                                                 profile
                                                 password)))
    (redirect-to "/account")))

; consume request, return X-expr representing sign-up page
; request -> X-expr
(define (sign-up-page r)
  (let* ([bindings (request-bindings r)]
         [message (check-and-extract-binding 'message bindings)]
         [contents (or message "Sign up today to join our interesting community!")])
    (page r
          "Sign Up"
          `(div ((class "items"))
                (div ((class "top-items"))
                     (div ((class "top-item login") (style "width: 85px"))
                          (form ((action "do-signup"))
                                (input ((class "our-input") (type "text") (placeholder "username") (name "username")))
                                (input ((class "our-input") (type "password") (placeholder "password") (name "password")))
                                (button ((class "our-button")) "continue")))
                     (div ((class "second-items info")) ,contents))))))
   
; consume request, return page asking whether user wants to delete their account
(define (delete-account r )
  (let ([u (current-user db r)])
    (page r
          "Delete Account"
          `(div ((class "items"))
                (div ((class "userpage-holder"))
                     (h3 ,(string-append "Deleting account '" (user-username u) "'"))
                     (p ((class "our-paragraph"))
                        "Are you sure you want to delete this account?")
                     (a ((href "/account"))(button ((class "our-button") (style "width: 125px")) "no"))
                     (a ((href "/delete-account-now"))(button ((class "our-button") (style "background-color: brown; width: 125px")) "yes")))))))

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

; consume a request, return a page that describes a user account
(define (user-page r uid)
  (let ([user (id->db->user db uid)]
        [comments (get-recent-user-comments db uid)]
        [posts (~> (get-recent-user-posts r db uid)
                   (map render-post _))])
    (page r
          "User"
          `(div ((class "items"))
                (div ((class "userpage-holder"))
                     
                     (h3 ,(string-append "Profile for '" (user-username user) "'"))
                     (p ((class "our-paragraph"))
                        ,(if (non-empty-string? (user-profile user)) (user-profile user) "This user has not filled out their profile.")
                        (br)
                        (br)
                        (a ((href "/report"))
                           (button ((class "our-button")) "report")))
                     
                     (h3 "Submissions")
                     ,@posts
                     ,(if (null? posts) "This user has not submitted any content yet." "")
                     (div ((class "comment-box") (style "padding-top: 25px;"))
                          (h3 "Comments")
                          ,@(if (null? comments)
                                `("This user has not posted any comments yet.")
                                (render-comments comments #f (if (user-logged-in? db r) (current-user db r) #f)))))))))

(provide (all-defined-out))