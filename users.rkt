#lang racket

(require web-server/servlet
         threading)

(require "page.rkt"
         "dbconn.rkt"
         "sessions.rkt"
         "model.rkt"
         "utils.rkt"
         "views.rkt"
         "comments.rkt")

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

(provide (all-defined-out))
