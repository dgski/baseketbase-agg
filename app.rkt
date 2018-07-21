#lang web-server

(require web-server/servlet
         web-server/servlet-env
         racket/port)

(require "views.rkt")
(require "model.rkt")


; # CREATE DATABASE CONNECTION
(define db
  (sqlite3-connect #:database "test-r.db" #:mode 'create))

; # RENDER FUNCTIONS

; render website heading
(define (render-heading sorter?)
  `(div ((class "heading"))
       (div ((class "heading-holder"))
            (div ((class "heading-helper"))
                 (div ((class "heading-text"))
                      (a ((class "heading-link-main") (href "/"))
                         "basket" (span ((class "base")) ".") "base"))
                 ,(if sorter? `(div ((class "heading-sorter")) ,(render-sorter)) "")
                 (div ((class "heading-links"))
                      (div ((style "width: 100%; text-align: right"))
                           (a ((class "heading-link") (href "/submit")) "submit")
                           (a ((class "heading-link") (href "/about")) "about")
                           (a ((class "heading-link") (href "/inbox")) "inbox")
                           (a ((class "heading-link") (href "/account")) "account")
                           (a ((class "heading-link") (href "/signout")) "sign out")))))))

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
  (get-posts db)
  `(div ((class "items"))
        ;,@(map render-item (take ITEMS 3))
        ,@(map render-item (get-posts db))
        ,(render-footer)))

; consume a title and an X-expr and return a X-expr for a general page
; string X-expr -> X-expr
(define (render-gnr-page title content)
  (response/xexpr
   #:preamble #"<!doctype html>"
   `(html
     (head
      (title "basketbase")
      (link ((rel "stylesheet") (type "text/css") (href "/static/style.css"))))
     (body        
      ,(render-heading #f)     
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
                  (div ((class "datetime-container")) ,(comment-datetime current))))
        
         ,(if [> 4 depth]`(div ((class "comment-replies"))
         ,@(map (lambda (x) (render-comment x (+ 1 depth))) replies))""))))
 ; )


; consume a list of comments and return a X-expr representing it
(define (render-comments comms)
  `(div ((class "comment-box"))
        ,@(map (lambda (x) (render-comment x 0)) comms)))



; # RECIEVING UPDATES
(define (submit-post r)
  ; Insert post into database
  (post->db db (parse-post(request-bindings r)))
  (redirect-to "/"))


; # PAGE RESPONSE FUNCTIONS

; consume request and return the front page
; request -> X-expr
(define (front-page r)
  (render-gnr-page "basketbase - Front Page" (render-items)))

; consume request and return the about page
; request -> X-expr
(define (about-page r)
  (render-gnr-page
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
   (string-append "id" "'s Profile")
   `(div ((class "items") (style "margin-bottom: 70px;"))
         (div ((style "padding-top: 25px; text-align: left"))
              (h3 (string-append "Account Information for '" "id" "'"))
              (br)
              "Change email:"(br)(br)
              (input ((class "our-input") (value "fat_papa@gmail.com") (type "email") (name "email")))
              (br)(br)
              "Change profile:"(br)(br)
              (textarea ((width "fill")
                         (placeholder "body")
                         (style "margin-bottom: 30px")
                         (class "our-input submit-input submit-text-area"))
                        "This is the part that you can say a little something about yourself or put a few links to places you care about. If your comment of submission gives someone pause and they want to heard more about you, then they will check out your profile.")
              (br)
              "Change password:"(br)(br)
              (input ((class "our-input") (type "password") (placeholder "old password") (name "old-password")))
              (br)
              (input ((class "our-input") (type "password") (placeholder "new password") (name "new-password-1")))
              (br)
              (input ((class "our-input") (type "password") (placeholder "re-type new password") (name "new-password-2")))
              (br)
              (button ((class "our-button")) "save changes")
              (button ((class "our-button") (style "background-color: brown")) "delete account")))))
              
   
; consume request and return the post being requested along with comments
; request -> X-expr
(define (post-page r id)
  (render-gnr-page
   "Post Page"
   `(div ((class "items") (style "padding-top: 35px; padding-bottom: 35px"))
         ,(render-item (pid->db->post db (string->number id)))
         ,(render-comments (pid->db->comms db (string->number id)))
         )))

; consume request and return the submit page
; request -> X-expr
(define (submit-page r)
  (render-gnr-page
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
(define (start request)
  ; session verification will go here
  (dispatch request))

; Request dispatching Table
(define-values (dispatch url)
  (dispatch-rules
   [("") front-page]
   [("about") about-page]
   [("post" (string-arg)) post-page]
   [("submit") submit-page]
   [("account") account-page]
   [("submit-new-post") submit-post]
   [("static" (string-arg)) serve-asset]
   [else (lambda (x) (response/xexpr "WRONG TURN, BRO"))]))


; Start the server
(serve/servlet start
               #:extra-files-paths (list (build-path (current-directory) "static"))  ; directory for static files
               #:launch-browser? #f
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:port 8080)
