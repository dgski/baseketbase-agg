#lang web-server

; # REQUIRE MODULES
(require web-server/servlet
         web-server/servlet-env
         racket/port
         web-server/dispatchers/dispatch-log)

; # REQUIRE LOCAL
(require "model.rkt"
         "sessions.rkt"
         "utils.rkt"
         "dbconn.rkt"
         "content.rkt"
         "page.rkt"
         "users.rkt")

; # SETUP CRYPTOGRAPHY
(setup-crypto)

; # SAVE CURRENT DIRECTORY
(define curr-dir (current-directory))

; # FRONT PAGE
; request -> x-expression
(define (front-page r)
  (let* ([bindings (request-bindings r)]
         [order (or (check-and-extract-binding 'sort bindings) "hot")]
         [start (string->number (or (check-and-extract-binding 'start bindings) "0"))]
         [end (string->number (or (check-and-extract-binding 'end bindings) (number->string POSTS_PER_PAGE)))]
         [cookies (request-cookies r)]
         [render-banner? (not (or (user-logged-in? db r) (cookie-exists? cookies "no-banner")))]
         [content (map (attach-comments-to-post r) (get-sorted-posts db order start end))])
    
    (page #:order order
          #:sorter #t
          r
          "front page"
          (render-posts content order start end render-banner?))))

; # REQUEST DISPATCHING

; Consume request and return the right thing
; request -> x-expression
(define (start r)
  (begin
    (log-request r curr-dir)
    (dispatch r)))

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
   [("inbox") (logreq inbox-page)]
   
   ; Receiving Data
   [("submit") (logreq submit-page)]
   [("add-comment" (integer-arg)) (logreq add-comment)]
   [("vote") (logreq submit-vote)]
   [("reply-comment") (logreq reply-comment)]
   [("update-user-information") (logreq update-user)]
   [("delete-comment") (logreq delete-comment)]
   [("delete-post") (logreq delete-post)]

   ; Login management
   [("login") (nonlogreq login-page)]
   [("do-login") (nonlogreq attempt-user-login)]
   [("do-logout") (logreq attempt-user-logout)]
   [("signup") (nonlogreq sign-up-page)]
   [("do-signup") (nonlogreq attempt-user-signup)]
   [("delete-account") (logreq delete-account)]
   [("do-delete-account") (logreq attempt-user-delete)]
   [("hide-banner") (nonlogreq hide-banner)]
   [("report-user" (integer-arg)) (logreq report-user)]
   [("do-report-user" (integer-arg)) (logreq do-report-user)]

   ; Administration
   [("reported") reported-users]
   
   ; Utilities
   [("static" (string-arg)) serve-asset]

   ; Page Not Found
   [else page-not-found]))


; Get command line arguments
(define args (current-command-line-arguments))
(when (< (vector-length args) 2)
  (begin (displayln "Usage: app.rkt <DATABASE_FILE> <IP> <PORT>")
         (exit)))

; Define Port and Ip
(define ip (string->number (vector-ref args 1)))
(define port (string->number (vector-ref args 2)))

; Announce Start of App
(displayln "Starting App...")

; Start the server
(serve/servlet start
               #:extra-files-paths (list (build-path (current-directory) "static"))  ; directory for static files
               #:launch-browser? #f
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:listen-ip ip
               #:port port)
