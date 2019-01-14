#lang racket

(require db
         web-server/servlet)

(require "dbconn.rkt"
         "model.rkt"
         "utils.rkt"
         "sessions.rkt"
         "page.rkt")

; Gets the user's vote for post if it exists
; db, request -> vote | boolean
(define (get-user-vote-if-logged-in db r post)
  (if (user-logged-in? db r) (get-post-vote db (user-id (current-user db r)) (post-id post)) #f))

; consume request and return the post being requested along with comments
; request -> x-expression
(define (post-page r id)
  (let* ([render-reply (user-logged-in? db r)]
        [post (pid->db->post db id)]
        [rendered-post (render-post (cons post (get-user-vote-if-logged-in db r post)))])
    (page r
          "post"
          `(div ((class "items"))
                ,rendered-post
                ,(if (equal? (post-body post) "") "" `(div ((class "body-box")) ,(post-body post)))

                (div ((class "user-controls"))
                     "posted by "
                     (a ((class "user-link") (href ,(string-append "/user/" (number->string (post-uid post))))) (b ,(uid->db->string db (post-uid post))))
                     " on "
                     ,(posix->string (post-datetime post) DEFAULT_DATETIME_FORMAT)
                     ,(if (and (user-logged-in? db r) (equal? (user-id (current-user db r)) (post-uid post)))
                          `(a ((class "user-link user-link-float-right") (href ,(string-append "/delete-post?pid=" (number->string (post-id post))))) "delete")
                          ""))
                           
                (div ((class "comment-box"))
                     ,(if (user-logged-in? db r)
                          `(form ((class "reply-box")
                                  (action ,(string-append "/add-comment/" (number->string id))))
                                 (div ((class "reply-box-textarea"))
                                      (textarea ((placeholder "New Commment...")
                                                 (class "our-input submit-input submit-text-area comment-input")
                                                 (name "body"))))
                                 (div ((class "reply-box-button"))
                                      (button ((class "our-button")) "Post"))) ""))
                
                ,@(render-comments (pid->db->hotcomms db id) render-reply (if (user-logged-in? db r) (current-user db r) #f))))))


; consume request, if user is allowed to, delete comment, redirect to front-page
(define (delete-post r)
  (let* ([bindings (request-bindings r)]
         [pid (extract-binding/single 'pid bindings)]
         [currpost (pid->db->post db pid)]
         [curr-uid (user-id (current-user db r))]
         [uid (post-uid currpost)])
    
    (begin (when (= curr-uid uid) (delete-post-db db pid))
           (redirect-to "/"))))

; consume request and return the submit page
; request -> x-expression
(define (submit-page r)
  (page r
   "submit page"
   `(div ((class "items about"))
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

; Consume a request containing new post information and add it to database  if valid
; request -> redirect to "/"
(define (submit-post r)
  (post->db db (parse-post (current-user db r) (request-bindings r)))
  (redirect-to "/"))

; welcome banner constant
; x-expression representing the welcome banner
(define WELCOME-BANNER
  `(div ((class "info"))
        (a ((href "/hide-banner") (class "heading-link info-close")) "x")
        (h3 ((class "welcome-header")) "Welcome to Our Site!")
        "This is a minimal online news aggregator. You can see links that others think are interesting or noteworthy, be part of engaging discussions in the comments section, and submit your own links too!"))

; consume a list of items and return x-expression representing it
; list of item -> x-expression
(define (render-posts posts order start end [render-banner #t])
  `(div ((class "items"))
        ,(if render-banner WELCOME-BANNER "")
        ,@(map render-post posts)
        ,(render-footer order start end (length posts))))

; change vote status in database
; vote, number, number, number, string, function ->
(define (handle-vote-change v dir id uid type new-vote-func)
  (match (list (vote-dir v) dir)
    [(list 1 1)
     (alter-vote type db id "down")]
    [(list 1 0)
     (for ([i 2]) (alter-vote type db id "down"))
     (new-vote-func)]
    [(list 0 1)
     (for ([i 2]) (alter-vote type db id "up"))
     (new-vote-func)]
    [(list 0 0)
     (alter-vote type db id "up")]))

; consume type uid id and dir and return a function that will alter that vote
; number,number, number, number -> function
(define (vote-change type uid id dir)
  (if (equal? type "post")
      (lambda () (vote->db db (vote 0 uid id -1 POST dir)))
      (lambda () (vote->db db (vote 0 uid -1 id COMMENT dir)))))

; consume request return previous page
; request -> redirect
(define (submit-vote r)
  (let* ([bindings (request-bindings r)]
         [type (extract-binding/single 'type bindings)]
         [id (string->number (extract-binding/single 'id bindings))]
         [dir (string->number (extract-binding/single 'dir bindings))]
         [uid (user-id (current-user db r))]
         [v (get-vote type db uid id)])
    
    (if (user-voted type db uid id)
        (begin
          (delete-vote type db uid id)
          (handle-vote-change v dir id uid type (vote-change type uid id dir)))
        (begin
          (create-new-vote type db uid id dir)
          (alter-vote type db id (if (= dir 1) "up" "down"))))
  
    (redirect-to (referer-direct r))))

; consume item p and attach
; request -> function
(define (attach-comments-to-post r)
  (lambda (p)
    (cons p (if (user-logged-in? db r) (get-post-vote db (user-id (current-user db r)) (post-id p)) #f))))

; consume item p and return X-expr representing data
; format (post vote)
; item -> X-expr
(define (render-post p)
  (let* ([post (car p)]
         [pid (post-id post)]
         [score (number->string (post-score post))]
         [url (post-url post)]
         [title (post-title post)]
         [numcom (number->string (post-numcom post))]
         [vote (cdr p)]
         [voters (render-voters "post" pid vote)])
    
    `(div ((class "item"))
          (div ((class "heat-level"))
               (div ((class "heat-level-cont")) ,voters ,score))
          (a ((class "item-link")(href ,url))
             (div ((class "content"))
                  (div ((class "title")) ,title)
                  (div ((class "url-sample")) ,url)))
          (div ((class "comments"))
               (div ((class "comment-container"))   
                    (a ((class "comment-link")
                        (href ,(string-append  "/post/" (number->string pid))))
                       ,(string-append numcom " comments")))))))

; consume a list of comments and return a X-expr representing it
; list, boolean, user, boolean -> x-expression
(define (render-comments comms render-reply u [hilight? #f])
    (map (lambda (x) (render-comment x 0 render-reply u hilight?)) comms))

; consume a user id and return a string representing a link to that user page
; number -> string
(define (create-user-link uid)
  `(a ((class "user-link") (href ,(string-append "/user/" (number->string uid)))) ,(uid->db->string db uid)))

; consume a comment id and return a string representing a reply link to it
; number -> string
(define (create-reply-link render-reply cid pid)
  (if render-reply
      `(a ((class "reply-link") (href ,(string-append "/reply-comment?cid=" (number->string cid) "&pid=" (number->string pid)))) "reply")
      ""))

; consume a comment id and return a string representing a delete link to it
; number -> string
(define (create-delete-link render-reply comment-uid current-uid cid)
  (if (and render-reply (equal? comment-uid current-uid))
      `(a ((class "reply-link") (href ,(string-append "/delete-comment?cid=" (number->string cid)))) "delete")
      ""))

; consume a comment and a depth and return a X-expr representing it and all of it's children
; comment number -> X-expr
(define (render-comment comments-list depth render-reply curr-user [hilight-comments #f]) ; hilighting is for inbox
  (let* ([current (car comments-list)]
         [replies (cadr comments-list)]
         [hilight? (if hilight-comments (caddr comments-list) #f)]
         [cid (comment-id current)]
         [uid (comment-uid current)]
         [pid (comment-pid current)]
         [body (comment-body current)]
         [datetime (posix->string (comment-datetime current) DEFAULT_DATETIME_FORMAT)]
         [voters (render-voters "comment" cid (if curr-user (get-comm-vote db (user-id curr-user) cid) #f))]
         [user-link (create-user-link uid)]
         [reply-link (create-reply-link render-reply cid pid)]
         [delete-link (create-delete-link render-reply uid curr-user cid)])
    
    `(div ((class "comment"))
          (div ((class ,(if hilight? "comment-aligner inbox-unread" "comment-aligner")))
               (div ((class "comment-content"))
                    (div ((class "comment-username")) ,voters ,user-link ,reply-link ,delete-link)
                    (div ((class "comment-body")) ,body))
               (div ((class "comment-datetime"))
                    (div ((class "datetime-container"))
                         (a ((href ,(string-append "/comment/" (number->string cid))) (class "comment-link")) ,datetime))))
          
          ,(if (> 4 depth)
               `(div ((class "comment-replies"))
                     ,@(map (lambda (c)
                              (render-comment c (+ 1 depth) render-reply curr-user)) replies))
               `(div ,@(map (lambda (c)
                              (render-comment c (+ 1 depth) render-reply curr-user)) replies))))))

; consume a request, return a page that allows replying to the given comment
; request -> x-expression
(define (reply-comment r)
  (let* ([bindings (request-bindings r)]
         [pid (extract-binding/single 'pid bindings)]
         [cid (extract-binding/single 'cid bindings)])
    (page r "reply to:" `(div ((class "items info-page"))
                                         (h3 "Replying to:")
                                         ,(render-comment (list (id->db->comment db cid) '()) 0 #f (if (user-logged-in? db r) (current-user db r) #f))
                                         (br)(br)
                                         (form ((class "reply-box")
                                                (action ,(string-append "/add-comment/" pid)))
                                               (div ((class "reply-box-textarea"))
                                                    (textarea ((placeholder "New Commment...")
                                                               (class "our-input submit-input submit-text-area comment-input")
                                                               (name "body")
                                                               (active "")))
                                                    (input ((type "hidden") (name "replyto") (value ,cid))))
                                               (div ((class "reply-box-button"))
                                                    (button ((class "our-button")) "Post")))))))

; consume request, if user is allowed to, delete comment, redirect to front-page
; request -> re-direct
(define (delete-comment r)
  (let* ([bindings (request-bindings r)]
         [cid (extract-binding/single 'cid bindings)]
         [currcomm (id->db->comment db cid)]
         [curr-uid (user-id (current-user db r))]
         [uid (comment-uid currcomm)]
         [dest-url (referer-direct r)])
    
    (begin
      (when (= uid curr-uid) (delete-comment-db db cid))
      (redirect-to dest-url))))

; consume request, pid and return the post page (after adding comment)
; request -> redirect to "/post/pid"
(define (add-comment r pid)
  (let* ([bindings (request-bindings r)]
         [body (extract-binding/single 'body bindings)]
         [replyto (if (exists-binding? 'replyto bindings) (string->number (extract-binding/single 'replyto bindings)) -1)]
         [uid (user-id (current-user db r))]
         [new-comment (comment 0 uid pid 1 0 1 "2018-07-20" body replyto)]
         [post-url (string-append "/post/" (number->string pid))])
  
  (begin (comment->db db new-comment)
         (when (not (= replyto -1))
           (inbox-msg->db db (inbox-msg 0
                                        (comment-uid (id->db->comment db replyto))
                                        (get-most-recent-cid db uid)
                                        0)))
         (inc-comment-db db pid)
         (redirect-to post-url))))

; consume request and cid, return X-expr representing comment page
; request, number -> X-expr
(define (comment-page r cid)
  (let* ([currcomm (id->db->comment db cid)]
         [currpost (pid->db->post db (comment-pid currcomm))]
         [render-reply (user-logged-in? db r)])
    (page r "comment page" `(div ((class "items comment-page"))
                                 ,(render-post (cons currpost (if (user-logged-in? db r) (get-post-vote db (user-id (current-user db r)) (post-id currpost)) #f)))
                                 (div ((class "comment-page-helper"))
                                      (div ((class "comment-page-more"))
                                           (a ((class "comments-back") (href ,(string-append "/post/" (number->string (post-id currpost))))) "< back to post"))
                                      ,@(render-comments (list (list currcomm (get-comment-replies db (comment-id currcomm)))) render-reply (if (user-logged-in? db r) (current-user db r) #f)))))))

(provide (all-defined-out))