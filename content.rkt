#lang racket

(require db
         web-server/servlet)

(require "dbconn.rkt"
         "model.rkt"
         "utils.rkt"
         "sessions.rkt"
         "page.rkt")

; consume request and return the post being requested along with comments
; request -> X-expr
(define (post-page r id)
  (let ([render-reply (user-logged-in? db r)]
        [post (pid->db->post db id)])
    (page r
          "Post Page"
          `(div ((class "items") (style "padding-top: 35px; padding-bottom: 35px"))
                ,(render-post (cons post (if (user-logged-in? db r) (get-post-vote db (user-id (current-user db r)) (post-id post)) #f)))
                ,(if (equal? (post-body post) "") "" `(div ((class "body-box")) ,(post-body post)))

                (div ((style "text-align: left; padding: 5px; margin-top: 10px; font-size: 12px; color: #858cac"))
                     "posted by "
                     (a ((class "user-link") (href ,(string-append "/user/" (number->string (post-uid post))))) (b ,(uid->db->string db (post-uid post))))
                     " on "
                     ,(posix->string (post-datetime post) DEFAULT_DATETIME_FORMAT)
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


; consume request, if user is allowed to, delete comment, redirect to front-page
(define (delete-post r)
  (let* ([bindings (request-bindings r)]
         [pid (extract-binding/single 'pid bindings)]
         [currpost (pid->db->post db pid)])
    (begin
      (when (= (user-id (current-user db r)) (post-uid currpost)) (delete-post-db db pid))
      (redirect-to "/"))))

; consume request and return the submit page
; request -> X-expr
(define (submit-page r)
  (page r
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


; Consume a request containing new post information and add it to database  if valid
; request -> redirect to "/"
(define (submit-post r)
  (post->db db (parse-post (current-user db r) (request-bindings r)))
  (redirect-to "/"))


; consume a list of items and return X-expr representing it
; list of item -> X-expr
(define (render-posts posts order start end [render-banner #t])
  `(div
    ;(div ((class "top-items-helper"))
    ;,(render-top-posts (take posts 3)))
    (div ((class "items") (style "margin-top: 40px;"))
         ,(if render-banner `(div ((class "info") (style "margin-bottom: 40px; background: linear-gradient(black,navy); color: white;"))
              (a ((href "/hide-banner") (class "heading-link") (style "float: right; margin-top: -20px; margin-right: -15px; font-size: 16px;")) "x")
              (h3 ((style "margin-top: 0px;")) "Welcome to Our Site!")
              "This is a minimal online news aggregator. You can see links that others think are interesting or noteworthy, be part of engaging discussions in the comments section, and submit your own links too!") "")
        ,@(map render-post #|(cdddr posts)|# posts)
        ,(render-footer order start end (length posts)))))

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


; consume item x and return X-expr representing data
; item -> X-expr
(define (render-post x)
  (let* ([post (car x)]
         [pid (post-id post)]
         [score (number->string (post-score post))]
         [url (post-url post)]
         [title (post-title post)]
         [numcom (number->string (post-numcom post))]
         [vote (cdr x)]
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
(define (render-comments comms render-reply u)
    (map (lambda (x) (render-comment x 0 render-reply u)) comms))

; consume a user id and return a string representing a link to that user page
; number -> string
(define (create-user-link uid)
  `(a ((class "user-link") (href ,(string-append "/user/" (number->string uid)))) ,(uid->db->string db uid)))

; consume a comment id and return a string representing a reply link to it
; number -> string
(define (create-reply-link render-reply cid)
  (if render-reply
      `(a ((class "reply-link") (href ,(string-append "/reply-comment?cid=" (number->string cid) "&pid=" (number->string cid)))) "reply")
      ""))

; consume a comment id and return a string representing a delete link to it
; number -> string
(define (create-delete-link render-reply comment-uid current-uid cid)
  (if (and render-reply (equal? comment-uid current-uid))
      `(a ((style "padding-left: 0px") (class "reply-link") (href ,(string-append "/delete-comment?cid=" (number->string cid)))) "delete")
      ""))

; consume a comment and a depth and return a X-expr representing it and all of it's children
; comment number -> X-expr
(define (render-comment comments-list depth render-reply curr-user)
  (let* ([current (car comments-list)]
         [replies (cadr comments-list)]
         [cid (comment-id current)]
         [uid (comment-uid current)]
         [body (comment-body current)]
         [datetime (posix->string (comment-datetime current) DEFAULT_DATETIME_FORMAT)]
         [voters (render-voters "comment" cid (if curr-user (get-comm-vote db (user-id curr-user) cid) #f))]
         [user-link (create-user-link uid)]
         [reply-link (create-reply-link render-reply cid)]
         [delete-link (create-delete-link render-reply uid curr-user cid)])

    `(div ((class "comment"))
          (div ((class "comment-aligner"))
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
(define (reply-comment r)
  (let* ([bindings (request-bindings r)]
         [pid (extract-binding/single 'pid bindings)]
         [cid (extract-binding/single 'cid bindings)])
    (page r "Reply to:" `(div ((class "items") (style "text-align: left;padding-top: 25px;"))
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


; consume request, if user is allowed to, delete comment, redirect to front-page
(define (delete-comment r)
  (let* ([bindings (request-bindings r)]
         [cid (extract-binding/single 'cid bindings)]
         [currcomm (id->db->comment db cid)])
    (begin
      (when (= (user-id (current-user db r)) (comment-uid currcomm)) (delete-comment-db db cid))
      (redirect-to (bytes->string/utf-8 (header-value (headers-assq #"Referer" (request-headers/raw r))))))))



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

; consume request and cid, return X-expr representing comment page
; request, int -> X-expr
(define (comment-page r cid)
  (let* ([currcomm (id->db->comment db cid)]
         [currpost (pid->db->post db (comment-pid currcomm))]
         [render-reply (user-logged-in? db r)])
    (page r "Comment Page" `(div ((class "items") (style "padding-top: 35px; padding-bottom: 35px"))
                                 ,(render-post (cons currpost (if (user-logged-in? db r) (get-post-vote db (user-id (current-user db r)) (post-id currpost)) #f)))
                                 (div ((style "padding-top: 50px"))
                                      (div ((style "padding-bottom: 20px; text-align: left"))
                                           (a ((class "comments-back") (href ,(string-append "/post/" (number->string (post-id currpost))))) "< back to post"))
                                      ,@(render-comments (list (list currcomm (get-comment-replies db (comment-id currcomm)))) render-reply (if (user-logged-in? db r) (current-user db r) #f)))))))

(provide (all-defined-out))