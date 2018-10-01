#lang racket

(require db
         web-server/servlet)

(require "dbconn.rkt"
         "model.rkt"
         "utils.rkt"
         "sessions.rkt"
         "page.rkt"
         "comments.rkt"
         "views.rkt")

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

; consume a list of three items and return an X-exp representing it
; list of item -> X-expr
(define (render-top-posts lat)
  `(div ((class "top-items"))
        (div ((class "top-item"))
             (div ((class "img-crop") (style "height: 232px;")) ,(post-body (car (list-ref lat 0))))
             ,(render-post (list-ref lat 0)))
        (div ((class "second-items"))
             (div ((class "img-crop") (style "height: 74px")) ,(post-body (car (list-ref lat 1))))
             ,(render-post (list-ref lat 1))
             (div ((class "img-crop") (style "height: 74px")) ,(post-body (car (list-ref lat 2))))
             ,(render-post (list-ref lat 2)))))

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




(provide (all-defined-out))
