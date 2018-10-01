#lang racket

(require db
         web-server/servlet)

(require "dbconn.rkt"
         "model.rkt"
         "utils.rkt"
         "sessions.rkt"
         "page.rkt"
         "views.rkt")

; consume a list of comments and return a X-expr representing it
(define (render-comments comms render-reply u)
    (map (lambda (x) (render-comment x 0 render-reply u)) comms))

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
                            ;,(date->string (seconds->date (comment-datetime current)) #t)
                            ,(posix->string (comment-datetime current) DEFAULT_DATETIME_FORMAT)
                            ))))
        
          ,(if [> 4 depth] `(div ((class "comment-replies"))
                                 ,@(map (lambda (x) (render-comment x (+ 1 depth) render-reply u)) replies))
               `(div ,@(map (lambda (x) (render-comment x (+ 1 depth) render-reply u)) replies) )))))


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
    (render-gnr-page r
                     "Comment Page"
                     `(div ((class "items") (style "padding-top: 35px; padding-bottom: 35px"))
                           ,(render-post (cons currpost (if (user-logged-in? db r) (get-post-vote db (user-id (current-user db r)) (post-id currpost)) #f)))
                           (div ((style "padding-top: 50px"))
                                (div ((style "padding-bottom: 20px; text-align: left"))
                                     (a ((class "comments-back") (href ,(string-append "/post/" (number->string (post-id currpost))))) "< back to post"))
                                ,@(render-comments (list (list currcomm (get-comment-replies db (comment-id currcomm)))) render-reply (if (user-logged-in? db r) (current-user db r) #f)))))))


(provide (all-defined-out))
