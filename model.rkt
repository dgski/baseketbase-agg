#lang racket

; This file provides the functions used to interact with the data model and database

(require db
         web-server/servlet
         threading)

(require "utils.rkt")


; # CONSTANTS
(define POST_DECAY_RATE (expt 0.5 (/ 1 86400)))
(define 2WEEKS 1209600)
(define POSTS_PER_PAGE 15)


; # UTILITY FUNCTIONS

; consumes lat of pairs and return a string representing a column
; list of lists -> string
(define (create-col lat)
  (cond
    ((null? lat) "")
    (else
     (string-append (car (car lat)) 
                    " " 
                    (car (cdr (car lat))) 
                    ((lambda (lat) (if (null? lat) "" ", ")) (cdr lat)) 
                    (create-col (cdr lat))))))

; consumes string and list or lists and returns a string representing a table
; string, list -> string
(define create-table
  (lambda (name lat)
    (string-append "CREATE TABLE " name " (" (create-col lat) ");")))


; # POSTS

; Post convenience struct
(struct post (id uid pos neg score datetime title url body numcom section))

; Consume request bindings and return post
; bindings -> post
(define (parse-post u b)
  (post 0
        (user-id u)
        1
        0
        1
        (current-datetime)
        (extract-binding/single 'title b)
        (extract-binding/single 'url b)
        (extract-binding/single 'body b)
        0
        "front"))

; consume database and post add to db
; db, post -> into db
(define (post->db db x)
  (query-exec db "INSERT INTO posts (uid, pos, neg, score, datetime, title, url, body, numcom, section) VALUES (?,?,?,?,?,?,?,?,?,?);"            
              (post-uid x)
              (post-pos x)
              (post-neg x)
              (post-score x)
              (current-datetime)
              (post-title x)
              (post-url x)
              (post-body x)
              (post-numcom x)
              (post-section x)))

; consume database and post and delete post 
(define (delete-post-db db pid)
  (query-exec db "DELETE FROM posts WHERE id = ?" pid))

; consumes db result vector and returns post
; db_result -> post
(define (vector->post x)
  (post (vector-ref x 0)
        (vector-ref x 1)
        (vector-ref x 2)
        (vector-ref x 3)
        (vector-ref x 4)
        (vector-ref x 5)
        (vector-ref x 6)
        (vector-ref x 7)
        (vector-ref x 8)
        (vector-ref x 9)
        (vector-ref x 10)))

; Get posts from db
; -> list
(define (get-posts db)  
  (map vector->post (query-rows db "SELECT * FROM posts")))

; Get post from db with id
; -> vector
(define (pid->db->post db id)
  (vector->post (query-row db "SELECT * FROM posts WHERE id = ?" id)))

; Get posts from db with uid
; -> list
(define (uid->db->posts db uid)
  (map vector->post (query-rows db "SELECT * FROM posts WHERE uid = ?" uid)))

; Update the vote tallys for this post
(define (alter-post-vote db id dir)
  (let ([currpost (pid->db->post db id)])
    (cond
      [(equal? dir "up")
       (query-exec db "UPDATE posts SET pos = ?, score = ? WHERE id = ?"
                   (add1 (post-pos currpost))
                   (add1 (post-score currpost))
                   id)]
      [else
       (query-exec db "UPDATE posts SET neg = ?, score = ? WHERE id = ?"
                   (add1 (post-neg currpost))
                   (sub1 (post-score currpost))
                   id)])))

; Update the number of comments for this post
(define (inc-comment-db db pid)
  (let ([currpost (pid->db->post db pid)])
    (query-exec db "UPDATE posts SET numcom = ? WHERE id = ?"
                (add1 (post-numcom currpost))
                pid)))

; Calculate the heat level of the post
(define (calc-post-heat x)
  (* (post-score x) (expt POST_DECAY_RATE (- (current-datetime) (post-datetime x)))))

; consume a string and return list
(define (get-sorted-posts db type start end)
  (let ([posts (get-posts db)])
    (list-slice (match type
                  ["hot"
                   (~> posts
                       (map (lambda (x) (cons (calc-post-heat x) x)) _)
                       (sort _ (lambda (a b)
                                 (if (< (post-score (cdr a)) (post-score (cdr b)))
                                     #f
                                     #t)))
                       (map (lambda (x) (cdr x)) _))]
                  ["top"
                   (sort posts (lambda (a b) (if (< (post-score a) (post-score b)) #f #t)))]
                  ["new"
                   (sort posts (lambda (a b) (if (< (post-datetime a) (post-datetime b)) #f #t)))]) start end)))
       
;# COMMENTS

; Comment convenience struct
(struct comment (id uid pid pos neg score datetime body replyto))

; consume db result vector and return comment
; vector -> comment
(define (vector->comment x)
  (comment (vector-ref x 0)
           (vector-ref x 1)
           (vector-ref x 2)
           (vector-ref x 3)
           (vector-ref x 4)
           (vector-ref x 5)
           (vector-ref x 6)
           (vector-ref x 7)
           (vector-ref x 8)))

; Add comment to database
;-> comment
(define (comment->db db x)
  (query-exec db "INSERT INTO comments (uid, pid, pos, neg, score, datetime, body, replyto) VALUES (?,?,?,?,?,?,?,?);"
              (comment-uid x)
              (comment-pid x)
              (comment-pos x)
              (comment-neg x)
              (comment-score x)
              (current-datetime)
              (comment-body x)
              (comment-replyto x)))

; Get comment with id from database
; string -> comment
(define (id->db->comment db cid)
  (vector->comment (query-row db "SELECT * FROM comments WHERE id = ?" cid)))

; Get a comment's replies
; string -> list of comments
(define (get-comment-replies db cid)
  (map (lambda (x)
         (let ([comm (vector->comment x)])
           (list comm (get-comment-replies db (comment-id comm)))))
       (query-rows db "SELECT * FROM comments WHERE replyto = ?" cid)))

; Get a posts top-level comments
; string -> list of comments
(define (pid->db->toplvlcomms db pid)
  (map vector->comment (query-rows db "SELECT * FROM comments WHERE pid = ? AND replyto = ?" pid -1)))

; Get all comments and their replies
; string -> '((comment replies) (comment replies))
(define (pid->db->comms db pid)
  (map (lambda (x)
         (list x (get-comment-replies db (comment-id x)))) (pid->db->toplvlcomms db pid)))

; Get all comments and their replies - sorted using hotness algorithm
; string -> '((comment replies) (comment replies))
(define (pid->db->hotcomms db pid)
  (~> (pid->db->comms db pid)
      (map (lambda (x) (cons (calc-comment-heat x) x)) _)
      (sort _ (lambda (a b) (if (< (comment-score (cadr a)) (comment-score (cadr b))) #f #t)))
      (map (lambda (x) (cdr x)) _)))

; consume db and uid and return list of all comments by that user
; db, string -> list
(define (uid->db->comms db uid)
  (map (lambda (x) (list (vector->comment x) '())) (query-rows db "SELECT * FROM comments WHERE uid = ?" uid)))

; Delete comment
; cid -> delete from db
(define (delete-comment-db db cid)
  (query-exec db "DELETE FROM comments WHERE id = ?" cid))

; Update the vote tallys for this comment
(define (alter-comm-vote db id dir)
  (let ([currcomm (id->db->comment db id)])
    (cond
      [(equal? dir "up")
       (query-exec db "UPDATE comments SET pos = ?, score = ? WHERE id = ?"
                   (add1 (comment-pos currcomm))
                   (add1 (comment-score currcomm))
                   id)]
      [else
       (query-exec db "UPDATE comments SET neg = ?, score = ? WHERE id = ?"
                   (add1 (comment-neg currcomm))
                   (sub1 (comment-score currcomm))
                   id)])))

; Calculate the heat level of the comment
(define (calc-comment-heat x)
  (* (comment-score (car x)) (expt POST_DECAY_RATE (- (current-datetime) (comment-datetime (car x))))))


(define (get-inbox-comments db uid)
  (map (lambda (x) (list (vector->comment x) '())) (query-rows db "SELECT * FROM comments c1 JOIN comments c2 ON c1.replyto = c2.id WHERE c2.uid = ?" uid)))



; # USERS

; User convenience struct
(struct user (id username email profile passhash))

; Consume vector and return user
(define (vector->user u)
  (user (vector-ref u 0)
        (vector-ref u 1)
        (vector-ref u 2)
        (if (sql-null? (vector-ref u 3)) "" (vector-ref u 3))
        (vector-ref u 4)))

; Add user to database
(define (user->db db x)
  (query-exec db "INSERT INTO users (username, email, passhash) VALUES (?,?,?);"
              (user-username x)
              (user-email x)
              (user-passhash x)))

; Update user in database
(define (user->db! db x)
  (query-exec db "UPDATE users SET username = ?, email = ?, profile = ?, passhash = ? WHERE id = ?;"
              (user-username x)
              (user-email x)
              (user-profile x)
              (user-passhash x)
              (user-id x)))

; Get user from database with username
(define (username->db->user db username)
  (let ([user-data (query-rows db "SELECT * FROM users WHERE username = ?" username)])
    (if (null? user-data) #f (vector->user (list-ref user-data 0)))))

; Get user from database with id
(define (id->db->user db id)
  (let ([user-data (query-rows db "SELECT * FROM users WHERE id = ?" id)])
    (if (null? user-data) #f (vector->user (list-ref user-data 0)))))

; Consume a uid and return the username
; number -> string

(define (uid->db->string db uid)
  (vector-ref (query-row db "SELECT * FROM users WHERE id = ?" uid) 1))


; # SESSIONS

; Session convenience struct
(struct session (id uid ip useragent expiry))

; consume session and add to db
(define (session->db db x)
  (query-exec db "INSERT INTO sessions (id, uid, ip, useragent, expiry) VALUES (?,?,?,?,?)"
              (session-id x)
              (session-uid x)
              (session-ip x)
              (session-useragent x)
              (+ (current-datetime) 2WEEKS)))

; consume a session id and return a session
; string -> session
(define (sid->db->session db id)
  (let ([x (query-row db "SELECT * FROM sessions WHERE id = ?" id)])
    (session (vector-ref x 0)
             (vector-ref x 1)
             (vector-ref x 2)
             (vector-ref x 3)
             (vector-ref x 4))))

; consume a session id and delete that session
(define (delete-session-db db id)
  (query-exec db "DELETE FROM sessions WHERE id = ?" id))

; consume a session id and check if it exists
(define (session-exists? db id)
  (not (null? (query-rows db "SELECT * FROM sessions WHERE id = ?" id))))

; # VOTES
; Votes convenience struct
(struct vote (id uid pid cid type dir))

(define POST 0)
(define COMMENT 1)
(define UP 1)
(define DOWN 0)

; consume db result and return vote
(define (vector->vote x)
  (vote (vector-ref x 0)
        (vector-ref x 1)
        (vector-ref x 2)
        (vector-ref x 3)
        (vector-ref x 4)
        (vector-ref x 5)))


; consume vote and add to db
(define (vote->db db x)
  (query-exec db "INSERT INTO votes (uid, pid, cid, type, dir) VALUES (?,?,?,?,?)"
              (vote-uid x)
              (vote-pid x)
              (vote-cid x)
              (vote-type x)
              (vote-dir x)))

; delete vote from db
(define (pid-delete-vote db uid pid)
  (query-exec db "DELETE FROM votes WHERE uid = ? AND pid = ?;" uid pid))

; delete vote from db
(define (cid-delete-vote db uid cid)
  (query-exec db "DELETE FROM votes WHERE uid = ? AND cid = ?;" uid cid))

; delete vote from db using appropriate function
(define (delete-vote type db uid id)
  (if (equal? type "post")
      (pid-delete-vote db uid id)
      (cid-delete-vote db uid id)))

; consume uid, pid and return vote information
(define (get-post-vote db uid pid)
  (let ([v (query-rows db "SELECT * FROM votes WHERE uid = ? AND pid = ?" uid pid)])
    (if (null? v) #f (vector->vote (car v)))))

; consume uid, cid and return vote information
(define (get-comm-vote db uid cid)
  (let ([v (query-rows db "SELECT * FROM votes WHERE uid = ? AND cid = ?" uid cid)])
    (if (null? v) #f (vector->vote (car v)))))

; consume type, db, uid and id and return vote information using proper function
(define (get-vote type db uid id)
  (if (equal? type "post")
      (get-post-vote db uid id)
      (get-comm-vote db uid id)))


; consume type, dbm, uid, id and return whether user has voted on given item
(define (user-voted type db uid id)
  (let* ([col (if (equal? type "post") "pid" "cid")]
         [q-string (string-append "SELECT * FROM votes WHERE uid = ? AND " col " = ?")]
         [v (query-rows db q-string uid id)])
    (if (null? v) #f #t)))

; use proper function to alter item vote
(define (alter-vote type db id dir)
  (if (equal? type "post")
      (alter-post-vote db id dir)
      (alter-comm-vote db id dir)))

(define (create-new-vote type db uid id dir)
  (if (equal? type "post")
      (vote->db db (vote 0 uid id -1 POST dir))
      (vote->db db (vote 0 uid -1 id COMMENT dir))))


; # EXPORTS
(provide (all-defined-out))
(provide (all-from-out db))
