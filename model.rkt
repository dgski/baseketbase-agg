#lang racket

(require db
         web-server/servlet)

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
(define (parse-post b)
  (post 0
        0
        0
        0
        0
        "GETDATE()"
        (extract-binding/single 'title b)
        (extract-binding/single 'url b)
        (extract-binding/single 'title b)
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
              (post-datetime x)
              (post-title x)
              (post-url x)
              (post-body x)
              (post-numcom x)
              (post-section x)))

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


; Get all posts from db
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
  (vector->post (query-rows db "SELECT * FROM posts WHERE uid = ?" uid) 0))

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
              (comment-datetime x)
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

; Delete comment
; cid -> delete from db
(define (delete-comment-db db cid)
  (query-exec "DELETE FROM comments WHERE cid = ?" cid))


; # USERS

; User convenience struct
(struct user (id username email profile passhash))

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
  (query-exec db "UPDATE ? SET username = ?, email = ?, profile = ?, passhash = ? WHERE id = ?"
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
              (session-expiry x)))

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





; # EXPORTS
(provide (all-defined-out))
(provide (all-from-out db))



