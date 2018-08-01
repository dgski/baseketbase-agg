#lang racket

(require db
         web-server/servlet
         threading)

; # CONSTANTS
(define POST_DECAY_RATE (expt 0.5 (/ 1 86400)))
(define 2WEEKS 1209600)


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



(define (current-datetime)
  (floor (* 0.001 (current-inexact-milliseconds))))

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
              (current-datetime)
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

; Calculate the heat level of the comment
(define (calc-comment-heat x)
  (* (comment-score (car x)) (expt POST_DECAY_RATE (- (current-datetime) (comment-datetime (car x))))))


; consume a string and return list
(define (get-sorted-posts db type)
  (let ([posts (get-posts db)])
    (cond
      [(equal? type "hot")
       (~> posts
           (map (lambda (x) (cons (calc-post-heat x) x)) _)
           (sort _ (lambda (a b) (if (< (post-score (cdr a)) (post-score (cdr b))) #f #t)))
           (map (lambda (x) (cdr x)) _))]
      [(equal? type "top")
       (sort posts (lambda (a b) (if (< (post-score a) (post-score b)) #f #t)))]
      [(equal? type "new")
       (sort posts (lambda (a b) (if (< (post-datetime a) (post-datetime b)) #f #t)))]))
  )
       
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
  (query-exec "DELETE FROM comments WHERE cid = ?" cid))

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



; consume uid, pid and return whether user voted on this post already
(define (user-voted-on-post db uid pid)
  (let ([v (query-rows db "SELECT * FROM votes WHERE uid = ? AND pid = ?" uid pid)])
    (if (null? v) #f #t)))

; consume uid, pid and return vote information
(define (get-post-vote db uid pid)
  (let ([v (query-rows db "SELECT * FROM votes WHERE uid = ? AND pid = ?" uid pid)])
    (if (null? v) #f (vector->vote (car v)))))

; consume uid, pid and return whether user voted on this comment already
(define (user-voted-on-comm? db uid cid)
  (let ([v (query-rows db "SELECT * FROM votes WHERE uid = ? AND cid = ?" uid cid)])
    (if (null? v) #f #t)))

; consume uid, cid and return vote information
(define (get-comm-vote db uid cid)
  (let ([v (query-rows db "SELECT * FROM votes WHERE uid = ? AND cid = ?" uid cid)])
    (if (null? v) #f (vector->vote (car v)))))



; # EXPORTS
(provide (all-defined-out))
(provide (all-from-out db))


