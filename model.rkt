#lang racket

(require db
         web-server/servlet)

(provide (all-defined-out))
(provide (all-from-out db))


;Create table col string from list of pairs
(define create-col
  (lambda (lat)
    (cond
      ((null? lat) "")
      (else
        (string-append (car (car lat)) 
                       " " 
                       (car (cdr (car lat))) 
                       ((lambda (lat) (if (null? lat) "" ", ")) (cdr lat)) 
                       (create-col (cdr lat)))))))

;Create a table with the following columns
(define create-table
  (lambda (name lat)
    (string-append "CREATE TABLE " name " (" (create-col lat) ");")))


; POSTS

; Post convenience struct
(struct post (id uid pos neg score datetime title url body numcom section))

; Parse post from request bindings
(define (parse-post b)
  (post 0
        0
        0
        0
        0
        0
        (extract-binding/single 'title b)
        (extract-binding/single 'url b)
        (extract-binding/single 'title b)
              0
              "front"))

;Add post to db
(define (add-post-db db x)
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
(define (db->post x)
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
(define (get-posts db)
  (map db->post (query-rows db "SELECT * FROM posts")))

; Get post from db with id
(define (pid->post db id)
  (db->post (query-row db "SELECT * FROM posts WHERE id = ?" id)))

; Get posts from db with uid
(define (uid->posts db uid)
  (db->post (query-rows db "SELECT * FROM posts WHERE uid = ?" uid) 0))


; USERS

(define (uid->string db x)
  (vector-ref (query-row db "SELECT * FROM users WHERE id = ?" x) 1))



; # MODEL DEFINITIONS
(struct basket (items) #:mutable)
(struct comment (id username body datetime replies))





