#lang racket

(require db)

(define our_db
  (sqlite3-connect #:database "test-r.db" #:mode 'create))

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

;Add post
(define addpost
  (lambda (ourdb id uid datetime title ups downs comments url type)
    (query-exec our_db "INSERT INTO posts (id, uid, datetime, title, ups, downs, comments, url, type) VALUES (?,?,?,?,?,?,?,?,?);" id uid datetime title ups downs comments url type)))


;Create Users table
(if (not (table-exists? our_db "users"))
 (query-exec our_db (create-table "users" '(("id" "number")
                                           ("username" "text")
                                           ("email" "text")))) "user table already exists")

;Create Posts table
(if (not (table-exists? our_db "posts"))
    (query-exec our_db (create-table "posts" '(("id" "number") 
                                      ("uid" "number") 
                                      ("datetime" "datetime")
                                      ("title" "text")
                                      ("ups" "number")
                                      ("downs" "number")
                                      ("comments" "number")
                                      ("url" "text")
                                      ("type" "text")))) "posts table already exists")



(addpost our_db 1 1 1222 "Apple's New iPhone is Triangular" 300 55 785 "apple.com" "link")
(addpost our_db 2 1 1222 "Some People still don't know this fact" 364 10 230 "wikipedia" "link")



(query-exec our_db "INSERT INTO users (id, username, email) VALUES (?,?,?);" 1 "David" "gorski.dave@gmail.com")
(query-exec our_db "INSERT INTO users (id, username, email) VALUES (?,?,?);" 2 "Bob" "bob@gmail.com")

(foldl (lambda (x xs)
                  (string-append xs
                                 "<p>"
                                 (number->string (vector-ref x 0)) ". " (vector-ref x 1) " - " (vector-ref x 2)
                                 "</p>"
                                 )) "" (query-rows our_db "SELECT * FROM users"))



