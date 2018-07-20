#lang racket

(require "model.rkt")

(define our_db
  (sqlite3-connect #:database "test-r.db" #:mode 'create))


;Create Users table
(if (not (table-exists? our_db "users"))
 (query-exec our_db (create-table "users" '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT")
                                            ("username" "TEXT")
                                            ("email" "TEXT")
                                            ("profile" "TEXT")
                                            ("passhash" "TEXT")))) "users table already exists")

;Create Posts table
(if (not (table-exists? our_db "posts"))
    (query-exec our_db (create-table "posts"
                                     '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT")
                                       ("uid" "INTEGER")
                                       ("pos" "INTEGER")
                                       ("neg" "INTEGER")
                                       ("score" "INTEGER")
                                       ("datetime" "DATETIME")
                                       ("title" "TEXT")
                                       ("url" "TEXT")
                                       ("body" "TEXT")
                                       ("numcom" "INTEGER")
                                       ("section" "TEXT")
                                       ))) "posts table already exists")

; Create Comments table
(if (not (table-exists? our_db "comments"))
    (query-exec our_db (create-table "comments"
                                     '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT") 
                                       ("uid" "INTEGER")
                                       ("pid" "INTEGER")
                                       ("pos" "INTEGER")
                                       ("neg" "INTEGER")
                                       ("score" "INTEGER")
                                       ("datetime" "DATETIME")
                                       ("body" "TEXT")
                                       ("reply_to" "INTEGER")))) "comments table already exists")

; Create sessions table
(if (not (table-exists? our_db "sessions"))
    (query-exec our_db (create-table "sessions"
                                     '(("id" "TEXT")
                                       ("uid" "INTEGER")
                                       ("ip" "TEXT")
                                       ("useragent" "TEXT")
                                       ("expiry" "DATETIME")))) "sessions table already exists")

; Create messages table
(if (not (table-exists? our_db "messages"))
    (query-exec our_db (create-table "messages"
                                     '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT"
                                       ("uid_to" "INTEGER")
                                       ("uid_from" "INTEGER")
                                       ("body" "TEXT")
                                       ("datetime" "DATETIME")
                                       ("seen" "INTEGER")
                                       ("reply_to" "INTEGER"))))) "messages table already exists")

; Create votes table
(if (not (table-exists? our_db "votes"))
    (query-exec our_db (create-table "votes"
                                     '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT")
                                       ("uid" "INTEGER")
                                       ("pid" "INTEGER")
                                       ("cid" "INTEGER")
                                       ("type" "INTEGER")
                                       ("direction" "INTEGER")))) "votes table already exists")



; Add Sample Posts Into db
; (struct post (id uid pos neg score datetime title url body numcomm section))
(define sample-posts (list (post 0 0 999 234 765 0 "Incredible Sights" "Bill Thompson" "" 671 "front")))

(map (lambda (x) (add-post-db our_db x)) sample-posts)



(query-exec our_db "INSERT INTO users (username, email) VALUES (?,?);" "David" "gorski.dave@gmail.com")
(query-exec our_db "INSERT INTO users (username, email) VALUES (?,?);" "Bob" "bob@gmail.com")
