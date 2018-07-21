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
                                       ("replyto" "INTEGER")))) "comments table already exists")

; Create sessions table
(if (not (table-exists? our_db "sessions"))
    (query-exec our_db (create-table "sessions"
                                     '(("id" "TEXT PRIMARY KEY")
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
                                       ("replyto" "INTEGER"))))) "messages table already exists")

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
(define sample-posts (list (post 0 0 999 234 765 0 "Incredible Sights" "Bill Thompson" "" 671 "front")
                           (post 0 0 700 234 66 0 "Castles" "http://discover.com" "" 9 "front")
                           (post 0 0 499 234 66 0 "Nothing" "Bill Hulio" "" 900 "front")
                           (post 0 0 234 23 66 0 "Apple's New iPhone is Triangular" "http://apple.com" "" 785 "front")
                           (post 0 0 580 234 66 0 "Some People still don't know this fact" "http://wikipedia.org" "" 230 "front")
                           (post 0 0 876 0 876 0 "The Artic Ocean at Sundown" "http://nationalgeographic.com" "" 1237 "front")))

(map (lambda (x) (post->db our_db x)) sample-posts)

; Add Sample comments
(comment->db our_db (comment 1 2 1 3 4 5 "2018-03-10" "This is a top level comment" -1))
(comment->db our_db (comment 1 2 1 3 4 5 "2018-07-10" "This is a reply" 1))
(comment->db our_db (comment 1 2 1 3 4 5 "2018-10-10" "This is another reply" 1))
(comment->db our_db (comment 1 2 1 3 4 5 "2018-22-10" "Third level down." 2))




(user->db our_db (user 0 "jose_gonzalez" "jose@gonzalez.com" "" "xxxxx"))
(user->db our_db (user 0 "testo_richardson" "testo@gmail.com" "" "xxxxx"))
