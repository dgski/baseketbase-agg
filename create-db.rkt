#lang racket

(require "model.rkt"
         "sessions.rkt"
         "dbconn.rkt")

; # SETUP CRYPTOGRAPHY
(setup-crypto)

;Create Users table
(if (not (table-exists? db "users"))
    (query-exec db (create-table "users" '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT")
                                           ("username" "TEXT")
                                           ("email" "TEXT")
                                           ("profile" "TEXT")
                                           ("passhash" "TEXT")))) "users table already exists")

;Create Posts table
(if (not (table-exists? db "posts"))
    (query-exec db (create-table "posts"
                                 '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT")
                                   ("uid" "INTEGER")
                                   ("pos" "INTEGER")
                                   ("neg" "INTEGER")
                                   ("score" "INTEGER")
                                   ("datetime" "INTEGER")
                                   ("title" "TEXT")
                                   ("url" "TEXT")
                                   ("body" "TEXT")
                                   ("numcom" "INTEGER")
                                   ("section" "TEXT")
                                   ))) "posts table already exists")

; Create Comments table
(if (not (table-exists? db "comments"))
    (query-exec db (create-table "comments"
                                 '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT") 
                                   ("uid" "INTEGER")
                                   ("pid" "INTEGER")
                                   ("pos" "INTEGER")
                                   ("neg" "INTEGER")
                                   ("score" "INTEGER")
                                   ("datetime" "INTEGER")
                                   ("body" "TEXT")
                                   ("replyto" "INTEGER")))) "comments table already exists")

; Create sessions table
(if (not (table-exists? db "sessions"))
    (query-exec db (create-table "sessions"
                                 '(("id" "TEXT PRIMARY KEY")
                                   ("uid" "INTEGER")
                                   ("ip" "TEXT")
                                   ("useragent" "TEXT")
                                   ("expiry" "INTEGER")))) "sessions table already exists")

; Create messages table
(if (not (table-exists? db "messages"))
    (query-exec db (create-table "messages"
                                 '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT"
                                         ("uid_to" "INTEGER")
                                         ("uid_from" "INTEGER")
                                         ("body" "TEXT")
                                         ("datetime" "INTEGER")
                                         ("seen" "INTEGER")
                                         ("replyto" "INTEGER"))))) "messages table already exists")

; Create votes table
(if (not (table-exists? db "votes"))
    (query-exec db (create-table "votes"
                                 '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT")
                                   ("uid" "INTEGER")
                                   ("pid" "INTEGER")
                                   ("cid" "INTEGER")
                                   ("type" "INTEGER") ; 0 - post, 1 - comment
                                   ("dir" "INTEGER")))) "votes table already exists")

(if (not (table-exists? db "inbox"))
    (query-exec db (create-table "inbox"
                                 '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT")
                                   ("uid" "INTEGER")
                                   ("cid" "INTEGER")
                                   ("seen" "INTEGER")))) "inbox table already exists")


; Add Sample Posts Into db
; (struct post (id uid pos neg score datetime title url body numcomm section))
(define sample-posts (list (post 0 1 999 234 1234 0 "Incredible Sights" "Bill Thompson" "" 4 "front")
                           (post 0 1 700 234 344 0 "Castles" "http://discover.com" "" 0 "front")
                           (post 0 1 499 234 233 0 "Nothing" "Bill Hulio" "" 0 "front")
                           (post 0 1 234 23 432 0 "Apple's New iPhone is Triangular" "http://apple.com" "" 0 "front")
                           (post 0 1 580 234 444 0 "Some People still don't know this fact" "http://wikipedia.org" "" 0 "front")
                           (post 0 1 876 0 674 0 "The Artic Ocean at Sundown" "http://nationalgeographic.com" "" 0 "front")
                           (post 0 1 43 0 455 0 "Does anyone need a job in Belfast?" "reginald_papa" "" 0 "front")
                           (post 0 1 77 0 463 0 "The Truth Behind Medicine" "http://popularscience.com" "" 0 "front")
                           (post 0 1 11 0 96 0 "Stoic Philosophy" "http://medium.com" "" 0 "front")
                           (post 0 1 1002 0 124 0 "We Need Biking More than Ever" "http://nytimes.com" "" 0 "front")
                           (post 0 1 300 0 233 0 "Minimalism: Start Guide" "http://medium.com" "" 0 "front")
                           (post 0 1 109 0 453 0 "Avengers: Infinity War - HISHE" "http://youtube.com" "" 0 "front")
                           (post 0 2 10 0 634 0 "When Life Seems like too much.." "http://basket.base" "" 0 "front")
                           (post 0 2 54 0 27 0 "Top Museums, Rome - Pictures" "http://instagram.com" "" 0 "front")
                           ))
(map (lambda (x) (post->db db x)) sample-posts)

; Add Sample comments
(comment->db db (comment 1 2 1 3 4 5 1000 "This is a top level comment" -1))
(comment->db db (comment 1 2 1 3 4 5 1000 "This is a reply" 1))
(comment->db db (comment 1 2 1 3 4 5 1000 "This is another reply" 1))
(comment->db db (comment 1 2 1 3 4 5 1000 "Third level down." 2))

; Add Sample users
(user->db db (user 0 "gonzalez" "jose@gonzalez.com" "" (hashpass "1234")))
(user->db db (user 0 "testo_richardson" "testo@gmail.com" "" "xxxxx"))