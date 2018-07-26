#lang racket

(require "model.rkt"
         "sessions.rkt")

; # SETUP CRYPTOGRAPHY
(setup-crypto)

(define our_db
  (sqlite3-connect #:database "baseketbase.db" #:mode 'create))


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
                                       ("datetime" "INTEGER")
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
                                       ("datetime" "INTEGER")
                                       ("body" "TEXT")
                                       ("replyto" "INTEGER")))) "comments table already exists")

; Create sessions table
(if (not (table-exists? our_db "sessions"))
    (query-exec our_db (create-table "sessions"
                                     '(("id" "TEXT PRIMARY KEY")
                                       ("uid" "INTEGER")
                                       ("ip" "TEXT")
                                       ("useragent" "TEXT")
                                       ("expiry" "INTEGER")))) "sessions table already exists")

; Create messages table
(if (not (table-exists? our_db "messages"))
    (query-exec our_db (create-table "messages"
                                     '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT"
                                       ("uid_to" "INTEGER")
                                       ("uid_from" "INTEGER")
                                       ("body" "TEXT")
                                       ("datetime" "INTEGER")
                                       ("seen" "INTEGER")
                                       ("replyto" "INTEGER"))))) "messages table already exists")

; Create votes table
(if (not (table-exists? our_db "votes"))
    (query-exec our_db (create-table "votes"
                                     '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT")
                                       ("uid" "INTEGER")
                                       ("pid" "INTEGER")
                                       ("cid" "INTEGER")
                                       ("type" "INTEGER") ; 0 - post, 1 - comment
                                       ("dir" "INTEGER")))) "votes table already exists")



; Add Sample Posts Into db
; (struct post (id uid pos neg score datetime title url body numcomm section))
(define sample-posts (list (post 0 0 999 234 765 0 "Incredible Sights" "Bill Thompson" "" 4 "front")
                           (post 0 0 700 234 66 0 "Castles" "http://discover.com" "" 0 "front")
                           (post 0 0 499 234 66 0 "Nothing" "Bill Hulio" "" 0 "front")
                           (post 0 0 234 23 66 0 "Apple's New iPhone is Triangular" "http://apple.com" "" 0 "front")
                           (post 0 0 580 234 66 0 "Some People still don't know this fact" "http://wikipedia.org" "" 0 "front")
                           (post 0 0 876 0 876 0 "The Artic Ocean at Sundown" "http://nationalgeographic.com" "" 0 "front")
                           (post 0 0 43 0 43 0 "Does anyone need a job in Belfast?" "reginald_papa" "" 0 "front")
                           (post 0 0 77 0 876 0 "The Truth Behind Medicine" "http://popularscience.com" "" 0 "front")
                           (post 0 0 11 0 876 0 "Stoic Philosophy" "http://medium.com" "" 0 "front")
                           (post 0 0 1002 0 876 0 "We Need Biking More than Ever" "http://nytimes.com" "" 0 "front")
                           (post 0 0 300 0 876 0 "Minimalism: Start Guide" "http://medium.com" "" 0 "front")
                           (post 0 0 109 0 876 0 "Avengers: Infinity War - HISHE" "http://youtube.com" "" 0 "front")
                           (post 0 0 10 0 876 0 "When Life Seems like too much.." "http://basket.base" "" 0 "front")
                           (post 0 0 54 0 876 0 "Top Museums, Rome - Pictures" "http://instagram.com" "" 0 "front")
                           ))
(map (lambda (x) (post->db our_db x)) sample-posts)

#|
(define sample-posts (list (post 0 0 999 234 765 0 "Incredible Sights" "Bill Thompson" "" 671 "front")
                           (post 0 0 700 234 66 0 "Castles" "http://discover.com" "" 9 "front")
                           (post 0 0 499 234 66 0 "Nothing" "Bill Hulio" "" 900 "front")
                           (post 0 0 234 23 66 0 "Apple's New iPhone is Triangular" "http://apple.com" "" 785 "front")
                           (post 0 0 580 234 66 0 "Some People still don't know this fact" "http://wikipedia.org" "" 230 "front")
                           (post 0 0 876 0 876 0 "The Artic Ocean at Sundown" "http://nationalgeographic.com" "" 1237 "front")
                           (post 0 0 43 0 43 0 "Does anyone need a job in Belfast?" "reginald_papa" "" 14 "front")
                           (post 0 0 77 0 876 0 "The Truth Behind Medicine" "http://popularscience.com" "" 45 "front")
                           (post 0 0 11 0 876 0 "Stoic Philosophy" "http://medium.com" "" 90 "front")
                           (post 0 0 1002 0 876 0 "We Need Biking More than Ever" "http://nytimes.com" "" 1003 "front")
                           (post 0 0 300 0 876 0 "Minimalism: Start Guide" "http://medium.com" "" 55 "front")
                           (post 0 0 109 0 876 0 "Avengers: Infinity War - HISHE" "http://youtube.com" "" 77 "front")
                           (post 0 0 10 0 876 0 "When Life Seems like too much.." "http://basket.base" "" 5 "front")
                           (post 0 0 54 0 876 0 "Top Museums, Rome - Pictures" "http://instagram.com" "" 8 "front")
                           ))
(map (lambda (x) (post->db our_db x)) sample-posts)
|#


; Add Sample comments
(comment->db our_db (comment 1 2 1 3 4 5 1000 "This is a top level comment" -1))
(comment->db our_db (comment 1 2 1 3 4 5 1000 "This is a reply" 1))
(comment->db our_db (comment 1 2 1 3 4 5 1000 "This is another reply" 1))
(comment->db our_db (comment 1 2 1 3 4 5 1000 "Third level down." 2))




(user->db our_db (user 0 "gonzalez" "jose@gonzalez.com" "" (hashpass "1234")))
(user->db our_db (user 0 "testo_richardson" "testo@gmail.com" "" "xxxxx"))
