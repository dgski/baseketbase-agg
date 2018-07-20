#lang racket

(require "model.rkt")

(define our_db
  (sqlite3-connect #:database "test-r.db" #:mode 'create))


;Create Users table
(if (not (table-exists? our_db "users"))
 (query-exec our_db (create-table "users" '(("id" "number")
                                           ("username" "text")
                                           ("email" "text")))) "user table already exists")

; (struct item (id pos neg title url content numcomm comments)) 
;Create Posts table
(if (not (table-exists? our_db "posts"))
    (query-exec our_db (create-table "posts"
                                     '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT") 
                                      ("pos" "number") 
                                      ("neg" "number")
                                      ("title" "text")
                                      ("url" "text")
                                      ("content" "text")
                                      ("numcomm" "number")
                                      ))) "posts table already exists")

;(struct comment (id username body datetime replies))
; Create Comments table
(if (not (table-exists? our_db "comments"))
    (query-exec our_db (create-table "comments"
                                     '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT") 
                                      ("username" "text") 
                                      ("body" "text")
                                      ("title" "text")
                                      ("datetime" "datetime")
                                      ("replyto" "number")
                                      ))) "comments table already exists")


(define sample-posts (list (item 2 999 234 "Incredible Sights" "Bill Thompson" "" 671
                                  (list (comment 0 "gonzalez_2" "This is a sample comment for this article. I will add a little more words to it so I can see how it looks when it's content wraps around... Hopefully everything goes well!" "9 hours ago" (list reply-sample-comment))))
                            (item 1 66 0 "Castles" "www.discover.com" "" 9 '(100 20030)) 
                            (item 3 341 123 "Nothing" "Bill Hulio" "" 900 '(100 20030))
                            (item 4 345 123 "Apple's New iPhone is Triangular" "http://www.apple.com" "" 785 '(100 20030))
                            (item 5 859 320 "Some People still don't know this fact" "http://www.wikipedia.org" "" 230 '(0203 12344))
                            (item 6 233 122 "The Artic Ocean at Sundown" "http://nationalgeographic.com" "" 237 '(0203 12344))))


(map (lambda (x) (add-post-db our_db x)) sample-posts)

#|
(query-exec our_db "INSERT INTO users (id, username, email) VALUES (?,?,?);" 1 "David" "gorski.dave@gmail.com")
(query-exec our_db "INSERT INTO users (id, username, email) VALUES (?,?,?);" 2 "Bob" "bob@gmail.com")
|#
