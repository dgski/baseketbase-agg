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

;Add post
(define add-post
  (lambda (db id pos neg title url content numcomm)
    (query-exec db "INSERT INTO posts (id, pos, neg, title, url, content, numcomm) VALUES (?,?,?,?,?,?,?);" id pos neg title url content numcomm)))

;Add post to db
(define (add-post-db db x)
  (query-exec db "INSERT INTO posts (pos, neg, title, url, content, numcomm) VALUES (?,?,?,?,?,?);"            
              (item-pos x)
              (item-neg x)
              (item-title x)
              (item-url x)
              (item-content x)
              (item-numcomm x)))


(define (db->post x)
  (item (vector-ref x 0)
        (vector-ref x 1)
        (vector-ref x 2)
        (vector-ref x 3)
        (vector-ref x 4)
        (vector-ref x 5)
        (vector-ref x 6)
        '()))


(define (get-posts db)
  (map db->post (query-rows db "SELECT * FROM posts")))


(define (get-post db id)
  (db->post (list-ref (query-rows db "SELECT * FROM posts WHERE id = ?" id) 0)))




; # MODEL DEFINITIONS
(struct item (id pos neg title url content numcomm comments)) 
(struct basket (items) #:mutable)
(struct comment (id username body datetime replies))


(define reply-sample-comment
  (comment 0 "richardson_11" "Hey, I think your reply is cool, but I have an even more cool experience to share. My comment is the best, because, quite frankly, I am the best!" "4 hours ago" '()))

(define ITEMS (basket (list (item 2 999 234 "Incredible Sights" "Bill Thompson" "" 671
                                  (list (comment 0 "gonzalez_2" "This is a sample comment for this article. I will add a little more words to it so I can see how it looks when it's content wraps around... Hopefully everything goes well!" "9 hours ago" (list reply-sample-comment))))
                            (item 1 66 0 "Castles" "www.discover.com" "" 9 '(100 20030)) 
                            (item 3 341 123 "Nothing" "Bill Hulio" "" 900 '(100 20030))
                            (item 4 345 123 "Apple's New iPhone is Triangular" "http://www.apple.com" "" 785 '(100 20030))
                            (item 5 859 320 "Some People still don't know this fact" "http://www.wikipedia.org" "" 230 '(0203 12344))
                            (item 6 233 122 "The Artic Ocean at Sundown" "http://nationalgeographic.com" "" 237 '(0203 12344)))))

(define (parse-item b)
  (item 0 0 0 (extract-binding/single 'title b)
              (extract-binding/single 'url b)
              (extract-binding/single 'title b)
              0
              '()))

(define (basket-insert-item! b i)
  (set-basket-items! b (cons i (basket-items b))))





