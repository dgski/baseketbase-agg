#lang web-server

(require web-server/servlet
         web-server/servlet-env
         racket/port)

(require "views.rkt")

; MODEL DEFINITIONS

(struct item (id pos neg title url content numcomm comments)) 
(struct basket (items) #:mutable)


(define ITEMS (basket (list (item 2 999 234 "Incredible Sights" "Bill Thompson" "" 671 '(100 20030))
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


;RENDER FUNCTIONS

(define (render-heading sorter?)
  `(div ((class "heading"))
       (div ((class "heading-holder"))
            (div ((class "heading-helper"))
                 (div ((class "heading-text"))
                      (a ((class "heading-link-main") (href "/"))
                         "basket" (span ((class "base")) ".") "base"))
                 ,(if sorter? `(div ((class "heading-sorter")) ,(render-sorter)) "")
                 (div ((class "heading-links"))
                      (div ((style "width: 100%; text-align: right"))
                           (a ((class "heading-link") (href "submit")) "submit")
                           (a ((class "heading-link") (href "about")) "about")
                           (a ((class "heading-link") (href "inbox")) "inbox")
                           (a ((class "heading-link") (href "account")) "account")
                           (a ((class "heading-link") (href "signout")) "sign out")))))))

(define (render-footer)
  `(div ((class "footer"))
             (div ((class "controls"))
                  (a ((class "control-link") (href "prev")) "< prev")
                  (a ((class "control-link") (href "next")) "next >"))))

(define (render-sorter)
  '(select (option ((value "hot")) "hot")
           (option ((value "new")) "new")
           (option ((value "top")) "top")))


(define (render-item x)
  `(div ((class "item"))
        (div ((class "heat-level"))
             (div ((class "heat-level-cont"))
                  (div ((class "voters")) "▼▲") ,(number->string (- (item-pos x) (item-neg x)))))
        (a ((class "item-link")(href ,(item-url x))) (div ((class "content"))
             (div ((class "title")) ,(item-title x))
             (div ((class "url-sample")) ,(item-url x))))
        (div ((class "comments"))
             (div ((class "comment-container"))
                  (a ((class "comment-link") (href "test.html")) ,(string-append (number->string (item-numcomm x)) " comments"))))))


(define (render-items)
  `(div ((class "items") (style "padding-top: 25px"))
        ;,@(map render-item (take ITEMS 3))
        ,@(map render-item (basket-items ITEMS))
        ,(render-footer)))

(define (render-gnr-page title content)
  (response/xexpr
   #:preamble #"<!doctype html>"
   `(html
     (head
      (title "basketbase")
      (link ((rel "stylesheet") (type "text/css") (href "static/style.css"))))
     (body        
      ,(render-heading #f)     
      ,content))))

;RECIEVING UPDATES
(define (submit-post r)
  (basket-insert-item! ITEMS (parse-item (request-bindings r)))
  (redirect-to "/"))




; PAGE RESPONSE FUNCTIONS

(define (front-page r)
  (render-gnr-page "basketbase - Front Page" (render-items)))
   
   
(define (post-page r str)
  (response/xexpr `(h1 ,(string-append "POST # " str))))

(define (submit-page r)
  (render-gnr-page
   "basketbase - Submit Page"
   `(div ((class "items"))
         (div ((class "submit"))
              (form ((action "submit-new-post"))
                    (p "Submit something new:")
                    (input ((class "our-input submit-input") (type "text") (placeholder "title") (name "title")))
                    (br)
                    (p "Link:")
                    (input ((class "our-input submit-input") (type "text") (placeholder "url") (name "url")))
                    (br)
                    (p "Or Write Something Yourself:")
                    (textarea ((width "fill")
                               (placeholder "body")
                               (class "our-input submit-input submit-text-area")
                               (name "body")))
                    (br)
                    (button ((class "our-button")) "submit"))))))
     

;STATIC FILE SERVING

(define (serve-asset r f)
 (response 200 #"OK" 0 #"text/css" empty (lambda (op)
(with-input-from-file (string-append "static/" f) (lambda () (copy-port
(current-input-port) op))))))


; REQUEST DISPATCHING
(define (start request)
  (dispatch request))

(define-values (dispatch url)
  (dispatch-rules
   [("") front-page]
   [("post" (string-arg)) post-page]
   [("submit") submit-page]
   [("submit-new-post") submit-post]
   [("static" (string-arg)) serve-asset]
   [else (lambda (x) (response/xexpr "WRONG TURN, BRO"))]))

(serve/servlet start
               #:extra-files-paths (list (build-path (current-directory) "static"))  ; directory for static files
               #:launch-browser? #f
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:port 5000)
