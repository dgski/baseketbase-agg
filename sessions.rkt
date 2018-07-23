#lang web-server

(require web-server/http/id-cookie)

(require "model.rkt")

; Generate session_id
(define (gen-sid)
  (foldr (lambda (next prev) (string-append prev (number->string next 16))) "" (for/list ((i 32)) (add1 (random 256)))))

; Consume HTTP bindings and return a list of length 2 representing user information
; bindings -> list
(define (parse-login-info b)
  (list (extract-binding/single 'username b) (extract-binding/single 'password b)))

;Parse request for session if cookie
(define (parse-session-info r)
  (request-id-cookie "sid" (make-secret-salt/file "salt.key") r))

; consumes request and determines whether request contains valid active session
; request -> bool
(define (user-logged-in? db r)
  (let ([session_id (parse-session-info r)])
    (and session_id (session-exists? db session_id))))

; get the currently logged in user
(define (current-user db r)
  (let ([session_id (parse-session-info r)])
    (id->db->user db (session-uid (sid->db->session db session_id)))))

; receives request and db connection and attempts to log user in
; request, db -> redirect
(define (attempt-user-login r db)
  (match-let ([(list username password) (parse-login-info (request-bindings r))])
    (let ([curr_user (username->db->user db username)])
      (if curr_user
          (let ([sid (gen-sid)])
            (begin (session->db db (session sid
                                            (user-id curr_user)
                                            (request-client-ip r)
                                            "Mozilla"
                                            "2018-03-10"))
                   (redirect-to "/" #:headers (list (cookie->header (make-id-cookie "sid"
                                                                                    (make-secret-salt/file "salt.key")
                                                                                    sid))))))
      (redirect-to "login")))))

; receives request and db connection and attemps to log user out
; request, db -> redirect

(define (attempt-user-logout r db)
  (delete-session-db db (request-id-cookie "sid" (make-secret-salt/file "salt.key") r))
  (redirect-to "/" #:headers (list (cookie->header (logout-id-cookie "sid")))))

(provide (all-defined-out))
(provide (all-from-out web-server/http/id-cookie))