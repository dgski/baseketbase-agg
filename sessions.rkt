#lang web-server

(require web-server/http/id-cookie
         crypto
         crypto/all)

(require "model.rkt"
         "dbconn.rkt"
         "utils.rkt")

; Use the right crypto libraries
(define (setup-crypto)
  (use-all-factories!))

; Function which catches all exceptions
; Exception type -> #t
(define exn:all (lambda (v) #t))

; Get the User Agent from the given request
; request -> string
(define (get-user-agent r)
  (bytes->string/utf-8 (header-value (headers-assq #"User-Agent" (request-headers/raw r)))))

; Hash password
; string -> hash and formula string
(define (hashpass p)
  (pwhash '(pbkdf2 hmac sha256) (string->bytes/utf-8 p) '((iterations 100000))))

; Validate given password
; password:string, hash:string -> bool
(define (valid-password? password hash)
  (with-handlers ([exn:all (lambda (v) #f)])
    (pwhash-verify #f (string->bytes/utf-8 password) hash)))

; Generate session_id
; Process: generate list of 32 numbers (256 max) and fold into hex asci string
; -> string of 32 chars
(define (gen-sid)
  (foldr (lambda (next prev) (string-append prev (number->string next 16))) "" (for/list ((i 32)) (add1 (random 256)))))

; Consume HTTP bindings and return a list of length 2 representing user information
; bindings -> list
(define (parse-login-info b)
  (list (extract-binding/single 'username b) (extract-binding/single 'password b)))

; Parse request for cookie with "sid" key
; Uses secure cookie library
; request -> string
(define (parse-session-info r)
  (request-id-cookie "sid" (make-secret-salt/file "salt.key") r))

; consumes request and determines whether request contains valid active session
; Checks whether session exists in db, whether it has not expired yet, and whether ip and user-agent match
; request -> bool
(define (user-logged-in? db r)
  (let ([session_id (parse-session-info r)])
    (and session_id
         (session-exists? db session_id)
         ; Check if session is expired
         (let ([curr_session (sid->db->session db session_id)])
           (and (equal? (session-ip curr_session) (request-client-ip r))
                (< (current-datetime) (session-expiry curr_session))
                (equal? (session-useragent curr_session) (get-user-agent r)))))))

; get the currently logged in user
; db, r -> user
(define (current-user db r)
  (let ([session_id (parse-session-info r)])
    (id->db->user db (session-uid (sid->db->session db session_id)))))

; consumes sid, uid and request and creates a new session, and adds it to the database
; string -> add to database
(define (create-add-session sid uid r)
  (session->db db (session sid uid (request-client-ip r) (get-user-agent r) "")))

; consumes a cookie key and value and returns an HTTP header with that single cookie
(define (single-cookie-header key value)
  (list (cookie->header (make-id-cookie key (make-secret-salt/file "salt.key") value))))

; receives request and db connection and attempts to log user in
; request, db -> redirect
(define (attempt-user-login r)
  (match-let* ([(list username password) (parse-login-info (request-bindings r))]
              [curr-user (username->db->user db username)])
    (if [and curr-user (non-empty-string? password) (valid-password? password (user-passhash curr-user))]
        (let ([sid (gen-sid)])
          (begin (create-add-session sid (user-id curr-user) r) (redirect-to "/" #:headers (single-cookie-header "sid" sid))))
        (redirect-to "login"))))

; receives request and db connection and attempts to log user out
; request, db -> redirect
(define (attempt-user-logout r)
  (delete-session-db db (request-id-cookie "sid" (make-secret-salt/file "salt.key") r))
  (redirect-to "/" #:headers (list (cookie->header (logout-id-cookie "sid")))))

; recieves request and db connections and attempts to sign up user
(define (attempt-user-signup r)
  (match-let* ([login-info (parse-login-info (request-bindings r))]
              [(list username password) login-info])
    (if (username->db->user db username)
        (redirect-to "/signup?message=Username is taken - Try again.")
        (begin
          (user->db db(user 'null-it-autoincrements username "" "" (hashpass password)))
          (attempt-user-login r db)))))

; consumes function and returns wrapping lambda which verifies request contains valid session information before running function
; function -> function
(define logreq
  (gate-factory (lambda (a) (user-logged-in? db (car a))) (redirect-to "login")))

; consumes function and returns wrapping lambda which verifies request does not contain valid session information before running function
; function -> function
(define nonlogreq
  (gate-factory (lambda (a) (not (user-logged-in? db (car a)))) (redirect-to "account")))

(provide (all-defined-out))
(provide (all-from-out web-server/http/id-cookie))