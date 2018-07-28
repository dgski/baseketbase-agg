#lang web-server

(require web-server/http/id-cookie
         crypto
         crypto/all)

(require "model.rkt")

; Use the right crypto libraries
(define (setup-crypto)
  (use-all-factories!))

; Get the User Agent from the given request
; request -> string
(define (get-user-agent r)
  (bytes->string/utf-8 (header-value (headers-assq #"User-Agent" (request-headers/raw r)))))

; Hash password
; string -> hash and formula string
(define (hashpass p)
  (pwhash '(pbkdf2 hmac sha256) (string->bytes/utf-8 p) '((iterations 100000))))

; Validate given password
(define (valid-password? password hash)
  (with-handlers ([exn:all (lambda (v) #f)])
             (pwhash-verify #f (string->bytes/utf-8 password) hash)))

; Catches all exceptions
(define exn:all (lambda (v) #t))

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
    (and session_id
         (session-exists? db session_id)
         (let ([curr_session (sid->db->session db session_id)])
           (and (equal? (session-ip curr_session) (request-client-ip r))
                (< (current-datetime) (session-expiry curr_session))
                (equal? (session-useragent curr_session) (get-user-agent r)))))))

; get the currently logged in user
(define (current-user db r)
  (let ([session_id (parse-session-info r)])
    (id->db->user db (session-uid (sid->db->session db session_id)))))

; receives request and db connection and attempts to log user in
; request, db -> redirect
(define (attempt-user-login r db)
  (match-let ([(list username password) (parse-login-info (request-bindings r))])
    (let ([curr_user (username->db->user db username)])
      (if [and
           curr_user
           (non-empty-string? password)
           (valid-password? password (user-passhash curr_user))]
          (let ([sid (gen-sid)])
            (begin (session->db db (session sid
                                            (user-id curr_user)
                                            (request-client-ip r)
                                            (get-user-agent r)
                                            "SELECT DATETIME('now','+ 1 month');"))
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