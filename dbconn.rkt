#lang racket
(require db)

; GET DATABASE NAME

(define args (current-command-line-arguments))

(when (< (vector-length args) 1)
  (begin (displayln "Usage: create-db.rkt <DATABASE_FILE>")
         (exit)))

(define database-name (vector-ref (current-command-line-arguments) 0))

; # CREATE DATABASE CONNECTION
(define db
  (sqlite3-connect #:database database-name #:mode 'create))

; EXPORT
(provide db)