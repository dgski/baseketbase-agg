#lang racket
(require db)

; GET DATABASE NAME
(define database-name (vector-ref (current-command-line-arguments) 0))

; # CREATE DATABASE CONNECTION
(define db
  (sqlite3-connect #:database database-name #:mode 'create))

; EXPORT
(provide db)