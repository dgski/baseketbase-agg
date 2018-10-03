#lang racket
(require db)

; # CREATE DATABASE CONNECTION
(define db
  (sqlite3-connect #:database "baseketbase.db" #:mode 'create))

; EXPORT
(provide db)