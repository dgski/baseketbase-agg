#lang racket

(require gregor
         gregor/period
         web-server/servlet
         web-server/servlet-env)


; BINDING UTILITIES

; consumes an id and http bindings and returns the value if it exists, otherwise returns false
; symbol, bindings -> string or bool
(define (check-and-extract-binding id binds)
  (if (exists-binding? id binds) (extract-binding/single id binds) #f))



; DATETIME UTILITIES

; Standard datetime format used for basketbase
(define DEFAULT_DATETIME_FORMAT "dd/MM/yy HH:mm")

; consumes a number representing posix time and a string representing format and returns a formatted string for the datetime
; number,string -> string
(define (posix->string secs format)
  (let* ([current (posix->datetime secs)]
         [current-with-timezone (with-timezone current (current-timezone))])
    (~t (+period current (period [seconds (->utc-offset current-with-timezone)])) format)))

; # EXPORTS
(provide (all-defined-out))
