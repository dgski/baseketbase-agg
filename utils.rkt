#lang racket

(require gregor
         gregor/period)


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
