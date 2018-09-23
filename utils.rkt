#lang racket

(require gregor
         gregor/period
         web-server/servlet
         web-server/servlet-env)



; LIST UTILITIES

; consumes a list, a start and an optional end index and returns a subset of the given list using those indices
; list, Number, Number -> list
(define (list-slice l start [end #f])
  (cond
    [(null? l) '()]
    [(= start 0)
     (if end
         (if (< end (length l)) (take l end) l)
         l)]
    
    [else (list-slice (cdr l) (- start 1) (if end (- end 1) end))]))



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
