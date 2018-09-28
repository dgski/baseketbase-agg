#lang racket

(require gregor
         gregor/period
         web-server/servlet
         web-server/servlet-env)


; COOKIE UTILITIES

;consumes a list of cookies and a cookie id and returns a boolean value whether this cookie exists in the list
; list, string -> bool
(define (cookie-exists? cookies id)
  (cond
    [(null? cookies) #f]
    [(equal? (client-cookie-name (car cookies)) id) #t]
    [else (cookie-exists? (cdr cookies) id)]))


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


; Consume HTTP bindings and return a list of user-specific bindings
; bindings -> list
(define (extract-user-bindings bindings)
  (for/list ([b '(email profile old-password new-password-1 new-password-2)])
    (extract-binding/single b bindings)))

; # EXPORTS
(provide (all-defined-out))



