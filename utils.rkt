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

; consume a request and return a redirect with a new cookie header to prevent banner display
; request -> redirect
(define (hide-banner r)
  (redirect-to "/" #:headers (list (cookie->header (make-cookie "no-banner" "true")))))


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

; returns the current datetime
(define (current-datetime)
  (floor (* 0.001 (current-inexact-milliseconds))))

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


; # FILE SERVING UTILITIES

; consume request and filename and send that file back to use
; Currently used for css
; request -> X-expr
(define (serve-asset r f)
  (response 200 #"OK" 0 #"text/css" empty (lambda (op)
                                            (with-input-from-file (string-append "static/" f)
                                              (lambda () (copy-port (current-input-port) op))))))


; # MISC UTILITIES

; consumes a test and a dest and returns a wrapping function which checks test and accordingly returns dest
; cond,string -> function
(define (gate-factory test dest)
  (lambda (f)
    (lambda args
      (if (test args) (apply f args) dest))))


(define (referer-direct)
  (bytes->string/utf-8 (header-value (headers-assq #"Referer" (request-headers/raw r)))))


; # EXPORTS
(provide (all-defined-out))