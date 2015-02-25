#lang racket

(require (only-in pict htl-append text)
         pict/code)

(provide disclaimer
         header
         example
         press-enter)

(define disclaimer? #f)

(define (disclaimer)
  (unless disclaimer?
    (set! disclaimer? #t)
    (display "
Mild WARNING: Some of the information in this tutorial may be subject to change.

Pict3D is written in Typed Racket, but you can use it in untyped Racket without
worrying about losing performance.
")))

(define (header s)
  (define n (+ 2 (string-length s)))
  (define m0 (max 0 (quotient (- 80 n) 2)))
  (define m1 (max 0 (- 80 n m0)))
  (printf "~n~v~n"
          (text (string-append
                 (make-string m0 #\=)
                 " "
                 s
                 " "
                 (make-string m0 #\=))
                (current-code-font)
                ((get-current-code-font-size)))))

(define-syntax (example stx)
  (syntax-case stx ()
    [(_ e ...)
     (syntax/loc stx
       (begin
         (newline)
         (printf "~v~n" (htl-append
                         (text "> " (current-code-font) ((get-current-code-font-size)))
                         (code e))) ...
         (newline)
         e ...))]))

(define default-enter-msg (string-append (make-string 80 #\=) "\n"))

(define (press-enter [msg default-enter-msg])
  (void (read-line))
  (printf msg))
