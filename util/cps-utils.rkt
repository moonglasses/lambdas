#lang racket

(provide map&)

(define (map& f l k)
  (if (null? l)
      (k '())
      (f (car l) (lambda (r)
                   (map& f (cdr l) (lambda (re)
                                     (k (cons r re))))))))
