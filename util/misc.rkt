#lang racket

(provide constant?
         singleton?)

(define (constant? c)
  (not (or (pair? c)
	   (symbol? c))))

(define (singleton? l)
  (and (pair? l)
       (null? (cdr l))))
