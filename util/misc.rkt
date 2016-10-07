#lang racket

(provide constant?)

(define (constant? c)
  (not (or (pair? c)
	   (symbol? c))))
