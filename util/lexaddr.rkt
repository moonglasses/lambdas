#lang racket

(define (lookup-lexical-address var env)
  (define (helper env acc)
    (cond ((null? env) #f)
          ((eq? (car env) var) acc)
          (else (helper (cdr env) (+ acc 1)))))
  (helper env 0))
