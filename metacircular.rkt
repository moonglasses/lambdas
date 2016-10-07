#lang racket

(require "util/env.rkt"
	 "util/misc.rkt")

(struct Closure (parameters body env))

(define (eval1 exp env)
  (match exp
    ((? constant?) exp)
    ((? symbol?) (env-lookup env exp))
    (`(quote ,e) e)
    (`(lambda (,x* ...) ,b ...)
     (Closure x* b env))
    (`(if ,t ,c ,a)
     (if (eval1 t env)
         (eval1 c env)
         (eval1 a env)))
    (`(,rator ,rand* ...)
     (define rator^ (eval1 rator env))
     (define rand*^ (evlis rand* env))
     (apply1 rator^ rand*^))))

(define (apply1 rator rand*)
  (if (procedure? rator)
      (apply rator rand*)
      (eval (Closure-body rator)
            (env-extend* (Closure-env rator)
                         (Closure-parameters rator)
                         rand*))))

(define (evlis el env)
  (map (lambda (e)
         (eval1 e env))
       el))
