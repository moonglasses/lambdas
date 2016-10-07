#lang racket

(require "util/env.rkt"
	 "util/misc.rkt")

(struct closure (parameters body env))

(struct ref ((val #:mutable)))

(define (eval1 exp env)
  (match exp
    ((? constant?) exp)
    ((? symbol?) (ref-val (env-lookup env exp)))
    (`(quote ,e) e)
    (`(lambda (,x* ...) ,b ...)
     (closure x* b env))
    (`(if ,t ,c ,a)
     (if (eval1 t env)
         (eval1 c env)
         (eval1 a env)))
    (`(set! ,v ,e)
     (set-ref-val! (env-lookup env v) (eval1 e env)))
    (`(begin ,e* ...)
     (eval-seq e* env))
    (`(,rator ,rand* ...)
     (define rator^ (eval1 rator env))
     (define rand*^ (evlis rand* env))
     (apply1 rator^ rand*^))))

(define (apply1 rator rand*)
  (if (procedure? rator)
      (apply rator rand*)
      (eval-seq (closure-body rator)
                (env-extend* (closure-env rator)
                             (closure-parameters rator)
                             (map ref rand*)))))

(define (eval-seq e* env)
  (if (singleton? e*)
      (eval1 (first e*) env)
      (begin (eval1 (first e*) env)
             (eval-seq (rest e*) env))))

(define (evlis e* env)
  (map (lambda (e)
         (eval1 e env))
       e*))

(define initial-env
  (make-env
   `((+ . ,(ref +))
     (- . ,(ref -))
     (/ . ,(ref /))
     (* . ,(ref *))
     (equal? . ,(ref equal?))
     (display . ,(ref display)))))
