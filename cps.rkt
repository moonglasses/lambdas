#lang racket

(require "util/env.rkt"
	 "util/misc.rkt")

(struct closure (parameters body env))

(struct ref ((val #:mutable)))

(define (evalk exp env k)
  (match exp
    ((? constant?) (k exp))
    ((? symbol?) (k (ref-val (env-lookup env exp))))
    (`(quote ,e) (k e))
    (`(lambda (,x* ...) ,b ...)
     (k (closure x* b env)))
    (`(if ,t ,c ,a)
     (evalk t env (lambda (r)
                    (if r
                        (evalk c env k)
                        (evalk a env k)))))
    (`(set! ,v ,e)
     (evalk e env (lambda (r)
                    (k (set-ref-val! (env-lookup env v) r)))))
    (`(begin ,e* ...)
     (eval-seq e* env k))
    (`(,rator ,rand* ...)
     (evalk rator env (lambda (rator^)
                        (evlis rand* env (lambda (rand*^)
                                           (applyk rator^ rand*^ k))))))))

(define (applyk rator rand* k)
  (if (procedure? rator)
      (k (apply rator rand*))
      (eval-seq (closure-body rator)
                (env-extend* (closure-env rator)
                             (closure-parameters rator)
                             (map ref rand*))
                k)))

(define (eval-seq e* env k)
  (if (singleton? e*)
      (evalk (first e*) env k)
      (evalk (first e*) env (lambda (r)
                                     (eval-seq (rest e*) env k)))))

(define (evlis e* env k)
  (k (map (lambda (e)
            (evalk e env (lambda (x) x)))
          e*)))

(define initial-env
  (make-env
   `((+ . ,(ref +))
     (- . ,(ref -))
     (/ . ,(ref /))
     (* . ,(ref *))
     (equal? . ,(ref equal?))
     (display . ,(ref display)))))

(define initial-cont
  (lambda (x) x))
