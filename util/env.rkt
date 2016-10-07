#lang racket

(provide make-env
	 env-lookup
	 env-extend
         env-extend*)

(define make-env make-immutable-hasheq)
(define env-lookup hash-ref)
(define env-extend hash-set)
(define (env-extend* env key* v*)
  (define (set-hash k v h) (hash-set h k v))
  (foldl set-hash env key* v*))
