((lambda (x) x) (quote 0))

(((lambda (x y) (lambda (z) (z (z x y) (z x y)))) 5 6) +)

((lambda (x) (x x)) (lambda (x) (x x)))

((lambda (x) ((x x) x)) (lambda (x) ((x x) x)))

((lambda (x) ((lambda (y z) (set! y z) y) x 7)) (quote 'x))

(((lambda (x) (if x (lambda (y) (if y x y)) (lambda (z) (if z z x)))) #t) #f)
