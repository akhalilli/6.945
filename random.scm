;;;; Random directed graph with edge probabilities p
(define (make-numbered n filter #!optional properties)
  (let ((properties (if (default-object? properties)
                      (lambda (k x) '())
                      properties)))
    (define-memoized (vertex k)
                     (make-vertex
                       (make-edges
                         (stream-map
                           (lambda (head) (apply make-edge
                                                 (cons* (vertex k) (vertex head) (properties k head))))
                           (stream-filter (lambda (x) (and (filter k x)
                                                           (not (eqv? k x))))
                                          (stream-iota n))))
                       k))
    (vertex 0)))

(define (bernoulli p)
  (lambda (k x) (< 1 (random (/ p)))))

(define (make-di-erdos n p)
  (make-numbered n (bernoulli p)))

(define (make-random-graph n p)
  (make-numbered n
                 (bernoulli p)
                 (lambda (k x) `(w ,(random 1.)))))
