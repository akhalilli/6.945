;;; Construct a graph with nodes numbered 0 through n-1 with edges filtered by filter
;;; and where the edges from a to b has properties in the list returned by (properties a b).
;;; (These properties still have the form '(weight 5 capacity 7).)
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
                       k
                       `(numbered ,n)))
    vertex))

(define (bernoulli p)
  (lambda (k x) (> 1 (random (exact->inexact (/ p))))))

;;; Random directed graph with edge probabilities p
;;; The difference between this and the actual Erdos-Renyi graphs is
;;; that if (-- tail head) is an edge, (-- head tail) may not be.
(define (make-di-erdos n p)
  (make-numbered n (bernoulli p)))

;;; Make a graph with random edges (and weights).
;;; p is the probability of an edge, or a function generating the probability between vertices.
;;; gen generates the weights between vertices, defaulted to random in [0,1).
(define (make-random-graph n p #!optional gen)
  (let ((gen (if (default-object? gen) (lambda (k x) (random 1.)) gen)))
    (make-numbered n
                   (if (number? p) (bernoulli p) p)
                   (lambda (k x) `(w ,(gen k x))))))

(vertex-edges-list ((make-di-erdos 5 .5) 0))
;Value: (#[edge (-- 0 2)] #[edge (-- 0 4)])
(apply shortest-path (map (make-di-erdos 6 .5) '(0 5)))
;Value: (2 . #[vertex 2])
(apply shortest-path (map (make-random-graph 6 .5) '(0 5)))
;Value: (.8948368051215073 . #[vertex 3])
(traverse-random ((make-random-graph 6 .5) 3) 3)
;Value: #[vertex 2]
