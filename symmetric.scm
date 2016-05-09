;;;; Line Graphs
(define (make-line n)
  (define-memoized (line k)
                   (make-vertex
                     (cond
                       ((eq? 0 k) (edges (line k) ((line (1+ k)))))
                       ((eq? n k) (edges (line k) ((line (-1+ k)))))
                       (else (edges (line k)
                                    ((line (1+ k)))
                                    ((line (-1+ k))))))
                     k
                     `(line ,n)))
  (line 1))

;;;; Circle Graphs
(define (make-circle n)
  (define-memoized (circle k)
                   (make-vertex
                     (edges (circle k)
                            ((circle (modulo (1+ k) n)))
                            ((circle (modulo (-1+ k) n))))
                     k
                     `(circle ,n)))
  (circle 0))
(define (circle-plot vertex)
  (let* ((k (vertex-name vertex))
         (n (cadr (vertex-graph vertex)))
         (theta (* 2 pi (/ k n))))
    (list (cos theta) (sin theta))))

(vertex-edges-list (make-circle 5))
;Value: (#[edge (-- 0 1)] #[edge (-- 0 4)])
(vertex-edges-list (edge-head (cadr (vertex-edges-list (make-circle 5)))))
;Value: (#[edge (-- 4 0)] #[edge (-- 4 3)])

;;;; Complete Graphs
(define (make-complete n)
  (define-memoized (complete k)
                   (make-vertex
                     (make-edges
                       (stream-map
                         (lambda (head) (make-edge (complete k) (complete head)))
                         (stream-filter (lambda (x) (not (eq? x k)))
                                        (stream-iota n))))
                     k
                     `(complete ,n)))
  (complete 0))

(vertex-edges-list (make-complete 5))
;Value: (#[edge (-- 0 1)] #[edge (-- 0 2)] #[edge (-- 0 3)] #[edge (-- 0 4)])
(vertex-edges-list (edge-head (caddr (vertex-edges-list (make-complete 5)))))
;Value: (#[edge (-- 3 0)] #[edge (-- 3 1)] #[edge (-- 3 2)] #[edge (-- 3 4)])

(stream-head (vertex-edges (make-complete #f)) 5)
;Value: (edges #[edge (-- 0 1)] #[edge (-- 0 2)] #[edge (-- 0 3)] #[edge (-- 0 4)])
(stream-tail (vertex-edges (make-complete #f)) 5)
;Value: (#[edge (-- 0 5)] . #[promise])

;;;; Platonic Solids
(define phi 2)
(define actual-phi (/ (+ 1 (sqrt 5)) 2))
(define (actual-coord v)
  (map (lambda (x)
         (cond
           ((eqv? phi (abs x)) (* (/ actual-phi phi) x))
           ((eqv? (/ phi) (abs x)) (* (/ phi actual-phi) x))
           (else x)))
       v))

(define tetrahedron
  (let ()
    (define-memoized (vertex coords)
                     (let ((x (first coords))
                           (y (second coords))
                           (z (third coords)))
                       (make-vertex
                         (edges (vertex coords)
                                ((vertex (list x (- y) (- z))))
                                ((vertex (list (- x) y (- z))))
                                ((vertex (list (- x) (- y) z))))
                         coords)))
    (vertex '(1 1 1))))

(define octahedron
  (let ()
    (define-memoized (vertex coords)
                     (let ((x (first coords))
                           (y (second coords))
                           (z (third coords)))
                       (make-vertex
                         (edges (vertex coords)
                                ((vertex (list y z x)))
                                ((vertex (map - (list y z x))))
                                ((vertex (list z x y)))
                                ((vertex (map - (list z x y)))))
                         coords)))
    (vertex '(1 0 0))))

(define cube
  (let ()
    (define-memoized (vertex coords)
                     (let ((x (first coords))
                           (y (second coords))
                           (z (third coords)))
                       (make-vertex
                         (edges (vertex coords)
                                ((vertex (list (- x) y z)))
                                ((vertex (list x (- y) z)))
                                ((vertex (list x y (- z)))))
                         coords)))
    (vertex '(1 1 1))))

(define icosahedron
  (let ()
    (define-memoized (vertex coords)
                     (let ((x (first coords))
                           (y (second coords))
                           (z (third coords)))
                       (make-vertex
                         (edges (vertex coords)
                                ((vertex (map (lambda (q) (case (abs q) ((1) (- q)) (else q))) coords)))
                                ((vertex (map (lambda (q) (case (abs q) ((0) (- phi)) ((1) 0) (else (sgn q)))) coords)))
                                ((vertex (map (lambda (q) (case (abs q) ((0) phi) ((1) 0) (else (sgn q)))) coords)))
                                ((vertex (map (lambda (q) (case (abs q) ((0) -1) ((1) (* phi q)) (else 0))) coords)))
                                ((vertex (map (lambda (q) (case (abs q) ((0) 1) ((1) (* phi q)) (else 0))) coords))))
                         coords)))
    (vertex `(0 1 ,phi))))

(define dodecahedron
  (let ()
    (define-memoized (vertex coords)
                     (let ((x (first coords))
                           (y (second coords))
                           (z (third coords)))
                       (make-vertex
                         (if (eqv? 1 (abs x))
                           (edges (vertex coords)
                                  ((vertex (list 0 (/ y phi) (* z phi))))
                                  ((vertex (list (* x phi) 0 (/ z phi))))
                                  ((vertex (list (/ x phi) (* y phi) 0))))
                           (edges (vertex coords)
                                  ((vertex (map (lambda (q) (case q ((0) 1) (else (sgn q)))) coords)))
                                  ((vertex (map (lambda (q) (case q ((0) -1) (else (sgn q)))) coords)))
                                  ((vertex (map (lambda (q) (if (eqv? (/ phi) (abs q)) (- q) q)) coords)))))
                         coords)))
    (vertex '(1 1 1))))

(count-graph-vertices tetrahedron)
;Value: 4
(count-graph-edges tetrahedron)
;Value: 12
(count-graph-vertices octahedron)
;Value: 6
(count-graph-edges octahedron)
;Value: 24
(count-graph-vertices cube)
;Value: 8
(count-graph-edges cube)
;Value: 24
(count-graph-vertices icosahedron)
;Value: 12
(count-graph-edges icosahedron)
;Value: 60
(count-graph-vertices dodecahedron)
;Value: 20
(count-graph-edges dodecahedron)
;Value: 60
