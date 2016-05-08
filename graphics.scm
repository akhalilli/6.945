;;;; Linear algebra
(define ((v-op op) . vecs)
  (if (eq? 1 (length vecs))
    (map op (car vecs))
    (let ((u (first vecs))
          (v (second vecs)))
      (cond
        ((every list? (list u v)) (map op u v))
        ((list? u) (map (lambda (x) (op v x)) u))
        ((list? v) (map (lambda (x) (op u x)) v))
        (else (op u v))))))
(define v:+ (v-op +))
(define v:- (v-op -))
(define v:* (v-op *))
(define v:/ (v-op /))

(define (dot u v)
  (if (every list? (list u v))
    (apply + (v:* u v))
    (v:* u v)))
(define (cross u v)
  (let ((u1 (first u))
        (u2 (second u))
        (u3 (third u))
        (v1 (first v))
        (v2 (second v))
        (v3 (third v)))
    (list (- (* u2 v3) (* u3 v2))
          (- (* u3 v1) (* u1 v3))
          (- (* u1 v2) (* u2 v1)))))

(define (sqr-norm u) (apply + (map square u)))
(define norm (compose sqrt sqr-norm))
(define (normalized u) (dot (/ (norm u)) u))
(define (proj u v) (v:* (/ (dot u v) (sqr-norm u)) u))

(define (random-point #!optional k n)
  (let ((k (if (default-object? k) 3 k))
        (n (if (default-object? n) 1. n)))
    (make-initialized-list k (lambda (i)
                               (* (if (eq? 0 (random 2)) -1 1)
                                  (random n))))))

;;;; Camera
(define (camera forward up #!optional scale)
  (let* ((z (v:- forward))
         (y (v:- up (proj z up)))
         (scale (if (default-object? scale)
                  (/ (norm y)
                     (norm z))
                  scale))
         (y (v:* scale (normalized y)))
         (x (cross y (normalized z))))
    (lambda (v) (list (dot x v) (dot y v)))))

(define (camera-vertex . args)
  (compose* (apply camera args) actual-coord vertex-name))

(define (camera-vertex-random #!optional scale)
  (let ((scale (if (default-object? scale) 1 scale)))
    (camera-vertex (random-point) (random-point) scale)))

;;;; Graphics
(define (make-graphics)
  (make-graphics-device (car (enumerate-graphics-types))))

(define (draw-graph vertex proj #!optional device)
  (let ((device (if (default-object? device) (make-graphics) device)))
    ((graph-dfs (lambda (v)
                  (stream-for-each (lambda (edge)
                                     (apply graphics-draw-line
                                            (cons device (append (proj (edge-tail edge))
                                                                 (proj (edge-head edge))))))
                                   (vertex-edges-stream v))
                  0))
     vertex)))

#|
(draw-graph tetrahedron (camera-vertex '(0 3 -1) '(9 1 2) 0.5))
(draw-graph octahedron (camera-vertex '(0 0 -1) '(0 1 0)))
(draw-graph icosahedron (camera-vertex '(0 0 -1) '(0 1 0) 0.5))
(draw-graph icosahedron (camera-vertex-random 0.3))
(draw-graph dodecahedron (camera-vertex-random 0.3))
|#
