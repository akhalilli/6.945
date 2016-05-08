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
(define (camera-vertex-transform transform . args)
  (compose* (apply camera args) transform actual-coord vertex-name))

(define (camera-vertex-random #!optional scale)
  (let ((scale (if (default-object? scale) 1 scale)))
    (camera-vertex (random-point) (random-point) scale)))
(define (camera-vertex-transform-random transform #!optional scale)
  (let ((scale (if (default-object? scale) 1 scale)))
    (camera-vertex-transform transform (random-point) (random-point) scale)))

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

(define (draw-3d-graph-rotate vertex rate #!optional scale device)
  (let ((device (if (default-object? device) (make-graphics) device))
        (scale (if (default-object? scale) 1 scale))
        (forward (random-point))
        (up (random-point)))
    (let iter ((t 0))
      (let ((r (rotate-y t)))
        (graphics-clear device)
        (draw-graph vertex (camera-vertex-transform r forward up scale) device)
        (sleep-current-thread 50)
        (iter (+ t rate))))))

#|
(draw-graph tetrahedron (camera-vertex '(0 3 -1) '(9 1 2) 0.5))
(draw-graph octahedron (camera-vertex '(0 0 -1) '(0 1 0)))
(draw-graph icosahedron (camera-vertex '(0 0 -1) '(0 1 0) 0.5))
(draw-graph icosahedron (camera-vertex-random 0.3))
(draw-graph dodecahedron (camera-vertex-random 0.3))
(draw-3d-graph-rotate octahedron 0.1 0.3)
(draw-3d-graph-rotate dodecahedron 0.1 0.3)
|#

#|
(draw-graph (make-circle 6) circle-plot)
(draw-graph (make-circle 12) circle-plot)
(draw-graph (make-complete 6) circle-plot)
(draw-graph (make-complete 12) circle-plot)
(draw-graph ((make-di-erdos 6 0.5) 0) circle-plot)
(draw-graph ((make-di-erdos 12 0.5) 0) circle-plot)
|#
