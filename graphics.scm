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

(define (graphics-draw-dot device x y)
  (for-each (lambda (c)
              (graphics-draw-text device x y (symbol->string c)))
            '(a e g i j p q s t u v w x y z)))

(define (draw-graph vertex proj #!optional device scale)
  (let ((device (if (default-object? device) (make-graphics) device))
        (scale (if (default-object? scale) 1 scale)))
    (graphics-set-coordinate-limits device (- scale) (- scale) scale scale)
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

(define (draw-random-walk vertex proj #!optional sleep scale device)
  (let ((device (if (default-object? device) (make-graphics) device))
        (scale (if (default-object? scale) 1 scale))
        (sleep (if (default-object? sleep) 50 sleep))
        (vertices (make-eq-hash-table))
        (edges (make-equal-hash-table)))
    (define (hash-table/get-edge edge)
      (or (hash-table/get edges (list (edge-tail edge) (edge-head edge)) #f)
          (hash-table/get edges (list (edge-head edge) (edge-tail edge)) #f)))
    (define (hash-table/put-edge! edge val)
      (hash-table/put! edges (list (edge-tail edge) (edge-head edge)) val))
    (graphics-set-coordinate-limits device (- scale) (- scale) scale scale)
    (hash-table/put! vertices vertex 0)
    (let iter ((vertex vertex)
               (t 0))
      (graphics-clear device)
      (apply graphics-draw-dot (cons device (proj vertex)))
      ((graph-dfs
         (lambda (v)
           (let ((time (hash-table/get vertices v #f)))
             (if time
             ; (apply graphics-draw-dot (cons device (proj v))))
             (apply graphics-draw-text (append (cons device (proj v)) `(,(string time))))))
           (stream-for-each
             (lambda (edge)
               (graphics-bind-line-style
                 device
                 (if (hash-table/get-edge edge)
                   0  ; solid
                   1) ; dash
                 (lambda ()
                   (apply graphics-draw-line
                          (cons device (append (proj (edge-tail edge))
                                               (proj (edge-head edge))))))))
             (vertex-edges-stream v))
           0))
       vertex)
      (sleep-current-thread sleep)
      (let* ((edges-stream (vertex-edges-stream vertex))
            (edge (if (null? edges-stream)
                    #f
                    (stream-ref edges-stream (random (stream-length edges-stream)))))
            (next (if edge (edge-head edge) vertex)))
        (if (not (hash-table/get vertices vertex #f))
          (hash-table/put! vertices vertex t))
        (if (and edge
                 (not (hash-table/get-edge edge)))
          (hash-table/put-edge! edge t))
        (iter next (1+ t))))))

#|
(draw-graph tetrahedron (camera-vertex '(0 3 -1) '(9 1 2) 0.5))
(draw-graph octahedron (camera-vertex '(0 0 -1) '(0 1 0)))
(draw-graph icosahedron (camera-vertex '(0 0 -1) '(0 1 0) 0.5))
(draw-graph icosahedron (camera-vertex-random 0.3))
(draw-graph dodecahedron (camera-vertex-random 0.3))
(draw-3d-graph-rotate octahedron 0.1 0.3)
(draw-3d-graph-rotate icosahedron 0.1 0.3)
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

#|
(draw-graph (make-lollipop 12) lollipop-plot (make-graphics) 4)
(draw-random-walk (make-circle 12) circle-plot 200 1.2)
(draw-random-walk (make-complete 12) circle-plot 200 1.2)
(draw-random-walk (make-line 12) line-plot 200 4)
(draw-random-walk (make-lollipop 12) lollipop-plot 200 4)
|#
