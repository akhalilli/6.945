;;;; Lattice
(define-memoized (lattice x y)
                 (make-vertex
                   (edges (lattice x y)
                          ((lattice (1+ x) y))
                          ((lattice x (1+ y)))
                          ((lattice (-1+ x) y))
                          ((lattice x (-1+ y))))
                   (list x y)))
