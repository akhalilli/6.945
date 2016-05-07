;;;; Lattice
(define-memoized (lattice x y)
                 (make-vertex
                   (edges (lattice x y)
                          ((lattice (1+ x) y))
                          ((lattice x (1+ y)))
                          ((lattice (-1+ x) y))
                          ((lattice x (-1+ y))))
                   (list x y)))

;;;; Shortest path example with infinite graphs

(define example-shortest-paths
  (shortest-path-tree (lattice 0 0) (lattice 3 5)))

(hash-table/get example-shortest-paths (lattice 3 5) #f)
;Value: (8 . #[vertex 14 (3 4)])

(hash-table/count example-shortest-paths)
;Value: 155

;;;; Examples using graph methods

(define v-a (lattice 2 3))
(define v-b (lattice 5 7))

(define lattice-edge (make-edge v-a v-b 'w 5 'c 3))
(add-edge! lattice-edge)
(remove-edge! lattice-edge)
