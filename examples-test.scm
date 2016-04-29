;;;; Shortest path example with infinite graphs

(define example-shortest-paths
  (shortest-path-tree (lattice 0 0) (lattice 3 5)))

(hash-table/get example-shortest-paths (lattice 3 5) #f)
;Value: (8 . #[vertex 14 (3 4)])

(hash-table/count example-shortest-paths)
;Value: 155
