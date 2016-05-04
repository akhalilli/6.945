;;;; Lattice
(define-memoized (lattice x y)
                 (make-vertex
                   (edges (lattice x y)
                          ((lattice (1+ x) y))
                          ((lattice x (1+ y)))
                          ((lattice (-1+ x) y))
                          ((lattice x (-1+ y))))
                   (list x y)))

;;; Generate Guassian distribution using Galton box
;;; Vinay

;;; Random graph
;;; * edges are random
;;; * edge weight are random
;;; * shortest path
;;; * random walk
;;; Brian

;;; Graph whose structure depends on your progress
;;; * random walk on a lattice that really doesn't want you to get far away
;;; Vinay

;;; Random walk
;;; * on line graph, do you return to starting position?
;;; Brian

;;; Expressing very symmetric finite graphs succinctly using rules for
;;; edge generation
;;; * complete
;;; * cycle graph
;;; * platonic solids
;;; Brian

;;; Cayley graphs
;;; Vinay

;;; Apollonian networks
;;; Vinay

;;; Graph that models a game (tree).
;;; & Think more about it
