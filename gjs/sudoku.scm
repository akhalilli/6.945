(declare (usual-integrations))

;; The partial information structure is just the list of possible
;; assignments.

(define (possibilities-merge content increment)
  (lset-intersection = content increment))

(define (possibilities-different? set1 set2)
  (not (null? (lset-xor = set1 set2))))

(define (all-different-deducer accumulators)
  (lambda ()
    (for-each add-content
              accumulators
              (all-different (map accumulator-content accumulators)))))

(define (possibilities-accumulator initial-content)
  (make-accumulator
    initial-content #t
    possibilities-merge
    possibilities-different?))

(define (initial-cell datum)
  (possibilities-accumulator
    (if (= datum 0)
      '(1 2 3 4 5 6 7 8 9)
      (list datum))))

(define (sudoku-puzzle list-of-rows)
  (define (sudoku-constraints cell-matrix)
    (let ((rows (map all-different-deducer cell-matrix))
          ;; apply map acts like a transpose
          (columns (apply map (lambda cells (all-different-deducer cells)) cell-matrix))
          (squares
            (map (lambda (big-row big-column)
                   (all-different-deducer
                     (map (lambda (row-offset column-offset)
                            (list-ref (list-ref cell-matrix (+ (* 3 big-row) row-offset))
                                      (+ (* 3 big-column) column-offset)))
                          '(0 0 0 1 1 1 2 2 2)
                          '(0 1 2 0 1 2 0 1 2))))
                 '(0 0 0 1 1 1 2 2 2)
                 '(0 1 2 0 1 2 0 1 2))))
      (append rows columns squares)))
  (let ((cell-matrix (map (lambda (row)
                            (map initial-cell row))
                          list-of-rows)))
    (one-guess-drive cell-matrix sudoku-constraints)))

#|

(pp (map (lambda (row) (map accumulator-content row))
         (sudoku-puzzle '((0 0 4 0 1 0 0 0 5)
                          (8 5 0 0 0 0 1 0 0)
                          (9 0 1 3 0 5 0 8 0)
                          (0 4 0 0 0 8 0 0 0)
                          (5 0 7 0 0 0 2 0 9)
                          (0 0 0 7 0 0 0 4 0)
                          (0 7 0 5 0 9 4 0 8)
                          (0 0 5 0 0 0 0 2 6)
                          (4 0 0 0 2 0 9 0 0)))))

;; Without the one-step guessing and backtracking, it gets stuck here.

(((7)       (2 3 6)   (4)       (8)     (1)       (2 6)       (3 6) (9)       (5))
 ((8)       (5)       (2 3 6)   (2 9)   (4 6 7 9) (2 4 6 7)   (1)   (3 6 7)   (2 4))
 ((9)       (2 6)     (1)       (3)     (4 6 7)   (5)         (6 7) (8)       (2 4))

 ((1 2 3 6) (4)       (2 3 6)   (2 9)   (3 6 9)   (8)         (5)   (1 3 6 7) (1 3 7))
 ((5)       (8)       (7)       (1 4 6) (3 4 6)   (1 3 4 6)   (2)   (1 3 6)   (9))
 ((1 2 3 6) (1 2 3 6) (9)       (7)     (5)       (1 2 3 6)   (8)   (4)       (1 3))

 ((1 2 3 6) (7)       (2 3 6)   (5)     (3 6)     (9)         (4)   (1 3)     (8))
 ((1 3)     (9)       (5)       (1 4)   (8)       (1 3 4 7)   (3 7) (2)       (6))
 ((4)       (1 3 6)   (8)       (1 6)   (2)       (1 3 6 7)   (9)   (5)       (1 3 7)))

;; With it, it solves the puzzle.

(((7) (6) (4) (8) (1) (2) (3) (9) (5))
 ((8) (5) (3) (9) (6) (4) (1) (7) (2))
 ((9) (2) (1) (3) (7) (5) (6) (8) (4))
 ((1) (4) (6) (2) (9) (8) (5) (3) (7))
 ((5) (8) (7) (1) (4) (3) (2) (6) (9))
 ((2) (3) (9) (7) (5) (6) (8) (4) (1))
 ((6) (7) (2) (5) (3) (9) (4) (1) (8))
 ((3) (9) (5) (4) (8) (1) (7) (2) (6))
 ((4) (1) (8) (6) (2) (7) (9) (5) (3)))
|#

#|
;; Interpreted, this takes
;process time: 113710 (112140 RUN + 1570 GC); real time: 113811
;; Compiled, this takes
;process time: 8290 (7980 RUN + 310 GC); real time: 8429
(pp (map (lambda (row) (map accumulator-content row))
         (sudoku-puzzle '((0 5 0 0 1 6 4 0 0)
                          (0 0 2 7 9 0 0 1 0)
                          (0 1 0 0 0 0 6 0 7)
                          (2 0 5 8 0 0 0 0 0)
                          (0 3 0 0 0 0 0 8 0)
                          (0 0 0 0 0 9 3 0 2)
                          (1 0 3 0 0 0 0 6 0)
                          (0 4 0 0 8 3 2 0 0)
                          (0 0 8 9 2 0 0 3 0)))))

(((8) (5) (7) (2) (1) (6) (4) (9) (3))
 ((3) (6) (2) (7) (9) (4) (8) (1) (5))
 ((9) (1) (4) (3) (5) (8) (6) (2) (7))
 ((2) (9) (5) (8) (3) (7) (1) (4) (6))
 ((4) (3) (1) (5) (6) (2) (7) (8) (9))
 ((7) (8) (6) (1) (4) (9) (3) (5) (2))
 ((1) (2) (3) (4) (7) (5) (9) (6) (8))
 ((5) (4) (9) (6) (8) (3) (2) (7) (1))
 ((6) (7) (8) (9) (2) (1) (5) (3) (4)))
|#

;; See Jean-Charles Regin AAAI-94 Proceedings pp 362-367
;; "A filtering algorithm for constraints of difference in CSPs."

;; Make the bipartite graph
;; Mark the maximum matching
;; Check whether it's big enough
;; Make the directed version of the graph
;; Mark strongly connected components
;; Mark edges reachable from free vertices
;; Remove everything else

(define (all-different value-lists)
  (let* ((variable-vertices
           (map (lambda (l)
                  (make-vertex '()))
                value-lists))
         (value-vertices
           (map (lambda (v)
                  (make-vertex '() v))
                (delete-duplicates (apply append value-lists))))
         (edges (append-map
                  (lambda (variable value-list)
                    (map (lambda (value)
                           (create-edge!
                             (list variable
                                   (find (lambda (vert)
                                           (eq? value (vertex-name vert)))
                                         value-vertices))))
                         value-list))
                  variable-vertices value-lists))
         (the-graph (make-graph (append variable-vertices value-vertices) edges)))
    (let ((matching (maximal-bipartite-matching variable-vertices value-vertices edges)))
      (define (traversable? di-edge)
        (if (memq (di-edge-edge di-edge) matching)
          (memq (di-edge-tail di-edge) variable-vertices)
          (memq (di-edge-tail di-edge) value-vertices)))
      (define (di-edge-length di-edge)
        (if (traversable? di-edge)
          1
          #f))
      (if (= (length variable-vertices)
             (length matching))
        ;; OK, proceed
        (let ((scc (strongly-connected-components the-graph traversable?))
              (free-vertices (lset-difference eq?
                                              ;; None of the variable vertices can be free
                                              value-vertices (append-map edge-vertices matching))))
          (define (representative vertex)
            (hash-table/get scc vertex (list 'unique-frob)))
          (define strongly-connected-edges
            (filter (lambda (e)
                      (eq? (representative (edge-vertex1 e))
                           (representative (edge-vertex2 e))))
                    edges))
          (receive (discovery-times ignore1 ignore2) (full-dfs free-vertices traversable?)
                   (define reachable-vertices
                     (filter (lambda (v)
                               (hash-table/get discovery-times v #f))
                             (append variable-vertices value-vertices)))
                   (define reachable-edges
                     (append-map (lambda (v)
                                   (filter (lambda (e)
                                             (traversable? (directed-version e v)))
                                           (vertex-edges v)))
                                 reachable-vertices))
                   (define overall-acceptable-edges
                     (delete-duplicates (append matching strongly-connected-edges reachable-edges)))
                   (define (acceptable-incident-edges vertex)
                     (filter (lambda (e)
                               (incident? vertex e))
                             overall-acceptable-edges))
                   (define (acceptable-values vertex)
                     (map vertex-name (map (lambda (e)
                                             (edge-other-end e vertex))
                                           (acceptable-incident-edges vertex))))
                   (map acceptable-values variable-vertices)))
        ;; Biggest matching is not big enough; fail
        (map (lambda (l) '()) value-lists)))))

(define (same-value-lists? vlists1 vlists2)
  (equal? (map (lambda (l) (sort l <)) vlists1)
          (map (lambda (l) (sort l <)) vlists2)))

(define-each-check
  (same-value-lists? '((1 2) (1 2))     (all-different '((1 2) (1 2))))
  (same-value-lists? '((1 2) (1 2) (3)) (all-different '((1 2) (1 2) (1 2 3))))
  (same-value-lists? '(() () ())        (all-different '((1 2) (1 2) (1 2))))
  (same-value-lists? '((1) (2 3) (2 3)) (all-different '((1) (1 2 3) (1 2 3)))))
