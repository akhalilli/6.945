(in-test-group
  graphs

  (define (matching-no-leak)
    (define (do-the-matching)
      (define the-graph (clr-graph2))
      (define the-matching
        (maximal-bipartite-matching
          (map (lambda (name) (graph-vertex the-graph name)) '(l1 l2 l3 l4 l5))
          (map (lambda (name) (graph-vertex the-graph name)) '(r1 r2 r3 r4))
          (graph-edges the-graph)))
      (check (sensible-matching? the-matching))
      (produces 3 (length the-matching)))
    (check (< (memory-loss-from (repeated 20 do-the-matching)) 10)))

  (define-each-check
    (not (graph-malformation (clr-graph)))
    (not (graph-malformation (clr-graph2)))
    (not (graph-malformation (clr-graph3))))
  )
