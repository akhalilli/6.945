(declare (usual-integrations))

;;;; Vertex and edge structures and creation

(define-structure (vertex (constructor %make-vertex))
                  edges
                  name
                  graph)

(define (make-vertex edges #!optional name graph)
  (%make-vertex
    edges
    (if (default-object? name)
      (generate-uninterned-symbol 'vertex)
      name)
    (if (default-object? graph)
      #f
      graph)))

(define-structure (edge (constructor %make-edge))
                  vertices
                  name
                  graph
                  directed-versions)

(define (make-edge vertices #!optional name graph)
  (%compute-directed-versions!
    (%make-edge
      vertices
      (if (default-object? name)
        (cons '-- (map vertex-name vertices))
        name)
      (if (default-object? graph)
        #f
        graph)
      #f)))

(define (%compute-directed-versions! edge)
  (set-edge-directed-versions!
    edge (map (lambda (v) (cons v (%make-di-edge edge v)))
              (edge-vertices edge)))
  edge)

(define-structure (di-edge (constructor %make-di-edge))
                  edge
                  tail)

(define (directed-version edge tail)
  (cdr (assq tail (edge-directed-versions edge))))

(define (attach-edge! edge)
  (for-each (lambda (v)
              (%attach-edge! v edge))
            (edge-vertices edge))
  edge)

(define (%attach-edge! vertex edge)
  (if (not (memq edge (vertex-edges vertex)))
    (set-vertex-edges! vertex (cons edge (vertex-edges vertex)))))

(define (create-edge! vertices)
  (attach-edge! (make-edge vertices)))

(define (edge-malformation edge)
  (apply boolean/or
         (map (lambda (vertex)
                (and (not (memq edge (vertex-edges vertex)))
                     `(,edge not attached to ,vertex)))
              (edge-vertices edge))))

;;;; Vertex and edge queries

(define (edge-vertex1 edge)
  (car (edge-vertices edge)))

(define (edge-vertex2 edge)
  (cadr (edge-vertices edge)))

(define (incident? vertex edge)
  (memq vertex (edge-vertices edge)))

(define (hyper? edge)
  (not (= 2 (length (edge-vertices edge)))))

(define (edge-other-end edge vertex)
  (assert (incident? vertex edge))
  (assert (not (hyper? edge)))
  (if (eq? vertex (edge-vertex1 edge))
    (edge-vertex2 edge)
    (edge-vertex1 edge)))

(define (forward-di-edge edge)
  (directed-version edge (edge-vertex1 edge)))

(define (reverse-di-edge edge)
  (directed-version edge (edge-vertex2 edge)))

(define (di-edge-head di-edge)
  (edge-other-end (di-edge-edge di-edge) (di-edge-tail di-edge)))

(define (di-edge-reverse di-edge)
  (directed-version (di-edge-edge di-edge) (di-edge-head di-edge)))

(define (di-edge-name di-edge)
  `(-> ,(vertex-name (di-edge-tail di-edge))
       ,(vertex-name (di-edge-head di-edge))))

;;;; Graph structure and creation

(define-structure (graph (constructor %make-graph))
                  vertices
                  edges)

(define (make-graph vertices edges)
  (ensure-kosher! (%make-graph vertices edges)))

(define (edges-of-vertices graph)
  (delete-duplicates (append-map vertex-edges (graph-vertices graph))))

(define (vertices-of-edges graph)
  (delete-duplicates (append-map edge-vertices (graph-edges graph))))

(define (graph-malformation graph)
  (or (apply boolean/or
             (map (lambda (v)
                    (and (not (eq? graph (vertex-graph v)))
                         `(,v not introduced to ,graph)))
                  (graph-vertices graph)))
      (apply boolean/or
             (map (lambda (e)
                    (and (not (eq? graph (edge-graph e)))
                         `(,e not introduced to ,graph)))
                  (graph-edges graph)))
      (apply boolean/or
             (map edge-malformation (graph-edges graph)))
      (let ((discrepant-vertices
              (lset-xor eq?
                        (filter (lambda (v)
                                  (not (null? (vertex-edges v))))
                                (graph-vertices graph))
                        (vertices-of-edges graph))))
        (and (not (null? discrepant-vertices))
             (map vertex-name discrepant-vertices)))
      (let ((discrepant-edges
              (lset-xor eq?
                        (graph-edges graph)
                        (edges-of-vertices graph))))
        (and (not (null? discrepant-edges))
             (map edge-name discrepant-edges)))))

(define (introduce-graph-to-vertex! v graph)
  (cond ((eq? (vertex-graph v) graph) 'ok)
        ((not (vertex-graph v))
         (set-vertex-graph! v graph))
        (else
          (error "Graph clash" v graph))))

(define (introduce-graph-to-edge! e graph)
  (cond ((eq? (edge-graph e) graph) 'ok)
        ((not (edge-graph e))
         (set-edge-graph! e graph))
        (else
          (error "Graph clash" e graph))))

(define (ensure-kosher! graph)
  (for-each (lambda (v)
              (introduce-graph-to-vertex! v graph))
            (graph-vertices graph))
  (for-each (lambda (e)
              (introduce-graph-to-edge! e graph))
            (graph-edges graph))
  (for-each attach-edge! (graph-edges graph))
  (let ((malformation (graph-malformation graph)))
    (if malformation
      (error "Not kosher!" malformation)))
  graph)

(define (copy-graph graph)
  (define new-vertex-map
    (map (lambda (v)
           (cons v (make-vertex #f (vertex-name v))))
         (graph-vertices graph)))
  (define (new-vertex vertex)
    (cdr (assq vertex new-vertex-map)))
  (define new-edge-map
    (map (lambda (e)
           (cons e (make-edge (map new-vertex (edge-vertices e)) (edge-name e))))
         (graph-edges graph)))
  (define (new-edge edge)
    (cdr (assq edge new-edge-map)))
  (for-each (lambda (v)
              (set-vertex-edges! (new-vertex v)
                                 (map new-edge (vertex-edges v))))
            (graph-vertices graph))
  (make-graph (map cdr new-vertex-map)
              (map cdr new-edge-map)))

(define (graph-vertex graph name)
  (find (lambda (vertex)
          (eq? name (vertex-name vertex)))
        (graph-vertices graph)))

(define (graph-edge graph name)
  (find (lambda (edge)
          (eq? name (edge-name edge)))
        (graph-edges graph)))

;;;; Graph creation syntax
(define-syntax let-vertices
  (syntax-rules ()
                ((let-vertices (name ...) body-form ...)
                 (let ((name (make-vertex '() 'name)) ...)
                   body-form ...))))

(define-syntax let-edges* ;; TODO Name this
  (syntax-rules ()
                ((let-edges* ((edge-name vertex ...) ...) body-form ...)
                 (let ((edge-name (make-edge (list vertex ...))) ...)
                   body-form ...))))

(define (conventional-edge-variable-name vertex-list)
  (string->symbol
    (apply string-append
           (let loop ((vertex-list (map symbol->string vertex-list)))
             (cond ((null? vertex-list)
                    '())
                   ((null? (cdr vertex-list))
                    vertex-list)
                   (else
                     (cons (car vertex-list)
                           (cons "-" (loop (cdr vertex-list))))))))))

;;; TODO How in the world do I test that this thing is properly
;;; hygienic?
(define-syntax let-edges
  (sc-macro-transformer
    (lambda (form use-env)
      (if (syntax-match? '(IDENTIFIER (* (IDENTIFIER IDENTIFIER)) . (* EXPRESSION)) form)
        (let ((edge-vertices (cadr form))
              (body (cddr form)))
          (define (wrap thing)
            (close-syntax thing use-env))
          (let ((names (map conventional-edge-variable-name edge-vertices)))
            `(let-edges*
               ,(map (lambda (name vlist)
                       (cons name (map wrap vlist)))
                     names
                     edge-vertices)
               ;; TODO Why do I have to make this let?
               ,(make-syntactic-closure use-env names `(let () ,@body)))))
        (ill-formed-syntax form)))))

;;; TODO Add syntax for vertices that have no edges, thus:
;;; (let-graph ((v1 v2) v3 (v4 v5)) ...)
;;; TODO How can I test whether this macro is hygienic?
(define-syntax let-graph
  (sc-macro-transformer
    (lambda (form use-env)
      (if (syntax-match? '(IDENTIFIER (* (IDENTIFIER IDENTIFIER)) . (* EXPRESSION)) form)
        (let ((edge-vertices (cadr form))
              (body (cddr form)))
          (let ((vertex-names (delete-duplicates (apply append edge-vertices)))
                (edge-names (map conventional-edge-variable-name edge-vertices)))
            `(let-vertices ,vertex-names
                           (let-edges ,edge-vertices
                                      ,@(append
                                          body
                                          (list `(make-graph (list ,@vertex-names) (list ,@edge-names))))))))
        (ill-formed-syntax form)))))

;;; Example Graph

(define (clr-graph)
  (let-graph ((s v1) (s v2) (v1 v2)
                     ;; (v2 v1)
                     (v1 v3) (v3 v2) (v2 v4) (v4 v3) (v3 t) (v4 t))
             (define (capacity! edge tail amount)
               (eq-put! (directed-version edge tail) 'capacity amount))
             (capacity! s-v1  s  16)
             (capacity! s-v2  s  13)
             (capacity! v1-v2 v1 10)
             (capacity! v1-v2 v2 4)
             (capacity! v1-v3 v1 12)
             (capacity! v3-v2 v3 9)
             (capacity! v2-v4 v2 14)
             (capacity! v4-v3 v4 7)
             (capacity! v3-t  v3 20)
             (capacity! v4-t  v4 4)))

;;;; Shortest paths

;; Edge-length should be a procedure that accepts a di-edge.  It
;; should return the length of the supplied di-edge, or #f if the edge
;; should not be traversed (in that direction).

;; The answer comes back as an eq-hash of vertices to pairs
;; (distance-to-here . predecessor-edge).

;; This is Dijkstra's algorithm except that we are using a hash table
;; instead of a priority queue.  This causes the asymptotic runtime to
;; be worse by an extra factor of |V| (because of the sort in
;; next-vertex below).
(define (shortest-path-tree source edge-length)
  (let ((done (make-eq-hash-table))
        (answer (make-eq-hash-table)))
    (define (done? vertex)
      (hash-table/get done vertex #f))
    (define (pending? vertex)
      (hash-table/get answer vertex #f))
    (define (distance vertex)
      (car (hash-table/get answer vertex #f)))
    (define (predecessor vertex)
      (cdr (hash-table/get answer vertex #f)))
    (define (clobber! vertex new-data)
      (hash-table/put! answer vertex new-data))
    (define (merge-distance! vertex new-distance predecessor)
      (cond ((done? vertex) 'ok)
            ((pending? vertex)
             (if (< new-distance (distance vertex))
               (clobber! vertex (cons new-distance predecessor))
               'ok))
            (else
              (clobber! vertex (cons new-distance predecessor)))))
    (define (process! vertex)
      (let ((distance (distance vertex)))
        (define (relax! edge)
          (let* ((putative-di-edge (directed-version edge vertex))
                 (length (edge-length putative-di-edge)))
            (if (not length)
              ;; Edge not allowed
              'ok
              (merge-distance! (di-edge-head putative-di-edge)
                               (+ distance length)
                               putative-di-edge))))
        (assert distance)
        (for-each relax! (vertex-edges vertex))
        (hash-table/put! done vertex #t)))
    (define (next-vertex)
      (let ((candidates (filter (lambda (v)
                                  (not (done? v)))
                                (hash-table/key-list answer))))
        (if (null? candidates)
          #f
          (car (sort-by candidates distance)))))
    (clobber! source (cons 0 #f))
    (let loop ()
      (let ((next (next-vertex)))
        (if next
          (begin (process! (next-vertex))
                 (loop)))))
    answer))

(define-test (example-shortest-paths)
             (let* ((graph (clr-graph))
                    (vertices (graph-vertices graph))
                    (tree (shortest-path-tree (car vertices) (lambda (e) 1))))
               (check (equal? '(0 1 1 2 2 3)
                              (map (lambda (v)
                                     (car (hash-table/get tree v #f)))
                                   vertices)))
               ;; TODO Check predecessors
               ))

(define (shortest-path source target edge-length)
  (let ((tree (shortest-path-tree source edge-length)))
    (define (lookup vertex)
      (hash-table/get tree vertex #f))
    (define (predecessor vertex)
      (let ((info (lookup vertex)))
        (assert info)
        (cdr info)))
    (if (lookup target)
      (let loop ((tail target)
                 (path '()))
        (let ((pred (predecessor tail)))
          (if pred
            (loop (di-edge-tail pred)
                  (cons pred path))
            path)))
      #f)))

;; TODO Separate data structure for paths?  And path trees, for that matter?
(define-test (example-shortest-path)
             (let* ((graph (clr-graph))
                    (vertices (graph-vertices graph))
                    (source (car vertices))
                    (target (car (last-pair vertices)))
                    (path (shortest-path source target (lambda (e) 1))))
               (define (real-path? path)
                 (let loop ((tail (di-edge-tail (car path)))
                            (rest path))
                   (if (null? rest)
                     #t
                     (and (eq? tail (di-edge-tail (car rest)))
                          (loop (di-edge-head (car rest))
                                (cdr rest))))))
               (check (real-path? path))
               (check (eq? source (di-edge-tail (car path))))
               (check (eq? target (di-edge-head (car (last-pair path)))))))

;;;; Maximum Flow

;; Edge-capacity should be a procedure that accepts a di-edge.  It
;; should return the capacity of the supplied edge.  (Zero is an
;; acceptable (in-)capacity).
(define (max-flow source sink edge-capacity)
  (let ((answer (make-eq-hash-table)))
    (define (loop)
      (let ((path (augmenting-path)))
        (if path
          (begin (apply-augmenting-path! path)
                 (loop))
          answer)))
    (define (augmenting-path)
      (shortest-path source sink
                     (lambda (di-edge)
                       (if (> (residual-capacity di-edge) 0)
                         1
                         #f))))
    (define (apply-augmenting-path! path)
      (let ((capacity (capacity path)))
        (for-each (lambda (di-edge)
                    (add-flow! di-edge capacity))
                  path)))
    (define (capacity path)
      (apply min (map residual-capacity path)))
    (define (flow di-edge)
      (hash-table/get answer di-edge 0))
    (define (residual-capacity di-edge)
      (+ (- (edge-capacity di-edge) (flow di-edge))
         (flow (di-edge-reverse di-edge))))
    (define (add-flow! di-edge amount)
      (%add-flow! di-edge amount)
      (normalize-flows! di-edge))
    ;; Maintain the invariant that of any pair of mutually-reverse
    ;; di-edges, at most one ever has non-zero flow.
    (define (normalize-flows! di-edge)
      (let ((reduction (min (flow di-edge) (flow (di-edge-reverse di-edge)))))
        (%add-flow! di-edge (- reduction))
        (%add-flow! (di-edge-reverse di-edge) (- reduction))))
    (define (%add-flow! di-edge amount)
      (hash-table/put! answer di-edge (+ amount (flow di-edge))))
    (loop)))

(define (sensible-flow? flow source sink edge-capacity)
  (define (edge-flow di-edge)
    (hash-table/get flow di-edge 0))
  (define (net-edge-flow di-edge)
    (- (edge-flow di-edge)
       (edge-flow (di-edge-reverse di-edge))))
  (define (no-reverse-flow di-edge)
    (or (= 0 (edge-flow di-edge))
        (= 0 (edge-flow (di-edge-reverse di-edge)))))
  (define (capacity-respected? di-edge)
    (<= (edge-flow di-edge)
        (edge-capacity di-edge)))
  (define (edge-ok? di-edge)
    (and (no-reverse-flow di-edge)
         (capacity-respected? di-edge)))
  (define (edges-ok?)
    (apply boolean/and
           (map edge-ok? (hash-table/key-list flow))))
  (define (vertex-flow vertex)
    (apply + (map net-edge-flow (map (lambda (edge)
                                       (directed-version edge vertex))
                                     (vertex-edges vertex)))))
  (define (interior-vertex-ok? vertex)
    (= 0 (vertex-flow vertex)))
  (define (flow-conserved?)
    (let ((interior-vertices
            (delq source (delq sink (graph-vertices (vertex-graph source))))))
      (and (= (vertex-flow source)
              (- (vertex-flow sink)))
           (apply boolean/and
                  (map interior-vertex-ok? interior-vertices)))))
  (and (edges-ok?)
       (flow-conserved?)))

(define-test (example-max-flow)
             (interaction
               (define graph (clr-graph))
               (define vertices (graph-vertices graph))
               (define source (car vertices))
               (define target (car (last-pair vertices)))
               (define flow (max-flow source target (lambda (e) (or (eq-get e 'capacity) 0))))
               (check (sensible-flow? flow source target (lambda (e) (or (eq-get e 'capacity) 0))))
               (map (lambda (edge tail)
                      (hash-table/get flow (directed-version edge tail) 0))
                    (graph-edges graph)
                    (map (lambda (name)
                           (graph-vertex graph name))
                         '(s s v1 v1 v3 v2 v4 v3 v4)))
               (produces '(12 11 0 12 0 11 7 19 4))))

;;;; Maximal bipartite matching

(define (maximal-bipartite-matching left-side right-side edges)
  (let* ((fresh-graph
           (copy-graph (vertex-graph (car left-side)))) ;; TODO Check for empty left side
         (fresh-source (make-vertex '() (list 'source) fresh-graph))
         (fresh-sink (make-vertex '() (list 'sink) fresh-graph))
         (left-names (map vertex-name left-side))
         (right-names (map vertex-name right-side)))
    (define (left? fresh-vertex)
      (memq (vertex-name fresh-vertex) left-names))
    (define (right? fresh-vertex)
      (memq (vertex-name fresh-vertex) right-names))
    (define (rightward? e)
      (or (and (eq? fresh-source (di-edge-tail e)) (left? (di-edge-head e)))
          (and (left? (di-edge-tail e)) (right? (di-edge-head e)))
          (and (right? (di-edge-tail e)) (eq? fresh-sink (di-edge-head e)))))
    (for-each (lambda (left)
                (create-edge! (list fresh-source (graph-vertex fresh-graph (vertex-name left)))))
              left-side)
    (for-each (lambda (right)
                (create-edge! (list (graph-vertex fresh-graph (vertex-name right)) fresh-sink)))
              right-side)
    (let ((flow (max-flow fresh-source fresh-sink
                          (lambda (e) (if (rightward? e) 1 0)))))
      (filter (lambda (edge)
                (let ((fresh-edge (graph-edge fresh-graph (edge-name edge))))
                  (or (= 1 (hash-table/get flow (forward-di-edge fresh-edge) 0))
                      (= 1 (hash-table/get flow (reverse-di-edge fresh-edge) 0)))))
              edges))))

(define (clr-graph2)
  (let-graph ((l1 r1) (l2 r1) (l2 r3) (l3 r2)
                      (l3 r3) (l3 r4) (l4 r3) (l5 r3))))

(define (sensible-matching? edges)
  (let ((vertices (append-map edge-vertices edges)))
    (= (length (delete-duplicates vertices eq?))
       (* 2 (length edges)))))

(define (in-graph? edge graph)
  (memq edge (graph-edges graph)))

(define-test (example-matching)
             (interaction
               (define the-graph (clr-graph2))
               (define the-matching
                 (maximal-bipartite-matching
                   (map (lambda (name) (graph-vertex the-graph name)) '(l1 l2 l3 l4 l5))
                   (map (lambda (name) (graph-vertex the-graph name)) '(r1 r2 r3 r4))
                   (graph-edges the-graph)))
               (check (sensible-matching? the-matching))
               (check (apply boolean/and (map (lambda (e) (in-graph? e the-graph)) the-matching)))
               (length the-matching)
               (produces 3)))

;;;; Depth First Search through graphs

(define (full-dfs roots traversable?)
  (let* ((discovery-times (make-eq-hash-table))
         (finishing-times (make-eq-hash-table))
         (predecessors (make-eq-hash-table))
         (time 1))
    (define (undiscovered? v)
      (not (hash-table/get discovery-times v #f)))
    (define (discover! v)
      (hash-table/put! discovery-times v time)
      (set! time (+ time 1)))
    (define (finish! v)
      (hash-table/put! finishing-times v time)
      (set! time (+ time 1)))
    (define (visit v)
      (discover! v)
      (for-each (lambda (u)
                  (hash-table/put! predecessors u v)
                  ;; TODO What if visiting u1 causes a later u2 to become
                  ;; discovered?
                  (visit u))
                (filter undiscovered?
                        (map di-edge-head
                             (filter traversable?
                                     (map (lambda (e)
                                            (directed-version e v))
                                          (vertex-edges v))))))
      (finish! v))
    (let loop ((remainder roots))
      (cond ((null? remainder)
             (values discovery-times finishing-times predecessors))
            ((undiscovered? (car remainder))
             (visit (car remainder))
             (loop (cdr remainder)))
            (else
              (loop (cdr remainder)))))))

;;;; Strongly Connected Components

(define (strongly-connected-components graph traversable?)
  (receive (ignore1 finishing-times ignore2) (full-dfs (graph-vertices graph) traversable?)
           (define (second-dfs-order v1 v2)
             (> (hash-table/get finishing-times v1 0)
                (hash-table/get finishing-times v2 0)))
           (receive (ignore1 ignore2 predecessors)
                    (full-dfs (sort (graph-vertices graph) second-dfs-order)
                              (lambda (de)
                                (traversable? (di-edge-reverse de))))
                    (let ((representatives (make-eq-hash-table)))
                      (define (top-predecessor v)
                        (let ((predecessor (hash-table/get predecessors v #f)))
                          (if predecessor
                            (top-predecessor predecessor)
                            v)))
                      (for-each (lambda (v)
                                  (hash-table/put! representatives v (top-predecessor v)))
                                (graph-vertices graph))
                      representatives))))

(define (clr-graph3)
  ;; This is from page 553 of CLRS second edition.
  (let-graph ((a b) (b c) (b e) (b f) (c d) (c g) #;(d c) (d h)
                    (e a) (e f) (f g) #;(g f) (g h) (h h)
                    )
             (define (-> vertex . edges)
               (for-each (lambda (edge)
                           (eq-put! (directed-version edge vertex) 'traversable #t))
                         edges))
             (-> a a-b)
             (-> b b-c b-e b-f)
             (-> c c-d c-g)
             (-> d c-d d-h)
             (-> e e-a e-f)
             (-> f f-g)
             (-> g f-g g-h)
             (-> h h-h)))

(define-test (example-strongly-connected-components)
             (define the-graph (clr-graph3))
             (define the-scc
               (strongly-connected-components
                 the-graph
                 (lambda (di-edge)
                   (eq-get di-edge 'traversable))))
             (define (dump-scc scc-table)
               (sort (map (lambda (component)
                            (sort (map vertex-name (map car component)) symbol<?))
                          (group-by (hash-table->alist scc-table) cdr))
                     (lexicgraphic-order symbol<?)))
             (check (equal? '((a b e) (c d) (f g) (h)) (dump-scc the-scc)))
             )

;;; TODO Abstract commonalities of treatment of vertices and edges
;;; TODO Nicety: draw dot pictures of these graphs
;;; TODO Generalize to nary-memoize-eq (and unary?)
;;; TODO Worry about eq-ness of generated names, vs uniqueness of names in a graph
;;; TODO Abstract common operations on answer structures (like "get the flow through this edge", etc)
