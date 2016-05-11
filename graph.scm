;;;; Vertex and edge structures and creation

;;; A vertex consists of a stream of edges (prepended so that the first edge is also delayed)
;;; and an identifier/name. The identifier is the argument in the definition of the procedure
;;; creating a vertex. The name field in the record is used mostly for printing purposes.
;;; The graph field is sometimes used to help position a vertex when displaying it graphically.
(define-structure (vertex (constructor %make-vertex))
                  edges
                  name
                  graph)

;;; Printer for vertex
(set-record-type-unparser-method!
  rtd:vertex
  (standard-unparser-method 'vertex (lambda (obj port)
                                      (write-char #\  port)
                                      (write (vertex-name obj) port))))

(define (make-vertex edges #!optional name graph)
  (%make-vertex
    edges
    (if (default-object? name)
      (generate-uninterned-symbol 'vertex)
      name)
    (if (default-object? graph)
      #f
      graph)))

;;; Syntactic sugar for defining a bunch of edges with the same tail,
;;; eg. for outgoing edges from a vertex.
;;; Example:
;;;   ('edges tail
;;;           (head-1 [arguments like weight, etc])
;;;           ...
;;;           (head-k [arguments like weight, etc]))
;;; See lattice.scm or symmetric.scm for more examples.
;;; This only works for explicit edges. For more power, generate a stream of edges directly.
;;; (Examples of this include the complete graph in symmetric.scm and the graphs in random.scm.)
(define-syntax edges
  (syntax-rules ()
                ((_ tail (args ...) ...)
                 (cons-stream* 'edges (make-edge tail args ...) ...))))
(define-syntax make-edges
  (syntax-rules ()
                ((_ edges-stream)
                 (cons-stream 'edges edges-stream))))

;;; Modifiers and accessors for edge streams
(define edges->stream stream-cdr)
(define edges->list (compose stream->list edges->stream))
(define vertex-edges-stream (compose edges->stream vertex-edges))
(define vertex-edges-list (compose stream->list vertex-edges-stream))
(define (set-vertex-edges-stream! vertex edges-stream)
  (set-vertex-edges! vertex (make-edges edges-stream)))

;;; A (directed) edge consists of a tail vertex and a head vertex,
;;; where the edges goes from the tail to the head.
;;; In addition, edges can store other properties like weight and capacity (default 1).
;;; For consistency with the vertex record, we have a graph field, but it isn't used.
(define-structure (edge (keyword-constructor %make-edge))
                  tail
                  head
                  (name 'edge)
                  graph
                  (weight 1)
                  (capacity 1))

;;; Printer for edge
(set-record-type-unparser-method!
  rtd:edge
  (standard-unparser-method 'edge (lambda (obj port)
                                    (write-char #\  port)
                                    (write (edge-name obj) port))))

;;; This parses the arguments to create an edge.
;;; The default name is a list containing the tail and head (prepended by '--).
;;; Examples:
;;;   (make-edge a b 'edge) -> (%make-edge 'tail a 'head b 'name 'edge)
;;;   (make-edge a b 'w 3 'c 7) -> (%make-edge 'tail a 'head b 'name [name] 'weight 3 'capacity 7)
;;;   (make-edge a b 'c 7 'w 3) -> (%make-edge 'tail a 'head b 'name [name] 'weight 3 'capacity 7)
;;;   (make-edge a b 'w 3 'name 'edge) -> (%make-edge 'tail a 'head b 'name 'edge 'weight 3)
(define (make-edge tail head . rest)
  (apply %make-edge
         (cons* 'tail tail 'head head
                (let parse
                  ((fields (list-tail (record-type-field-names rtd:edge) 2))
                   (rest rest))
                  (define (find-prefix symbol fields)
                    (list-index (lambda (x)
                                  (string-prefix? (symbol->string symbol)
                                                  (symbol->string x)))
                                fields))
                  (cond
                    ((null? rest)
                     (if (and (pair? fields) (eq? 'name (car fields)))
                       (list 'name (list '-- (vertex-name tail) (vertex-name head)))
                       '()))
                    ((and (symbol? (car rest))
                          (find-prefix (car rest) fields))
                     (let ((ref (find-prefix (car rest) fields)))
                       (cons* (list-ref fields ref)
                              (cadr rest)
                              (parse (append (list-head fields ref)
                                             (list-tail fields (1+ ref)))
                                     (cddr rest)))))
                    (else
                      (cons* (car fields)
                             (car rest)
                             (parse (cdr fields)
                                    (cdr rest)))))))))

;;; Be careful. Adding or removing an edge from a vertex with infinite degree will not return.
(define (add-edge! edge)
  (let ((tail (edge-tail edge)))
    (if (not (memq edge (vertex-edges-list tail)))
      (let ((edges-stream (vertex-edges-stream tail)))
        (set-vertex-edges-stream! tail (cons-stream edge edges-stream))))))

(define (remove-edge! edge)
  (let ((tail (edge-tail edge)))
    (if (memq edge (vertex-edges-list tail))
      (let ((edges-stream (stream-filter (lambda (x) (not (eq? edge x)))
                                         (vertex-edges-stream tail))))
        (set-vertex-edges-stream! tail edges-stream)))))

;;; Traverses edges by using the ith edge in the vertex's stream of edges.
;;; seq is a list of indices to trace edges.
;;; Use #f to choose randomly (must have finite edges)
;;; Random goes nowhere if the vertex has no edges.
(define (traverse vertex seq)
  (if (null? seq)
    vertex
    (traverse (let* ((edge-stream (vertex-edges-stream vertex))
                     (first (car seq))
                     (length (stream-length edge-stream)))
                (cond
                  (first (edge-head (stream-ref edge-stream first)))
                  ((not (eq? 0 length)) (edge-head (stream-ref edge-stream (random length))))
                  (else vertex)))
              (cdr seq))))

;;; Traverse k edges randomly.
;;; Goes nowhere if no edges.
;;; This can also be defined in terms of traverse or traverse-random-weighted,
;;; but is written here to demonstrate its simplicity.
(define (traverse-random vertex #!optional k)
  (let ((k (if (default-object? k) 1 k))
        (edges-stream (vertex-edges-stream vertex)))
    (if (or (eq? 0 k)
            (null? edges-stream))
      vertex
      (traverse-random
        (edge-head (stream-ref edges-stream
                               (random (stream-length edges-stream))))
        (-1+ k)))))

;;; Traverse k edges randomly in proportion to edge weights.
(define (traverse-random-weighted vertex #!optional k)
  (let ((k (if (default-object? k) 1 k)))
    (if (eq? 0 k)
      vertex
      (traverse-random-weighted
        (let iter ((edges-stream (vertex-edges-stream vertex))
                   (head vertex)
                   (sum 0))
          (if (null? edges-stream)
            head
            (let* ((edge (stream-car edges-stream))
                   (weight (edge-weight edge))
                   (sum (+ sum weight)))
              (iter (stream-cdr edges-stream)
                    (if (> 1 (random (exact->inexact (/ sum weight)))) (edge-head edge) head)
                    sum))))
        (-1+ k)))))

;;; Perform a dfs on a graph, starting from vertex.
;;; f is a function to be applied to all vertices.
;;; The sum of f on all vertices is returned.
(define ((graph-dfs f) vertex)
  (let ((done (make-eq-hash-table))
        (count 0))
    (define (dfs vertex)
      (if (not (hash-table/get done vertex #f))
        (begin
          (set! count (+ count (f vertex)))
          (hash-table/put! done vertex #t)
          (stream-for-each
            dfs
            (stream-map edge-head (vertex-edges-stream vertex))))))
    (dfs vertex)
    count))

;;; Count the number of vertices or edges reachable from a vertex
(define count-graph-vertices
  (graph-dfs (lambda (x) 1)))
(define count-graph-edges
  (graph-dfs (compose stream-length vertex-edges-stream)))
