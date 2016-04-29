;;;; Utilities

(define ((compose f g) x) (f (g x)))
(define (identity x) x)
(define (compose* . functions)
  (if (pair? functions)
    (compose (car functions)
             (apply compose* (cdr functions)))
    identity))

(define-syntax cons-stream*
  (syntax-rules ()
                ((_) the-empty-stream)
                ((_ first) (stream first))
                ((_ first second ...)
                 (cons-stream first (cons-stream* second ...)))))

(define-syntax define-memoized
  (syntax-rules ()
                ((_ (f args ...) bodies ...)
                 (define f
                   (let ((results (make-equal-hash-table)))
                     (lambda (args ...)
                       ((lambda vals
                          (hash-table/lookup results vals
                                             identity
                                             (lambda ()
                                               (let ((result (begin bodies ...)))
                                                 (hash-table/put! results vals result)
                                                 result))))
                        args ...)))))))

(define (value x)
  (if (promise? x) (force x) x))

(define (streams->lists x #!optional depth)
  (let ((x (value x))
        (depth (if (default-object? depth) 1 depth)))
    (cond
      ((= 0 depth) x)
      ((not (pair? x)) x)
      (else (cons (streams->lists (car x) (-1+ depth))
                  (streams->lists (cdr x) depth))))))

;;;; Vertex and edge structures and creation

(define-structure (vertex (constructor %make-vertex))
                  edges
                  name
                  graph)
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

(define-syntax edges
  (syntax-rules ()
                ((_ tail (args ...) ...)
                 (cons-stream* 'edges (make-edge tail args ...) ...))))
(define edges->stream stream-cdr)
(define edges->list (compose stream->list edges->stream))
(define vertex-edges-stream (compose edges->stream vertex-edges))

(define-structure (edge (keyword-constructor %make-edge))
                  tail
                  head
                  (name 'edge)
                  graph
                  (weight 1)
                  (capacity 1))
(set-record-type-unparser-method!
  rtd:edge
  (standard-unparser-method 'edge (lambda (obj port)
                                    (write-char #\  port)
                                    (write (edge-name obj) port))))

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

;;;; Lattice
(define-memoized (lattice x y)
                 (make-vertex
                   (edges (lattice x y)
                          ((lattice (1+ x) y))
                          ((lattice x (1+ y)))
                          ((lattice (-1+ x) y))
                          ((lattice x (-1+ y))))
                   (list x y)))

;;;; Adapted from gjs
(define (sort-by object property)
  (sort object (lambda (a b) (< (property a) (property b)))))
(define (shortest-path-tree source sink)
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
          (let ((length (edge-weight edge)))
            (if (not length)
              ;; Edge not allowed
              'ok
              (merge-distance! (edge-head edge)
                               (+ distance length)
                               (edge-tail edge)))))
        (assert distance)
        (for-each relax! (stream->list (vertex-edges-stream vertex)))
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
        (if (and next (not (done? sink)))
          (begin (process! (next-vertex))
                 (loop)))))
    answer))

#|

;;;; Shortest path example with infinite graphs

(define example-shortest-paths
  (shortest-path-tree (lattice 0 0) (lattice 3 5)))

(hash-table/get example-shortest-paths (lattice 3 5) #f)
;Value: (8 . #[vertex 14 (3 4)])

(hash-table/count example-shortest-paths)
;Value: 155

|#
