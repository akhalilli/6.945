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
