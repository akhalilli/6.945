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
                     ; need to do this to capture both the names and the values
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
      (else (cons (streams->lists (value (car x)) (-1+ depth))
                  (streams->lists (value (cdr x)) depth))))))

(define lattice-label stream-car)
(define (lattice-edges vertex)
  (stream-map (lambda (x) (list vertex x)) (stream-cdr vertex)))
(define-memoized (lattice x y) ; TODO: memoize
  (cons-stream*
    (list x y)
    (lattice (1+ x) y)
    (lattice x (1+ y))
    (lattice (-1+ x) y)
    (lattice x (-1+ y))))

(define vertex-edges (compose stream->list lattice-edges))
(define vertex-label lattice-label)

;;; Adapted from gjs
(define (sort-by object property)
  (sort object (lambda (a b) (< (property a) (property b)))))
(define (shortest-path-tree source sink edge-length)
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
          (let ((length (edge-length edge)))
            (if (not length)
              ;; Edge not allowed
              'ok
              (merge-distance! (second edge)
                               (+ distance length)
                               (first edge)))))
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
        (if (and next (not (done? sink)))
          (begin (process! (next-vertex))
                 (loop)))))
    answer))

#| ;;; Proof of concept with infinite graphs

(define example-shortest-paths (shortest-path-tree (lattice 0 0) (lattice 3 5) (lambda (e) 1)))

(hash-table/get example-shortest-paths (lattice 3 5) #f)
;Value: (8 (3 4) . #[promise 14])

(hash-table/count example-shortest-paths)
;Value: 175

|#
