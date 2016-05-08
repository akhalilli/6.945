;;;; Infinite line graph
(define-memoized (line x)
                 (make-vertex
                   (edges (line x)
                          ((line (1+ x)))
                          ((line (-1+ x))))
                   x))

;; steps to reach sink from source
(define (reach source sink #!optional limit count)
  (let ((limit (if (default-object? limit) #f limit))
        (count (if (default-object? count) 0 count)))
    (cond
      ((eq? source sink) count)
      ((eq? 0 limit) #f)
      (else (reach
              (traverse-random source)
              sink
              (and limit (-1+ limit))
              (1+ count))))))

(traverse-random (line 0) 500)
;Value: #[vertex 32]

(reach (line 0) (line 5))
;Value: 65
(reach (line 0) (line 20))
;Value: 940
