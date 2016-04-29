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
