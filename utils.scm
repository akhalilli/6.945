;;;; Utilities

(define ((compose f g) x) (f (g x)))
(define (identity x) x)
(define (compose* . functions)
  (if (pair? functions)
    (compose (car functions)
             (apply compose* (cdr functions)))
    identity))

(define pi (* 4 (atan 1 1)))

;;; like stream, but arguments after the first are not evaluated
(define-syntax cons-stream*
  (syntax-rules ()
                ((_) the-empty-stream)
                ((_ first) (stream first))
                ((_ first second ...)
                 (cons-stream first (cons-stream* second ...)))))

;;; like define, but memoizizes results in a hash table keyed by the arguments.
;;; very useful for ensuring the exact same object is returned.
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

;;; infinite stream when n is #f (or not a nonnegative integer)
(define (stream-iota n)
  (define (start k)
    (if (eq? n k)
      '()
      (cons-stream k (start (1+ k)))))
  (start 0))

(define (sgn x)
  (cond
    ((eq? 0 x) 0)
    ((< 0 x) 1)
    (else -1)))

;;;; Linear algebra
;;; Without defining a complete matrix algebra, this is a little messy.
;;; This is heavily focused on operations for camera angle in graphics.

(define ((v-op op) . vecs)
  (if (eq? 1 (length vecs))
    (map op (car vecs))
    (let ((u (first vecs))
          (v (second vecs)))
      (cond
        ((every list? (list u v)) (map op u v))
        ((list? u) (map (lambda (x) (op v x)) u))
        ((list? v) (map (lambda (x) (op u x)) v))
        (else (op u v))))))
(define v:+ (v-op +))
(define v:- (v-op -))
(define v:* (v-op *))
(define v:/ (v-op /))

(define (dot u v)
  (if (every list? (list u v))
    (apply + (v:* u v))
    (v:* u v)))
(define (cross u v)
  (let ((u1 (first u))
        (u2 (second u))
        (u3 (third u))
        (v1 (first v))
        (v2 (second v))
        (v3 (third v)))
    (list (- (* u2 v3) (* u3 v2))
          (- (* u3 v1) (* u1 v3))
          (- (* u1 v2) (* u2 v1)))))

(define (sqr-norm u) (apply + (map square u)))
(define norm (compose sqrt sqr-norm))
(define (normalized u) (dot (/ (norm u)) u))
(define (proj u v) (v:* (/ (dot u v) (sqr-norm u)) u))

(define ((rotate-x theta) v)
  (list (first v)
        (+ (* (cos theta) (second v)) (* (- (sin theta)) (third v)))
        (+ (* (sin theta) (second v)) (* (cos theta) (third v)))))
(define ((rotate-y theta) v)
  (list (+ (* (cos theta) (first v)) (* (sin theta) (third v)))
        (second v)
        (+ (* (- (sin theta)) (first v)) (* (cos theta) (third v)))))
(define ((rotate-z theta) v)
  (list (+ (* (cos theta) (first v)) (* (- (sin theta)) (second v)))
        (+ (* (sin theta) (first v)) (* (cos theta) (second v)))
        (third v)))

;;; Get a random point in the box from (-n,-n,-n) to (n,n,n).
;;; Note that if n is an integer, points with a 0 coordinate are counted double.
;;; TODO: Use a Guassian distribution (cf. galton box) to generate uniform random direction.
(define (random-point #!optional k n)
  (let ((k (if (default-object? k) 3 k))
        (n (if (default-object? n) 1. n)))
    (make-initialized-list k (lambda (i)
                               (* (if (eq? 0 (random 2)) -1 1)
                                  (random n))))))
