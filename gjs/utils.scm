(declare (usual-integrations))

(define (sort-by object property)
  ;; TODO memoize property?
  (sort object (lambda (a b) (< (property a) (property b)))))

(define (assert p #!optional error-comment irritant)
  (if (not p)
    (begin
      (if (not (default-object? irritant))
        (pp irritant))
      (error (if (default-object? error-comment)
               "Failed assertion"
               error-comment)))))

(define (lset-xor = . lists)
  (reduce (lambda (b a)      ; Compute A xor B:
            ;; Note that this code relies on the constant-time
            ;; short-cuts provided by LSET-DIFF+INTERSECTION,
            ;; LSET-DIFFERENCE & APPEND to provide constant-time short
            ;; cuts for the cases A = (), B = (), and A eq? B. It takes
            ;; a careful case analysis to see it, but it's carefully
            ;; built in.

            ;; Compute a-b and a^b, then compute b-(a^b) and
            ;; cons it onto the front of a-b.
            (receive (a-b a-int-b)   (lset-diff+intersection = a b)
                     (cond ((null? a-b)     (lset-difference = b a))
                           ((null? a-int-b) (append b a))
                           (else (fold (lambda (xb ans)
                                         (if (member xb a-int-b =) ans (cons xb ans)))
                                       a-b
                                       b)))))
          '() lists))

(define (lexicgraphic-order <)
  (lambda (lst1 lst2)
    (cond ((null? lst2) #f)
          ((null? lst1) #t)
          ((< (car lst1) (car lst2)) #t)
          ((< (car lst2) (car lst1)) #f)
          (else ((lexicgraphic-order <) (cdr lst1) (cdr lst2))))))

(define (group-by lst property)
  (let ((answers (make-eq-hash-table)))
    (let loop ((lst lst))
      (cond ((null? lst)
             (map cdr (hash-table->alist answers)))
            (else
              (let ((key (property (car lst))))
                (hash-table/modify!
                  answers key '() (lambda (datum) (cons (car lst) datum)))
                (loop (cdr lst))))))))
