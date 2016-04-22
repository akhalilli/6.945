(declare (usual-integrations))

;;; Support for memoized functions

(define (weak-car-value c default)
  (or (weak-car c)
      (and (not (weak-pair/car? c))
           default)))

;;; The following is conservative...
;;; It should fail if u and v are not the same!
(define unique-frob-1 (list 1))
(define unique-frob-2 (list 2))

(define (weak-list-eqv? u v)
  (cond ((eqv? u v) #t)
        ((and (null? u) (null? v)) #t)
        ((or (null? u (null? v))) #f)
        (else
          (and (eqv? (weak-car-value u unique-frob-1) ;different if GC'd
                     (weak-car-value v unique-frob-2))
               (weak-list-eqv? (weak-cdr u) (weak-cdr v))))))

(define (weak-list-eqv-hash-mod key modulus)
  (int:remainder (weak-list-eqv-hash key) modulus))

(define (weak-list-eqv-hash lst)
  ;; TODO I'm sure I can write a better hash function than this!
  (let loop ((total 0)
             (lst lst))
    (if (null? lst)
      total
      (loop (+ total (eqv-hash (weak-car lst)))
            (weak-cdr lst)))))

(define (weak-list-dead? w)
  (and (not (null? w))
       (or (not (weak-pair/car? w))
           (weak-list-dead? (weak-cdr w)))))

;;; Specialized versions for memoizing binary functions

(define (weak-cadr-value c default)
  ;; Open code of (weak-car-value (weak-cdr c) default)
  (let ((d (weak-cdr c)))
    (or (weak-car d)
        (and (not (weak-pair/car? d))
             default))))

(define (weak-doublet-eqv? u v)
  (or (eqv? u v)
      (and (eqv? (weak-car-value u unique-frob-1) ;different if GC'd
                 (weak-car-value v unique-frob-2))
           (eqv? (weak-cadr-value u unique-frob-1)
                 (weak-cadr-value v unique-frob-2)))))

(define (weak-doublet-eqv-hash-mod key modulus)
  (int:remainder (weak-doublet-eqv-hash key)
                 modulus))

(define (weak-doublet-eqv-hash pair)
  (int:+ (eqv-hash (weak-car pair))
         (eqv-hash (weak-car (weak-cdr pair)))))

;;; TODO This *still* loses, because the value held in the hash-cons
;;; table points back at the keys, and prevents them from being
;;; garbage-collected.  This appears to be a known problem, and the
;;; only known way to solve this problem is for the garbage collector
;;; to implement ephemerons.  See, e.g. "Ephemerons: a new
;;; finalization mechanism", by Barry Hayes, 1997,
;;; http://portal.acm.org/citation.cfm?id=263733&coll=portal&dl=ACM&CFID=26736578&CFTOKEN=29568682

;;; TODO n-dimensional version hashes a weak list of the arguments

(define (binary-memoize-eq f)
  ;; I don't want to cons if unnecessary.
  (let ((the-table
          ((strong-tidy-hash-table/constructor
             weak-doublet-eqv-hash-mod
             weak-doublet-eqv?
             (lambda (key datum)
               (not (weak-list-dead? key)))
             #t)))
        (the-key (weak-cons #f (weak-cons #f '()))))

    (define (memo-f x y)
      (weak-set-car! the-key x)
      (weak-set-car! (weak-cdr the-key) y)
      (let ((canonical-answer
              (hash-table/get the-table the-key #f)))
        (if canonical-answer
          (begin
            (weak-set-car! the-key #f)
            (weak-set-car! (weak-cdr the-key) #f)
            canonical-answer)
          (let ((new (f x y)))
            (hash-table/put! the-table the-key new)
            (set! the-key (weak-cons #f (weak-cons #f '())))
            new))))
    ;; Add clean-table to headed weak alist to be scanned by the GC-DAEMON
    (set-cdr! *memoization-tables*
              (cons (weak-cons memo-f the-table)
                    (cdr *memoization-tables*)))
    memo-f))

(define *memoization-tables* (list '*memoization-tables*))

(define (memoization-tables-gc-daemon)
  (clean-memoization-tables-list *memoization-tables*)
  'done)

(define (clean-memoization-tables-list weak-alist)
  (let clean-head ((this weak-alist))
    (let ((next (cdr this)))
      (if (pair? next)
        (if (weak-car (car next))  ;memo-f not gone
          (begin
            (clean-hash-table (weak-cdr (car next)))
            (clean-head next))
          (begin (set-cdr! this (cdr next))
                 (clean-head this)))
        'done))))

(define (clean-hash-table the-table)
  (hash-table/for-each the-table
                       (lambda (key datum)
                         (if (weak-list-dead? key)
                           (hash-table/remove! the-table key)))))

;;; Should be loaded only once!
;;; TODO Once-only this with lexical environment tricks
(add-gc-daemon! memoization-tables-gc-daemon)

;;; Given two arguments cons-unique returns a pair.  If exactly the
;;; same two arguments were previously combined with cons-unique it
;;; returns the same pair it returned the first time.

(define cons-unique (binary-memoize-eq cons))




;;; Given a list structure, to get a canonical copy equal to the given
;;; list structure.  Must canonicalize and share all substructure.

(define (canonical-copy x)
  (if (pair? x)
    (cons-unique (canonical-copy (car x))
                 (canonical-copy (cdr x)))
    x))

(define (map-unique p lst)
  (if (pair? lst)
    (cons-unique (p (car lst))
                 (map-unique p (cdr lst)))
    lst))
