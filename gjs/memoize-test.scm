(in-test-group
  memoize

  (define (smoke)
               (let ((a (list 1))  ; Distinct, fresh, uninterned objects
                     (b (list 2)))
                 (check (eq? (cons-unique a b)
                             (cons-unique a b)))))

  (define (cons-leaks-not)
               (define (one-fresh-cons)
                 (let ((new-frob1 (list 'frob))
                       (new-frob2 (list 'frob)))
                   (cons new-frob1 new-frob2)))
               (check (< (memory-loss-from
                           (repeated 1000 one-fresh-cons))
                         50)))

  (define (weak-eq-hashing)
               (define frotz (make-eq-hash-table))
               (define (one-fresh-object)
                 (hash-table/put! frotz (list 'fresh) (list 'fresh-value)))
               ;; Sadly, this doesn't work, because weak hash tables do not, in fact,
               ;; appear to clean themselves directly on gc.
               #;
               (check (< (memory-loss-from (repeated 10000 one-fresh-object))
                         100))
               ;; This version works, though, because, being address hashed, eq
               ;; tables clean themselves on access after gc.
               (define (prod-the-table count)
                 (lambda ()
                   (repeat count one-fresh-object)
                   ;; Force a gc and a post-gc rehash, which also cleans
                   ;; out the table
                   (gc-flip)
                   (hash-table/get frotz #f #f)))
               (check (< (memory-loss-from (prod-the-table 2500))
                         25))
               (check (= 0 (hash-table/count frotz))))

  (define (weak-cleaning)
    (define frotz (make-eq-hash-table))
    (hash-table/put! frotz #f 4)
    (produces 4 (hash-table/get frotz #f #f))
    (hash-table/clean! frotz)
    ;; Oops!
    (produces #f (hash-table/get frotz #f #f)))

  (define (weak-self-reference)
               (define frotz (make-eq-hash-table))
               (define (one-fresh-object)
                 (let ((fresh-object (list 'fresh)))
                   ;; Same value as key; no other references
                   (hash-table/put! frotz fresh-object fresh-object)))
               (define (prod-the-table count)
                 (lambda ()
                   (repeat count one-fresh-object)
                   ;; Force a gc and a post-gc rehash, which also cleans
                   ;; out the table
                   (gc-flip)
                   (hash-table/get frotz #f #f)))
               ;; Weak hash tables still hold on to circular associations :(
               (check (> (memory-loss-from (prod-the-table 1000))
                         6000)))

  (define (strong-tidy-tables)
               (define (ok? key value)
                 (eq? value 'ok))
               (define frotz (make-strong-eq-hash-table))
               (define (fresh-ok-object)
                 (hash-table/put! frotz (list 'fresh) 'ok))
               (define (fresh-not-ok-object)
                 (hash-table/put! frotz (list 'fresh) 'not-ok))
               (define (stuff-table count object-maker)
                 (lambda ()
                   (repeat count object-maker)
                   ;; Force a gc and a post-gc rehash, just to make sure
                   (gc-flip)
                   (hash-table/get frotz #f #f)))

               (check (< (memory-loss-from (stuff-table 1000 fresh-not-ok-object))
                         10))
               (check (= 0 (hash-table/count frotz)))

               (check (> (memory-loss-from (stuff-table 1000 fresh-ok-object))
                         5000))
               (check (= 1000 (hash-table/count frotz)))
               )

  (define (cons-unique-leaks-not)
               (define (one-fresh-unique-cons)
                 (let ((new-frob1 (list 'frob))
                       (new-frob2 (list 'frob)))
                   (cons-unique new-frob1 new-frob2)))
               (define (prod-the-table count)
                 (lambda ()
                   (repeat count one-fresh-unique-cons)
                   ;; Force a gc and a post-gc rehash, which also cleans
                   ;; out the table
                   (gc-flip)
                   (one-fresh-unique-cons)))
               ;; TODO Sadly, cons-unique does leak :(
               (check (> (memory-loss-from (prod-the-table 1000))
                         15000)))

  (define (lambda-leaks-not)
               (define (one-fresh-procedure)
                 (let ((fresh (list 'fresh)))
                   (lambda (x y) fresh)))
               (check (< (memory-loss-from (repeated 1000 one-fresh-procedure))
                         10)))

  (define
    (check (< (memory-loss-from (repeated 1000 make-eq-hash-table))
              10)))

  (define (binary-memoize-eq-leaks-not)
               (define (one-fresh-memoizer)
                 (binary-memoize-eq
                   (let ((fresh (list 'fresh)))
                     (lambda (x y) fresh))))
               (check (< (memory-loss-from (repeated 1000 one-fresh-memoizer))
                         10)))
  )
