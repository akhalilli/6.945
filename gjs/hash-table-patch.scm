(define (strong-tidy-hash-table/constructor
          key-hash key=? entry-valid? #!optional rehash-after-gc?)
  (hash-table-constructor
    (make-strong-tidy-hash-table-type
      key-hash key=? entry-valid? (if (default-object? rehash-after-gc?)
                                    #t
                                    rehash-after-gc?))))

(define (make-strong-tidy-hash-table-type key-hash key=? entry-valid? rehash-after-gc?)
  (if rehash-after-gc?
    (make-strong-tidy-rehash-type key-hash key=? entry-valid?)
    (make-strong-tidy-no-rehash-type key-hash key=? entry-valid?)))

(define-integrable (make-strong-tidy-rehash-type key-hash key=? entry-valid?)
                   (make-strong-tidy-type key-hash key=? entry-valid? #t (compute-address-hash key-hash)))

(define-integrable (make-strong-tidy-no-rehash-type key-hash key=? entry-valid?)
                   (make-strong-tidy-type key-hash key=? entry-valid? #f (compute-non-address-hash key-hash)))


(define-integrable (make-strong-tidy-type key-hash key=? entry-valid?
                                          rehash-after-gc? compute-hash!)
                   (define (%strong-tidy-entry-valid? entry)
                     (and (%strong-entry-valid? entry)
                          (entry-valid? (%strong-entry-key entry)
                                        (%strong-entry-datum entry))))
                   (make-table-type key-hash key=? rehash-after-gc?
                                    (make-method:get compute-hash! key=? %strong-entry-key
                                                     %strong-entry-datum)
                                    (make-method:put! compute-hash! key=? %strong-make-entry
                                                      %strong-entry-key %strong-set-entry-datum!)
                                    (make-method:modify! compute-hash! key=? %strong-make-entry
                                                         %strong-entry-key %strong-entry-datum
                                                         %strong-set-entry-datum!)
                                    (make-method:remove! compute-hash! key=? %strong-entry-key)
                                    (make-method:clean! %strong-tidy-entry-valid?)
                                    (make-method:rehash! key-hash %strong-tidy-entry-valid?
                                                         %strong-entry-key)
                                    (make-method:fold %strong-tidy-entry-valid? %strong-entry-key
                                                      %strong-entry-datum)
                                    (make-method:copy-bucket %strong-tidy-entry-valid?
                                                             %strong-make-entry
                                                             %strong-entry-key
                                                             %strong-entry-datum)))

(define (make-method:clean! entry-valid?)
  (lambda (table)
    (let ((buckets (table-buckets table)))
      (let ((n-buckets (vector-length buckets)))
        (do ((i 0 (fix:+ i 1)))
          ((not (fix:< i n-buckets)))
          (letrec
            ((scan-head
               (lambda (p)
                 (if (pair? p)
                   (if (entry-valid? (car p))
                     (begin
                       (vector-set! buckets i p)
                       (scan-tail (cdr p) p))
                     (begin
                       (decrement-table-count! table)
                       (scan-head (cdr p))))
                   (vector-set! buckets i p))))
             (scan-tail
               (lambda (p q)
                 (if (pair? p)
                   (if (entry-valid? (car p))
                     (scan-tail (cdr p) p)
                     (begin
                       (decrement-table-count! table)
                       (let loop ((p (cdr p)))
                         (if (pair? p)
                           (if (entry-valid? (car p))
                             (begin
                               (set-cdr! q p)
                               (scan-tail (cdr p) p))
                             (begin
                               (decrement-table-count! table)
                               (loop (cdr p))))
                           (set-cdr! q p)))))))))
            (scan-head (vector-ref buckets i))))))))
