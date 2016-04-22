(define (memory-loss-from thunk)
  ;; Two GCs make sure to execute gc-daemons and also clean up
  ;; everything the daemons discard.
  (gc-flip)
  (let ((initial-memory (gc-flip)))
    (thunk)
    ;; I need three here.  Otherwise, the binary-memoize-eq-leaks-not
    ;; test reports a spurious leak of 2 words per memoizer.  The
    ;; reason is that the first gc collects the memoizers but not
    ;; their memoization tables; and triggers the
    ;; memoization-tables-gc-daemon, which breaks references to those
    ;; tables.  The second gc reclaims the tables, but not their cells
    ;; in the address-hash-tables weak list in hashtb.scm; and
    ;; triggers the mark-address-hash-tables! loop that breaks
    ;; references to those cells.  Finally, the third gc reclaims the
    ;; cells themselves, producing a clean reading.
    (gc-flip)
    (gc-flip)
    (- initial-memory (gc-flip))))

(define (repeat count thunk)
  (let loop ((count count))
    (if (<= count 0)
      'ok
      (begin
        (thunk)
        (loop (- count 1))))))

;; This version is a thunk combinator!
(define ((repeated count thunk))
  (repeat count thunk))

;; To make sure the memory for the primes that hash tables use gets
;; allocated now, before I start poking said hash tables.
(let ((upto 150000))
  (let force-prime-numbers ((primes prime-numbers-stream))
    (if (< upto (car primes))
      (car primes)
      (force-prime-numbers (force (cdr primes))))))
