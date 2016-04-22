(define (compiled-code-type)
  ;; Trying to support the C backend
  (if (lexical-unbound?
        (nearest-repl/environment)
        'compiler:compiled-code-pathname-type)
    "com"
    (compiler:compiled-code-pathname-type)))

(define (cf-conditionally filename)
  (fluid-let ((sf/default-syntax-table (nearest-repl/environment)))
             (sf-conditionally filename))
  (if (not (file-processed? filename "bin" (compiled-code-type)))
    (compile-bin-file filename)))

(define (load-compiled filename)
  (cf-conditionally filename)
  (load filename))

(define (self-relatively thunk)
  (if (current-eval-unit #f)
    (with-working-directory-pathname
      (directory-namestring (->truename (current-load-pathname)))
      thunk)
    (thunk)))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(define (load-relative-compiled filename)
  (self-relatively (lambda () (load-compiled filename))))

(load-relative "../../../testing/load")

(self-relatively
  (lambda ()
    (load "hash-table-patch" (->environment '(runtime hash-table)))))

(define strong-tidy-hash-table/constructor
  (access strong-tidy-hash-table/constructor (->environment '(runtime hash-table))))

(for-each load-relative-compiled
          '("eq-properties"
            "memoize"
            "utils"
            "graphs"
            "accumulators"
            "driver"
            "sudoku"))

