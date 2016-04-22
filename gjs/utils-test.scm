(in-test-group
  utils

  (define-each-check
    (equal? '((6 4 2) (5 3 1)) (group-by '(1 2 3 4 5 6) even?))))
