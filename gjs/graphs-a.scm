(declare (usual-integrations))

;;; Compiling doesn't work with this in the same file
;;; let expression scoping?

(define (conventional-edge-variable-name vertex-list)
  (string->symbol
    (apply string-append
           (let loop ((vertex-list (map symbol->string vertex-list)))
             (cond ((null? vertex-list)
                    '())
                   ((null? (cdr vertex-list))
                    vertex-list)
                   (else
                     (cons (car vertex-list)
                           (cons "-" (loop (cdr vertex-list))))))))))
