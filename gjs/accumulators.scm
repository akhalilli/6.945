(declare (usual-integrations))

(define-structure
  accumulator
  content
  changed?
  merge
  different?)

(define (add-content accumulator increment)
  (let ((answer ((accumulator-merge accumulator)
                 (accumulator-content accumulator) increment)))
    (if ((accumulator-different? accumulator)
         answer (accumulator-content accumulator))
      (begin (set-accumulator-changed?! accumulator #t)
             (set-accumulator-content! accumulator answer)))))
