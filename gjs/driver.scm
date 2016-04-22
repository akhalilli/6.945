(declare (usual-integrations))

(define *drive-wallp* #f)

(define (drive accumulators drivees)
  (pp 'driver-loop)
  (for-each (lambda (acc)
              (set-accumulator-changed?! acc #f))
            accumulators)
  (for-each (lambda (drivee)
              (and *drive-wallp*
                   (pp (map accumulator-content accumulators)))
              (drivee))
            drivees)
  (if (any accumulator-changed? accumulators)
    (drive accumulators drivees)))

(define (fringe tree)
  (let walk ((tree tree)
             (answer '()))
    (cond ((null? tree)
           answer)
          ((pair? tree)
           (walk (car tree)
                 (walk (cdr tree)
                       answer)))
          (else
            (cons tree answer)))))

(define (copy-accumulator accumulator)
  (make-accumulator
    (accumulator-content accumulator)
    #t
    (accumulator-merge accumulator)
    (accumulator-different? accumulator)))

(define *guesser-wallp* #f)

(define (one-guess-drive accumulators drivee-constructor)
  (define (accumulator-solved? accumulator)
    (= 1 (length (accumulator-content accumulator))))
  (define (solved? accumulators)
    (every accumulator-solved? (fringe accumulators)))
  (define (contradiction? accumulators)
    (any null? (map accumulator-content (fringe accumulators))))
  (define (crunch! accumulators)
    (drive (fringe accumulators)
           (drivee-constructor accumulators))
    accumulators)
  (and *guesser-wallp*
       (pp accumulators))
  (let ((backbone (tree-copy accumulators)))
    (define (copy-into-backbone! originals target)
      (cond ((null? originals)
             'ok)
            ((pair? (car originals))
             (copy-into-backbone! (car originals) (car target))
             (copy-into-backbone! (cdr originals) (cdr target)))
            (else
              (set-car! target (copy-accumulator (car originals)))
              (copy-into-backbone! (cdr originals) (cdr target)))))
    (let loop ()
      (and *guesser-wallp*
           (pp 'one-guess-drive-looping))
      (crunch! accumulators)
      (let try-an-accumulator
        ((remaining-originals accumulators)
         (remaining-backbone backbone))
        (cond ((null? remaining-originals)
               accumulators)
              ((pair? (car remaining-originals))
               (try-an-accumulator (car remaining-originals)
                                   (car remaining-backbone))
               (try-an-accumulator (cdr remaining-originals)
                                   (cdr remaining-backbone)))
              ((accumulator-solved? (car remaining-originals))
               (try-an-accumulator (cdr remaining-originals)
                                   (cdr remaining-backbone)))
              (else
                (let try-a-guess
                  ((remaining-guesses
                     (accumulator-content (car remaining-originals))))
                  (define (guess-item!)
                    (and *guesser-wallp*
                         (pp `(trying ,(car remaining-guesses)
                                      for ,(car remaining-originals))))
                    (copy-into-backbone! accumulators backbone)
                    (add-content (car remaining-backbone)
                                 (list (car remaining-guesses)))
                    (crunch! backbone))
                  (cond ((null? remaining-guesses)
                         (try-an-accumulator (cdr remaining-originals)
                                             (cdr remaining-backbone)))
                        (else
                          (let ((result (guess-item!)))
                            (cond ((solved? result)
                                   ;; TODO What's really the right thing
                                   ;; to do here?
                                   (and *guesser-wallp*
                                        (pp '(guess solves))
                                        (pp (map accumulator-content (fringe result))))
                                   result)
                                  ((contradiction? result)
                                   (and *guesser-wallp*
                                        (pp '(guess contradictory)))
                                   (add-content
                                     (car remaining-originals)
                                     (delq (car remaining-guesses)
                                           (accumulator-content
                                             (car remaining-originals))))
                                   (loop))
                                  (else
                                    (and *guesser-wallp*
                                         (pp '(guess inconclusive)))
                                    (try-a-guess
                                      (cdr remaining-guesses)))))))))))))
  (crunch! accumulators))
