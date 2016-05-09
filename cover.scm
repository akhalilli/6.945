;;;; Cover time

;;; Cover time is the number of steps a random walk takes to hit every vertex
(define (cover-time vertex)
  (let ((n (count-graph-vertices vertex))
        (done (make-eq-hash-table)))
    (let iter ((vertex vertex)
               (count 0))
      (hash-table/put! done vertex #t)
      (if (eq? n (hash-table/count done))
        count
        (iter (traverse-random vertex) (1+ count))))))

;;; A lollipop is a line segment attached to a complete graph
(define (make-lollipop n)
  (let ((clique (make-complete n))
        (line (make-line n)))
    (add-edge! (make-edge clique line))
    (add-edge! (make-edge line clique))
    clique))

(define (line-plot vertex)
  (define line-length 2)
    `(,(+ 1 (* line-length (/ (vertex-name vertex)
                              (cadr (vertex-graph vertex)))))
       0))
(define (lollipop-plot vertex)
  ((if (equal? 'complete (car (vertex-graph vertex)))
     circle-plot
     line-plot)
   vertex))

#|

;;; One cool theoretical fact is that the cover time of a complete graph is O(n log n),
;;; the cover time of a line segment is O(n^2), but the cover time of a line segment
;;; attached to a complete graph (called a lollipop) is O(n^3).

(cover-time (make-complete 25))
;Value: 65
(cover-time (make-line 25))
;Value: 310
(cover-time (make-lollipop 25))
;Value: 13861

|#
