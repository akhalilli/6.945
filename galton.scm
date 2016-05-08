;;;; A demonstration of a Galton box using the graph library

;;; Galton box graph of depth n
(define-memoized (galton x y)
                 (make-vertex
                   (edges (galton x y)
                          ((galton (-1+ x) (1+ y)))
                          ((galton (1+ x) (1+ y))))
                   (list x y)))

;;; Random walk on a graph (unweighted)
(define (random-walk start n)
  (let step ((n n)
             (pos start))
    (if (= n 0)
        pos
        (let* ((edges (vertex-edges-list pos))
               (random-index (random (length edges)))
               (random-edge (list-ref edges random-index))
               (next-vertex (edge-head random-edge)))
          (step (- n 1) next-vertex)))))

;;; Perform random walk on Galton box

(define (galton-box) (galton 0 0))

(random-walk (galton-box) 10)
;Value 26: #[vertex 26 (2 10)]

;;; Draw an integer between -n and n from an approximate
;;; gaussian distribution using the Galton box, for large
;;; n.
(define (Gaussian-distribution n)
  (/ (car (vertex-name (random-walk (galton-box) (* 2 n)))) 2))

;;; Perform many runs of the Galton box
(define (run-galton-box runs n)
  (let loop ((results (make-eq-hash-table))
	     (runs-left runs))
    (if (= runs-left 0)
	results
        (let ((result (Gaussian-distribution n)))
          (hash-table/put! results
			   result
			   (+ 1 (hash-table/get results result 0)))
	  (loop results (- runs-left 1))))))

(let ((results (run-galton-box 10000 100)))
  (let loop ((i -25))
    (if (<= i 25)
	(begin
  	  (display i) (display ": ") (display (hash-table/get results i 0))
	  (newline)
	  (loop (+ i 1))))))

#|

-25: 0
-24: 0
-23: 5
-22: 1
-21: 12
-20: 8
-19: 7
-18: 20
-17: 30
-16: 41
-15: 47
-14: 72
-13: 97
-12: 129
-11: 167
-10: 207
-9: 234
-8: 326
-7: 372
-6: 394
-5: 448
-4: 483
-3: 510
-2: 529
-1: 513
0: 526
1: 552
2: 560
3: 524
4: 519
5: 416
6: 441
7: 367
8: 297
9: 235
10: 212
11: 165
12: 162
13: 107
14: 65
15: 61
16: 42
17: 38
18: 18
19: 15
20: 12
21: 10
22: 3
23: 0
24: 1
25: 0

|#