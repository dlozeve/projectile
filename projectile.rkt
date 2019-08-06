#lang racket


(define (dot x y)
  (for/sum ([i x]
	    [j y])
    (* i j)))

(define (norm x)
  (sqrt (for/sum ([i x]) (* i i))))

(define (normalize x)
  (let ([norm-x (norm x)])
    (for/vector ([i x]) (/ i norm-x))))

(struct plane
  (origin u v)
  #:transparent)

(define (make-plane origin u v)
  ;; TODO pad with zeros if dimension mismatch
  (plane origin (normalize u) (normalize v)))

;; Position of a point relative to a plane origin point
(define (affine-transform plane point)
  (for/vector ([i point]
	       [j (plane-origin plane)])
    (- i j)))

(define (project plane point)
  (let* ([new-point (affine-transform plane point)]
	 [point-u (dot new-point (plane-u plane))]
	 [point-v (dot new-point (plane-v plane))])
    #(point-u point-v)))

(define (distance plane point)
  (let* ([new-point (affine-transform plane point)]
	 [point-u (dot new-point (plane-u plane))]
	 [point-v (dot new-point (plane-v plane))])
    (norm (for/vector ([p-i new-point]
		       [u-i (plane-u plane)]
		       [v-i (plane-v plane)])
	    (- p-i (* point-u u-i) (* point-v v-i))))))

;; Width of a grid-aligned hypercube sliding against the plane
(define (hypercube-width plane)
  (let* ([n (vector-length (plane-origin plane))]
	 [shifts (apply cartesian-product (make-list n '(-1 0 1)))]
	 [corners (for/list ([shift shifts])
		    (for/vector ([i (plane-origin plane)]
				 [j shift])
		      (+ i j)))])
    (apply max (for/list ([c corners]) (distance plane c)))))

(define (neighbours grid-point)
  (let ([n (vector-length grid-point)])
    (for*/list ([k '(-1 1)]
		[i (range n)])
      (let ([new-grid-point (vector-copy grid-point)])
	(vector-set! new-grid-point i (+ k (vector-ref new-grid-point i)))
	new-grid-point))))

