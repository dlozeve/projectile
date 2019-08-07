#lang racket
(require racket/draw)

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

;; Position of a point relative to a plane origin point
(define (affine-transform plane point)
  (for/vector ([i point]
	       [j (plane-origin plane)])
    (- i j)))

(define (project plane point)
  (let* ([new-point (affine-transform plane point)]
	 [point-u (dot new-point (plane-u plane))]
	 [point-v (dot new-point (plane-v plane))])
    (vector point-u point-v)))

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

(define (make-grid dimension size)
  (apply append
	 (for/list ([u (apply cartesian-product (build-list dimension (lambda _ (range size))))])
	   (for/list ([k (in-range dimension)])
	     (let ([v (apply vector u)])
	       (vector-set! v k (add1 (vector-ref v k)))
	       (list (apply vector u) v))))))

(define (project-grid-on-plane plane grid)
  (let ([d (hypercube-width plane)])
    (for/list ([u grid]
	       #:when (and (> d (distance plane (car u)))
			   (> d (distance plane (cadr u)))))
      (list (project plane (car u)) (project plane (cadr u))))))

(define (draw-tiling tiling size)
  (define target (make-bitmap size size))
  (define dc (new bitmap-dc% [bitmap target]))
  (send dc set-pen "white" 1 'solid)

  (define max-pos
    (apply max (apply append (for/list ([s tiling])
			       (append (vector->list (car s))
				       (vector->list (cadr s)))))))

  (define factor (/ size max-pos))

  (for ([segment tiling])
    (match segment
      [(list (vector a b) (vector c d))
       (send dc draw-line (* factor a) (* factor b) (* factor c) (* factor d))]))

  target)

(define phi (/ (+ 1. (sqrt 5)) 2))

(define p (plane (vector 0. 0. 0 1. 0.)
		 (vector phi 0 (- phi) -1 1)
		 (vector -1 1 phi 0 (- phi))))

;; (define p (plane (make-vector 3 0.) (vector 0. 0. 1.) (vector 0. 1. 0.)))
(define tiling (project-grid-on-plane p (make-grid 3 5)))
(draw-tiling tiling 500)
