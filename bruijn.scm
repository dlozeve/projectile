(define-library (bruijn)
  (export plane positive-neighbours segments)

  ;;(import scheme r7rs)
  (import (scheme base)
	  (scheme inexact)
	  (srfi 1)
	  (srfi 42))

  (begin
    (define (dot u v)
      (apply + (map * u v)))

    (define (norm u)
      (sqrt (apply + (map square u))))

    (define-record-type plane
      (plane o u v)
      plane?
      (o plane-origin set-plane-origin!)
      (u plane-u set-plane-u!)
      (v plane-v set-plane-v!))

    (define (project-point p pl)
      (let ((o (plane-origin pl))
	    (u (plane-u pl))
	    (v (plane-v pl)))
	(list (dot u (map - p o))
	      (dot v (map - p o)))))

    (define (project-segment s pl)
      (map (lambda (p) (project-point p pl)) s))

    (define (grid-diagonal n)
      (list-ec (:range a (+ n 1))
    	       (:range b (+ n 1))
    	       (:range c (+ n 1))
    	       (:range d (+ n 1))
    	       (:range e (+ n 1))
    	       (if (= n (+ a b c d e)))
    	       (list a b c d e)))

    (define (positive-neighbours p)
      (let ((neighb (grid-diagonal 1)))
	(map (lambda (x) (map + x p)) neighb)))

    (define (segments-neighbours p plane)
      (let* ((p-proj (project-point p plane))
	     (neighb (positive-neighbours p))
	     (proj (lambda (x) (project-point x plane)))
	     (neighb-proj (map proj neighb)))
	(map (lambda (x) (cons p-proj (list x))) neighb-proj)))

    (define (segments n plane)
      (let* ((diag (grid-diagonal n))
	     (new-segments (concatenate (map (lambda (x) (segments-neighbours x plane)) diag))))
	(if (= n 0)
	    new-segments
	    (append (segments (- n 1) plane) new-segments))))))

