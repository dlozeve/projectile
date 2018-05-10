(import chicken scheme)
(use sdl2 miscmacros)
(use bruijn)

;; Initialize SDL
(set-main-ready!)
(init! '(video))

;; Call quit! when the program terminates normally
(on-exit quit!)

;; Call quit! when an unhandled exception reaches the top level
(current-exception-handler
 (let ((original-handler (current-exception-handler)))
   (lambda (exception)
     (quit!)
     (original-handler exception))))

;; Define the window and the renderer
(define *window* (create-window! "Hello" 0 0 600 400 '(resizable)))
(define *renderer* (create-renderer! *window* -1 '(software)))
(set! (render-draw-color *renderer*) (make-color 255 255 255 0))

(define (fill-window window)
  (fill-rect! (window-surface window)
	      #f
	      (make-color 0 0 30)))

(define (print-picture renderer)
  (render-draw-line! *renderer* 0 0 200 200))

(define (handle-event ev exit-main-loop!)
  (case (event-type ev)
    ((window)
     (fill-window *window*)
     (print-picture *renderer*))
    ((quit)
     (exit-main-loop! #t))
    ((key-down)
     (case (keyboard-event-sym ev)
       ((escape q)
	(exit-main-loop! #t))))))

(define (main-loop)
  (let/cc exit-main-loop!

	  (fill-window *window*)
	  (print-picture *renderer*)

	  (while #t
	    (pump-events!)
	    (while (has-events?)
	      (handle-event (poll-event!) exit-main-loop!))

	    (render-present! *renderer*)
	    (delay! 20))))

(main-loop)


