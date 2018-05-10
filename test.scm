(import (bruijn))

(define pl (plane '(1 1 1 0 0) '(1 0 0 0 0) '(0 1 1.2 3.4 9.112)))
(display (positive-neighbours '(1 2 3 4 5)))
(display (segments 2 pl))
(newline)
