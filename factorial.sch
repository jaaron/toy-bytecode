(define factorial (lambda (x) (if (= x 0) 1 
				  (* x (factorial (- x 1))))))
(define repeat (lambda (f n) 
	      (if (= n 1) (f)
		  (begin
		    (f)
		    (repeat f (- n 1))))))

(repeat (lambda ()
	  (begin
	    (print-num (factorial 10))
	    (print-char #\newline)))
	  100)
