(define factorial (lambda (x) (if (= x 0) 1 
				  (* x (factorial (- x 1))))))
(define foo (lambda (f x n) 
	      (if (= n 1) (f x)
		  (begin
		    (f x)
		    (foo f x (- n 1))))))
(foo (lambda (x) 
       (print-num (factorial x))
       (print-char #\newline)) 10 10000)
