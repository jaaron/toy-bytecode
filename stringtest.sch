(define print-string-list 
  (lambda (x)
    (if (null? x) nil
	(begin
	  (print-char (car x))
	  (print-string-list (cdr x))))))

(define print-string
  (lambda (s) (print-string-list (string->list s))))

(define length (lambda (s) (if (null? s) 0 (+ 1 (length (cdr s))))))

(print-num (string-length "Hello world")))