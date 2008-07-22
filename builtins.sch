(define newline (lambda () (print-char #\newline)))
(define list?   (lambda (l) (or (null? l) (and (pair? l) (list? (cdr l))))))
(define length  (lambda (l) (if (null? l) 0 (+ 1 (length (cdr l))))))
(define map     (lambda (f l) (if (null? l) '() (cons (f (car l)) (cdr l)))))
(define not     (lambda (x) (if x #f #t)))

(define display-list-body (lambda (l)
			  (if (null? l) #t
			      (begin
				(display (car l))
				(if (null? (cdr l)) #t
				    (begin
				      (display #\space)
				      (if (pair? cdr l) #t (display ". "))
				      (display (cdr l))))))))

(define display-list (lambda (l)
		       (display #\()
		       (display-list-body l)
		       (display #\))))
		       
(define display-string 
  (lambda (s) (display-list-body (string->list s))))

(define display (lambda (x)
		  (if (char? x) 
		      (print-char x)
		      (if (pair? x)
			  (display-list x)
			  (if (string? x)
			      (display-string x)
			      (if (number? x)
				  (print-num x)
				  #t))))))
		      