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

(define list->string-helper 
  (lambda (s l2 n)
    (if (null? l2) s
	(begin
	  (string-set! s n (car l2))
	  (list->string-helper s (+ n 1) (cdr l2))))))

(define list->string 
  (lambda (l)
      (list->string-helper (make-string (length l)) (length l) l)))

(define reverse
  (lambda (l acc)
    (if (null? l) acc  (reverse (cdr l) (cons (car l) acc)))))

(define number->string-helper
  (lambda (n rest)
    (let ((digit-to-char (lambda (d)
			   (if (= n 0) #\0
			       (if (= n 1) #\1
				   (if (= n 2) #\2
				       (if (= n 3) #\3 
					   (if (= n 4) #\4
					       (if (= n 5) #\5
						   (if (= n 6) #\6
						       (if (= n 7) #\7
							   (if (= n 8) #\8
							       (if (= n 9) #\9 nil)))))))))))))
      (if (= n 0)
	  (list->string (reverse rest))
	  (number->string-helper (/ n 10) (cons (digit-to-char (% n 10)) rest))))))

(define number->string
  (lambda (n) (number->string-helper n)))
    