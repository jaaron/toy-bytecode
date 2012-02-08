(define char<=?         (lambda (c1 c2) (if (char=? c1 c2) #t (char<? c1 c2))))
(define char>?          (lambda (c1 c2) (char<? c2 c1)))
(define char>=?         (lambda (c1 c2) (if (char=? c1 c2) #t (char<? c2 c1))))
(define newline		(lambda () (print-char #\newline)))
(define list?		(lambda (l) (if (null? l) #t 
					(if (pair? l) (list? (cdr l)) #f))))
(define length-helper	(lambda (l n) (if (null? l) n (length-helper (cdr l) (+ 1 n)))))
(define length		(lambda (l) (length-helper l 0)))
(define map		(lambda (f l) (if (null? l) '() (cons (f (car l)) (cdr l)))))
(define not		(lambda (x) (if x #f #t)))
(define reverse-helper	(lambda (l acc) (if (null? l) acc  (reverse-helper (cdr l) (cons (car l) acc)))))
(define reverse		(lambda (l) (reverse-helper l nil)))
(define revappend	(lambda (l1 l2)
			  (if (null? l1) l2 (revappend (cdr l1) (cons (car l1 l2))))))
(define append		(lambda (l1 l2) (revappend (reverse l1) l2)))

(define sublist-helper  (lambda (l start end acc)
			  (if (null? l) (reverse acc)
			      (if (= end 0) (reverse acc)
				  (if (= start 0)
				      (sublist-helper (cdr l) 0 (- end 1) (cons (car l) acc))
				      (sublist-helper (cdr l) (- start 1) (- end 1) nil))))))
(define sublist         (lambda (l start end) (sublist-helper l start end nil)))
(define substring       (lambda (s start end) (list->string (sublist (string->list s) start end))))
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

(define string-append
  (lambda (s1 s2) 
    (list->string (append (string->list s1) (string->list s2)))))

(define string=?-helper
  (lambda (sl1 sl2)
    (if (null? sl1)
	(if (null? sl2) #t
	    (if (not (char=? (car sl1) (car sl2))) #f
		(string=?-helper (cdr sl1) (cdr sl2))))
	(if (null? sl2) #f
	    (string=?-helper (cdr sl1) (cdr sl2))))))


(define string=? (lambda (s1 s2) (string=? (string->list s1) (string->list s2))))

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
    