(define char<=?         (lambda (c1 c2) (if (char=? c1 c2) #t (char<? c1 c2))))
(define char>?          (lambda (c1 c2) (char<? c2 c1)))
(define char>=?         (lambda (c1 c2) (if (char=? c1 c2) #t (char<? c2 c1))))
(define newline		(lambda () (print-char #\newline)))
(define list?		(lambda (l) (if (null? l) #t 
					(if (pair? l) (list? (cdr l)) #f))))
(define length-helper	(lambda (l n) (if (null? l) n (length-helper (cdr l) (+ 1 n)))))
(define length		(lambda (l) (length-helper l 0)))
(define map		(lambda (f l) (if (null? l) (quote ()) (cons (f (car l)) (cdr l)))))
(define not		(lambda (x) (if x #f #t)))
(define reverse-helper	(lambda (l acc) (if (null? l) acc  (reverse-helper (cdr l) (cons (car l) acc)))))
(define reverse		(lambda (l) (reverse-helper l nil)))
(define revappend	(lambda (l1 l2)
			  (if (null? l1) l2 (revappend (cdr l1) (cons (car l1) l2)))))
(define append		(lambda (l1 l2)
			  (revappend (reverse l1) l2)))

(define sublist-helper  (lambda (l start end acc)
			  (if (null? l) (reverse acc)
			      (if (= end 0) (reverse acc)
				  (if (= start 0)
				      (sublist-helper (cdr l) 0 (- end 1) (cons (car l) acc))
				      (sublist-helper (cdr l) (- start 1) (- end 1) nil))))))
(define sublist         (lambda (l start end) (sublist-helper l start end nil)))
(define substring       (lambda (s start end) (list->string (sublist (string->list s) start end))))
(define display-list-body (lambda (l spaces)
			    (if (null? l) #t
				(begin
				  (display (car l))
				  (if (null? (cdr l)) #t
				      (if (pair? (cdr l))
					  (begin
					    (if spaces (print-char #\space) #t)
					    (display-list-body (cdr l) spaces))
					  (begin
					    (print-char #\space)
					    (print-char #\.)
					    (print-char #\space)
					    (display (cdr l))
					    )))))))

(define display-list (lambda (l)
		       (print-char #\()
		       (display-list-body l #t)
		       (print-char #\))))
		       
(define display-string 
  (lambda (s) (display-list-body (string->list s) #f)))

(define display (lambda (x)
		  (if (null? x)
		      (begin 
			(print-char #\()
			(print-char #\)))
		      (if (char? x) 
			  (print-char x)
			  (if (pair? x)
			      (display-list x)
			      (if (string? x)
				  (display-string x)
				  (if (number? x)
				      (print-num x)
				      (if (= x #t) 
					  (begin (print-char #\#) 
						 (print-char #\t))
					  (if (= x #f)
					      (begin (print-char #\#)
						     (print-char #\f))
					      #t)))))))))

(define list->string-helper 
  (lambda (s l2 n)
    (if (null? l2) s
	(begin
	  (string-set! s n (car l2))
	  (list->string-helper s (cdr l2) (+ n 1))))))

(define list->string 
  (lambda (l)
      (list->string-helper (make-string (length l)) l 0)))

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

(define digit-to-char 
  (lambda (d)
    (if (= d 0) #\0
	(if (= d 1) #\1
	    (if (= d 2) #\2
		(if (= d 3) #\3 
		    (if (= d 4) #\4
			(if (= d 5) #\5
			    (if (= d 6) #\6
				(if (= d 7) #\7
				    (if (= d 8) #\8
					(if (= d 9) #\9 #f))))))))))))
(define number->string-helper
  (lambda (n rest)
    (if (= n 0)
	(list->string (reverse rest))
	(number->string-helper (/ n 10) (cons (digit-to-char (% n 10)) rest)))))

(define number->string
  (lambda (n) (number->string-helper n)))
    