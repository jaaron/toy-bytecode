(define >=              (lambda (n1 n2) (if (= n2 n1) #t (< n2 n1))))
(define char<=?         (lambda (c1 c2) (if (char=? c1 c2) #t (char<? c1 c2))))
(define char>?          (lambda (c1 c2) (char<? c2 c1)))
(define char>=?         (lambda (c1 c2) (if (char=? c1 c2) #t (char<? c2 c1))))
(define newline		(lambda () (print-char #\newline)))
(define list?		(lambda (l) (if (null? l) #t 
					(if (pair? l) (list? (cdr l)) #f))))
(define length-helper	(lambda (l n) (if (null? l) n (length-helper (cdr l) (+ 1 n)))))
(define length		(lambda (l) (length-helper l 0)))
(define not		(lambda (x) (if x #f #t)))
(define reverse-helper	(lambda (l acc) (if (null? l) acc  (reverse-helper (cdr l) (cons (car l) acc)))))
(define reverse		(lambda (l) (reverse-helper l nil)))
(define revappend	(lambda (l1 l2)
			  (if (null? l1) l2 (revappend (cdr l1) (cons (car l1) l2)))))
(define append		(lambda (l1 l2)
			  (revappend (reverse l1) l2)))
(define map-helper      (lambda (f l ll) (if (null? l) (reverse ll) (map-helper f (cdr l) (cons (f (car l)) ll)))))
(define map		(lambda (f l) (map-helper f l (quote ()))))

(define foldl-string    (lambda (f seed s start end)
			  (if (>= start end) seed
			      (foldl-string f (f seed (string-ref s start)) 
					    s (+ start 1) end))))

(define sublist-helper  (lambda (l start end acc)
			  (if (null? l) (reverse acc)
			      (if (= end 0) (reverse acc)
				  (if (= start 0)
				      (sublist-helper (cdr l) 0 (- end 1) (cons (car l) acc))
				      (sublist-helper (cdr l) (- start 1) (- end 1) nil))))))
(define sublist         (lambda (l start end) (sublist-helper l start end nil)))

(define copy-into-string (lambda (dest dest-offset src src-offset src-end)
			   (foldl-string (lambda (loc c)
					   (string-set! dest loc c)
					   (+ loc 1))
					 dest-offset src src-offset src-end)
			   dest))


(define substring       (lambda (s start end) 
			  (copy-into-string (make-string (- end start)) 0 s start end)))

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
  (lambda (s) 
    (foldl-string 
     (lambda (x c) (display c))
     nil s 0 (string-length s))))

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

;; (define string-append
;;   (lambda (s1 s2) 
;;     (list->string (append (string->list s1) (string->list s2)))))

(define string-append
  (lambda (s1 s2)
    (let ((s1-length (string-length s1))
	  (s2-length (string-length s2)))
    (copy-into-string (copy-into-string (make-string (+ s1-length s2-length)) 0 s1 0 s1-length)
		      s1-length s2 0 s2-length))))

(define substring=?
  (lambda (s1 s2 start end)
    (if (>= start end) #t
	(if (char=? (string-ref s1 start) (string-ref s2 start))
	    (substring=? s1 s2 (+ start 1) end)
	    #f))))

(define string=? (lambda (s1 s2) (let ((s1-length (string-length s1))
				       (s2-length (string-length s2)))
				   (if (= s1-length s2-length)
				       (substring=? s1 s2 0 s1-length) #f))))

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
					(if (= d 9) #\9 
					    (begin 
					      (display "Error: '")
					      (display d)
					      (display "' is not a digit!\n")
					      (quit))))))))))))))
(define number->string-helper
  (lambda (n rest)
    (if (= n 0)
	(list->string rest)
	(number->string-helper (/ n 10) (cons (digit-to-char (% n 10)) rest)))))

(define number->string
  (lambda (n) 
	  (if (= n 0) "0"
	      (if (< 0 n) 
		  (number->string-helper n nil)
		  (string-append "-" (number->string-helper (* n -1) nil))))))
    