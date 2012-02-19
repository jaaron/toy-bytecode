;; Copyright 2012 J. Aaron Pendergrass

;; This file is part of toy-bytecode.

;; toy-bytecode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; toy-bytecode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with toy-bytecode.  If not, see <http://www.gnu.org/licenses/>.

(define fold            (lambda (f s l) (if (null? l) s (fold f (f s (car l)) (cdr l)))))
(define vector-fold     (lambda (f seed s start end)
			  (if (>= start end) seed
			      (vector-fold f (f (vector-ref s start) seed) 
					   s (+ start 1) end))))
(define string-fold     vector-fold)

(define cadr            (lambda (l) (car (cdr l))))
(define caar            (lambda (l) (car (car l))))
(define cdar            (lambda (l) (cdr (car l))))
(define cddr            (lambda (l) (cdr (cdr l))))
(define caddr           (lambda (l) (car (cddr l))))
(define >=              (lambda (n1 n2) (if (= n2 n1) #t (< n2 n1))))
(define char<=?         (lambda (c1 c2) (if (char=? c1 c2) #t (char<? c1 c2))))
(define char>?          (lambda (c1 c2) (char<? c2 c1)))
(define char>=?         (lambda (c1 c2) (if (char=? c1 c2) #t (char<? c2 c1))))
(define newline		(lambda () (print-char #\newline)))
(define list?		(lambda (l) (if (null? l) #t 
					(if (pair? l) (list? (cdr l)) #f))))
(define length		(lambda (l) (fold (lambda (n x) (+ n 1)) 0 l)))
(define not		(lambda (x) (if x #f #t)))
(define revappend	(lambda (l1 l2) (fold (lambda (acc e) (cons e acc)) l2 l1)))
(define reverse		(lambda (l) (revappend l '())))
(define append		(lambda (l1 l2) (revappend (reverse l1) l2)))
(define map		(lambda (f l)
			  (reverse (fold (lambda (acc e) (cons (f e) acc)) '() l))))


(define copy-into-string (lambda (dest dest-offset src src-offset src-end)
			   (string-fold (lambda (c loc)
					   (string-set! dest loc c)
					   (+ loc 1))
					 dest-offset src src-offset src-end)
			   dest))


(define substring       (lambda (s start end) 
			  (copy-into-string (make-string (- end start)) 0 s start end)))

(define display-list-body (lambda (l)
			    (if (null? l) #t
				(begin
				  (display (car l))
				  (if (null? (cdr l)) #t
				      (if (pair? (cdr l))
					  (begin
					    (print-char #\space)
					    (display-list-body (cdr l)))
					  (begin
					    (print-char #\space)
					    (print-char #\.)
					    (print-char #\space)
					    (display (cdr l))
					    )))))))

(define display-list (lambda (l)
		       (print-char #\()
		       (display-list-body l)
		       (print-char #\))))

(define display-string 
  (lambda (s) 
    (string-fold
     (lambda (c x) (display c))
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

(define list->string 
  (lambda (l)
    (let ((s (make-string (length l))))
      (fold (lambda (i c) (string-set! s i c) (+ i 1)) 0 l)
      s)))

(define vector->list
  (lambda (s) (reverse (vector-fold (lambda (c l) (cons c l)) nil s 0 (vector-length s)))))

(define string->list vector->list)

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
    