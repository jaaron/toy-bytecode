(define is-upcase-letter? (lambda (c) (and (char>=? c #\A) (char<=? c #\Z))))
(define char-is-digit?    (lambda (c) (if (char>=? c #\0) (char<=? c #\9) #f)))
(define char-is-hexdigit? (lambda (c) (if (or (and (char>=? c #\0) (char<=? c #\9))
					      (and (char>=? c #\a) (char<=? c #\f))
					      (and (char>=? c #\A) (char<=? c #\F))) 
					  #t
					  #f)))
(define is-space?     (lambda (c) (if (eof-object? c) #t
				      (if (char=? c #\space) #t
					  (if (char=? c #\tab) #t
					      (if (char=? c #\newline) #t #f))))))    
(define char->digit
  (lambda (c)
    (if (char=? c #\0) 0
	(if (char=? c #\1) 1
	    (if (char=? c #\2) 2
		(if (char=? c #\3) 3
		    (if (char=? c #\4) 4
			(if (char=? c #\5) 5
			    (if (char=? c #\6) 6
				(if (char=? c #\7) 7
				    (if (char=? c #\8) 8
					(if (char=? c #\9) 9 #f))))))))))))
(define char->hexdigit
  (lambda (c)
    (if (char-is-digit? c) (char->digit c)
	(if (or (char=? c #\a) (char=? c #\A)) 10
	    (if (or (char=? c #\b) (char=? c #\B)) 11
		(if (or (char=? c #\c) (char=? c #\C)) 12
		    (if (or (char=? c #\d) (char=? c #\D)) 13
			(if (or (char=? c #\e) (char=? c #\E)) 14
			    (if (or (char=? c #\f) (char=? c #\F)) 15 #f)))))))))

(define asm-read-num 
  (lambda (c)
    (letrec ((dec-helper (lambda (acc sign c)
			   (if (char-is-digit? c) 
			       (dec-helper (+ (* 10 acc) (char->digit c))
					   sign
					   (read-char))
			       (cons (* acc sign) (read-char)))))
	     (hex-helper (lambda (acc c)
			   (if (char-is-hexdigit? c) 
			       (hex-helper (+ (* 16 acc) (char->hexdigit c))
					   (read-char))
			       (cons acc (read-char))))))
      (if (char=? c #\0) 
	  (let ((next (read-char)))
	    (if (or (char=? next #\x) (char=? next #\X))
		(hex-helper 0 (read-char))
		(dec-helper 0 1 next)))
	  (if (char=? #\+ c) 
	      (dec-helper 0 1 (read-char))
	      (if (char=? #\- c)
		  (dec-helper 0 -1 (read-char))
		  (dec-helper 0 1 c)))))))

(define asm-read-identifier 
  (lambda (c)
    (letrec ((helper (lambda (acc c) 
		       (if (or (and (char<=? #\a c) (char<=? c #\z))
			       (and (char<=? #\A c) (char<=? c #\Z))
			       (and (char<=? #\0 c) (char<=? c #\9))
			       (char=? c #\_))
			   (helper (cons c acc) (read-char))
			   (cons (list->string (reverse acc)) c)))))
      (if (or (and (char<=? #\a c) (char<=? c #\z))
	      (and (char<=? #\A c) (char<=? c #\Z))
	      (char=? c #\_))
	  (helper (list c) (read-char))
	  (begin
	    (display "ERROR: reading identifier, expected [a-zA-Z_]. Got '")
	    (display c)
	    (display "'\n")
	    (quit))))))

(define asm-reader-state-number
  (lambda (c)
    (let ((n (asm-read-num c)))
      (cons (cons "number" (car n)) (cdr n)))))

(define asm-reader-state-pointer
  (lambda (c)
    (let ((base (asm-read-num c)))
      (if (char=? (cdr base) #\,)
	  (let ((offset (asm-read-num (cdr base))))
	    (cons (list "pointer" (car base) (car offset))
		  (cdr offset)))
	  (begin
	    (display "Error reading pointer literal, expected ',' got: ")
	    (display (cdr base))
	    (quit))))))

(define asm-reader-state-vm-const 
  (lambda (c)
    (let ((n (asm-read-num c)))
      (cons (cons "vm-const" (car n)) (cdr n)))))

(define asm-reader-state-lang-const 
  (lambda (c)
    (let ((n (asm-read-num c)))
      (cons (cons "lang-const" (car n)) (cdr n)))))

(define asm-reader-state-string
  (lambda (c)
    (letrec ((state-unescaped 
	      (lambda (acc c) 
		(if (eof-object? c)
		    (begin
		      (display "ERROR: EOF Encountered while scanning string\n")
		      (quit))
		    (if (char=? #\" c) 
			(cons (reverse acc) (read-char))
			(if (char=? #\\ c)
			    (state-escaped (cons c acc) (read-char))
			    (state-unescaped (cons c acc) (read-char)))))))
	     (state-escaped
	      (lambda (acc c) 
		(if (eof-object? c)
		    (begin
		      (display "ERROR: EOF Encountered while scanning string\n")
		      (quit))
		    (state-unescaped (cons c acc) (read-char))))))
      (let ((r (state-unescaped nil c)))
	(cons (cons "string" (list->string (car r))) (cdr r))))))

(define asm-reader-state-char 
  (lambda (c)
    (let ((the-char (if (char=? c #\\) 
			(let ((cc (read-char)))
			  (if (char=? cc #\t) #\tab
			      (if (char=? cc #\n) #\newline
				  (if (char=? cc #\') #\'
				      (if (char=? cc #\") #\"
					  (if (char=? cc #\\) #\\
					      (begin (display "Unknown escaped sequence: \\")
						     (display cc)
						     (quit))))))))
			c)))
      (let ((close-q (read-char)))
	(if (char=? close-q #\')
	    (cons (cons "char" the-char) (read-char))
	    (begin
	      (display "ERROR: while reading character. Expected close ' got: ")
	      (display close-q)
	      (quit)))))))
					  
(define asm-reader-state-comment
  (lambda (c) 
    (if (char=? c #\newline) 
	(asm-reader-state-entrance (read-char))
	(asm-reader-state-comment  (read-char)))))

(define asm-reader-state-semicolon
  (lambda (c) 
    (if (char=? c #\;)
	(asm-reader-state-comment (read-char))
	(begin
	  (display "ERROR: expected semicolon, got '")
	  (display c)
	  (display "'\n")
	  (quit)))))

(define asm-reader-state-definition
  (lambda (c)
    (let ((ident (asm-read-identifier c)))
      (if (char=? (cdr ident) #\,)
	  (let ((size (asm-read-num (read-char))))
	    (cons (cons "definition" (cons (car ident) (car size)))
		  (cdr size)))
	  (cons (cons "definition" (cons (car ident) nil))
		(cdr ident))))))

(define asm-reader-state-reference
  (lambda (c)
    (let ((ident (asm-read-identifier c)))
      (cons (cons "reference" (car ident))
	    (cdr ident)))))


(define asm-reader-state-instruction 
  (lambda (c)
    (letrec ((helper (lambda (acc c)
		       (if (is-upcase-letter? c)
			   (helper (cons c acc) (read-char))
			   (cons (reverse acc) c)))))
      (let ((i (helper (list c) (read-char))))
	(cons (cons "instruction" (list->string (car i))) (cdr i))))))
 
(define asm-reader-state-entrance 
  (lambda (c)
    (if (eof-object? c) #f
	(if (is-space? c)
	    (asm-reader-state-entrance (read-char))
	    (if (char=? c #\n) 
		(asm-reader-state-number (read-char))
		(if (char=? c #\p)
		    (asm-reader-state-pointer (read-char))
		    (if (char=? c #\v)
			(asm-reader-state-vm-const (read-char))
			(if (char=? c #\l)
			    (asm-reader-state-lang-const (read-char))
			    (if (char=? c #\')
				(asm-reader-state-char (read-char))
				(if (char=? c #\;)
				    (asm-reader-state-semicolon (read-char))
				    (if (char=? c #\#)
					(asm-reader-state-comment (read-char))
					(if (char=? c #\")
					    (asm-reader-state-string (read-char))
					    (if (char=? c #\:)
						(asm-reader-state-definition (read-char))
						(if (char=? c #\@)
						    (asm-reader-state-reference (read-char))
						    (asm-reader-state-instruction c)))))))))))))))



(define look-ahead #f)
(define asm-next-token
  (lambda ()
    (if (not look-ahead)
	(begin 
	  (set! look-ahead (read-char))
	  (asm-next-token))
	(let ((z (asm-reader-state-entrance look-ahead)))
	  (if z
	      (begin
		(set! look-ahead (cdr z))
		(car z))
	      z)))))

(define display-char-asm
  (lambda (c)
    (display #\')
    (display 
     (if (char=? c #\') "\\'" 
	 (if (char=? c #\tab) "\\t" 
	     (if (char=? c #\newline) "\\n"
		 (if (char=? c #\\) "\\\\" c)))))
    (display #\')))

(define print-asm-tok
  (lambda (asm-tok)
    (let ((typ (car asm-tok))
	  (val (cdr asm-tok)))
      (if (string=? typ "number")
      	  (begin
      	    (display #\n)
      	    (display val))
      	  (if (string=? typ "vm-const")
      	      (begin (display #\v) (display val))
      	      (if (string=? typ "lang-const")
      		  (begin (display #\l) (display val))
      		  (if (string=? typ "pointer")
      		      (begin
      			(display #\p)
      			(display (car val))
      			(display #\,)
      			(display (cdr val)))
      		      (if (string=? typ "string")
      			  (begin (display #\") (display val) (display #\"))
			  (if (string=? typ "char")
			      (display-char-asm val)
			      (if (string=? typ "reference")
				  (begin (display #\@) (display val))
				  (if (string=? typ "definition")
				      (begin (display #\:) 
					     (display (car val))
					     (if (not (null? (cdr val)))
						 (begin (display #\,) (display (cdr val))) #f))
				      (if (string=? typ "instruction")
					  (display val)
					  (begin
					    (display "Unknown token type: ") 
					    (display typ)
					    (quit))))))))))))))

(define go
  (lambda ()
    (let ((tok (asm-next-token)))
      (if tok 
	  (begin
	    ;; (display tok)
	    ;; (newline)
	    (print-asm-tok tok)
	    (newline)
	    (go)) nil))))

(go)
				  