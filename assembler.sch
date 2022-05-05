(define vm-constants-alist (list (cons "PUSH"		0)
				 (cons "POP"		1)
				 (cons "SWAP"		2)
				 (cons "DUP"		3)
				 (cons "ROT"		4)
				 (cons "CALL"		5)
				 (cons "RET"		6)
				 (cons "JMP"		7)
				 (cons "JTRUE"		8)
				 (cons "END"		9)
				 (cons "ADD"		10)
				 (cons "MUL"		11)
				 (cons "SUB"		12)
				 (cons "DIV"		13)
				 (cons "MOD"		14)
				 (cons "SHL"		15)
				 (cons "SHR"		16)
				 (cons "BOR"		17)
				 (cons "BAND"		18)
				 (cons "EQ"		19)
				 (cons "LT"		20)
				 (cons "STOR"		21)
				 (cons "LOAD"		22)
				 (cons "ALOC"		23)
				 (cons "GETC"		24)
				 (cons "DUMP"		25)
				 (cons "PINT"		26)
				 (cons "PCHR"		27)
				 (cons "RDRR"		28)
				 (cons "WTRR"		29)
				 (cons "ISNUM"		30)
				 (cons "ISLCONST"	31)
				 (cons "ISPTR"		32)
				 (cons "ISBOOL"		33)
				 (cons "ISCHR"		34)
				 (cons "ISINS"		35)
				 (cons "PBIN"           36)
				 (cons "PBLCONSTI"      37)
				 (cons "PBVCONSTI"      38)
				 (cons "PBPTRI"         39)
				 (cons "TRUE"          128)
				 (cons "FALSE"         129)))

(define append-vm-constant print-vconst)
(define append-language-constant    print-lconst)
(define append-number      print-binary)
(define append-pointer     print-pointer)
(define append-character print-vconst)

(define make-label-definition
  (lambda (ins pc)
    (letrec ((helper (lambda (i)
		       (if (>= i (string-length ins)) #f
			   (if (char=? (string-ref ins i) #\,) i
			       (helper (+ i 1)))))))
      (let ((comma (helper 1)))
	(if comma
	    (list (substring ins 1 comma) pc 
		  (string->number (substring ins (+ comma 1) (string-length ins))))
	    (list (substring ins 1 (string-length ins)) pc 0))))))

(define append-label-definition
  (lambda (ref labels)
    (let ((tuple (assoc ref labels)))
      (let ((loc   (cadr  tuple))
	    (sz    (caddr tuple)))
	(print-pointer loc sz)))))

(define char->digit
  (lambda (d)
    (if (char=? d #\0) 0
	(if (char=? d #\1) 1
	    (if (char=? d #\2) 2
		(if (char=? d #\3 ) 3
		    (if (char=? d #\4) 4
			(if (char=? d #\5) 5
			    (if (char=? d #\6) 6
				(if (char=? d #\7) 7
				    (if (char=? d #\8) 8
					(if (char=? d #\9 ) 9
					    (begin 
					      (display "Error: '")
					      (display d)
					      (display "' is not a digit!\n")
					      (quit))))))))))))))

(define string->number
  (lambda (s)
    (string-fold
     (lambda (c n)
       (+ (* n 10) (char->digit c))) 0 s 0 (string-length s))))

(define assemble-instruction 
  (lambda (ins pc labels)
    (display "assemble-instruction ")(display ins)(display "\n")
    (let ((c (string-ref ins 0)))
      (if (char=? c #\")
	  (append-character (string-ref ins 1))
	  (if (char=? c #\n)
	      (append-number (string->number (substring ins 1 (string-length ins))))
	      (if (char=? c #\l)
		  (append-language-constant (string->number (substring ins 1 (string-length ins))))
		  (if (char=? c #\@)
		      (append-label-definition ins labels)		  
		      (append-vm-constant (cdr (assoc ins vm-constants-alist)))
		      )))))))

(define assemble-instructions
  (lambda (instrs pc labels)
    (if (null? instrs) #t
	(begin
	  (assemble-instruction (car instrs) pc labels)
	  (assemble-instructions (cdr instrs (+ pc 1) labels))))))

(define discard-to-nl (lambda () (let ((r (read-char)))
				   (if (char=? r #\newline) #t
				       (discard-to-nl)))))
(define is-space?
  (lambda (c) (if (eof-object? c) #t
		  (if (char=? c #\space) #t
		      (if (char=? c #\tab) #t
			  (if (char=? c #\newline) #t #f))))))

(define drop-chars-until (lambda (f) (let ((x (read-char))) (if (f x) x (drop-chars-until f)))))
(define next-non-ws      (lambda ()  (drop-chars-until (lambda (z) (or (eof-object? z)
								       (not (is-space? z)))))))


(define read-to-ws
  (lambda (c)
    (letrec ((helper (lambda (acc)
		       (let ((r (read-char)))
			 (if (is-space? r)
			     (list->string (reverse acc))
			     (helper (cons r acc)))))))
      (helper (list c)))))
      
(define read-assembly
  (lambda (instrs pc labels)
    (let ((c (next-non-ws)))
      (if (eof-object? c)
	  (cons (reverse instrs) labels)
	  (if (char=? c #\;)
	      (begin
		(drop-chars-until (lambda (z) (char=? z #\newline)))
		(read-assembly instrs pc labels))
	      (let ((tok (read-to-ws c)))
		(if (char=? c #\:)
		  (let ((lbl   (make-label-definition tok pc)))
		    (read-assembly instrs pc (cons lbl labels)))
		  (read-assembly (cons tok instrs) (+ pc 1) labels))))))))

(define assemble
  (lambda ()
    (let ((instrs-labels (read-assembly '() 0 '())))
      (assemble-instructions (car instrs-labels)
			     (cdr instrs-labels)))))

(assemble)
;; (display (read-assembly '() 0 '()))
;; (let ((xs (read-assembly '() 0 '())))
;;   (let ((instrs (car xs))
;; 	(labels (cadr xs)))
;;     (display instrs)))
		       
