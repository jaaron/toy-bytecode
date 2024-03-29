(define ins-push	"PUSH")
(define ins-pop		"POP")
(define ins-swap	"SWAP")
(define ins-dup		"DUP")
(define ins-rot		"ROT")
(define ins-call	"CALL")
(define ins-ret		"RET")
(define ins-jmp		"JMP")
(define ins-jtrue	"JTRUE")
(define ins-end		"END")
(define ins-add		"ADD")
(define ins-sub		"SUB")
(define ins-eq		"EQ")
(define ins-lt		"LT")
(define ins-stor	"STOR")
(define ins-load	"LOAD")
(define ins-aloc	"ALOC")
(define ins-rdrr	"RDRR")
(define ins-wtrr	"WTRR")
(define ins-isnum	"ISNUM")
(define ins-isptr	"ISPTR")
(define ins-mul	        "MUL")
(define ins-div	        "DIV")
(define ins-mod	        "MOD")
(define ins-shl     	"SHL")
(define ins-shr     	"SHR")
(define ins-bor     	"BOR")
(define ins-band	"BAND")
(define ins-getc	"GETC")
(define ins-dump	"DUMP")
(define ins-pint	"PINT")
(define ins-pchr	"PCHR")
(define ins-islconst	"ISLCONST")
(define ins-ischr	"ISCHR")
(define ins-isins	"ISINS")
(define ins-pbin        "PBIN")
(define ins-pblconsti   "PBLCONSTI")
(define ins-pbvconsti   "PBVCONSTI")
(define ins-pbptri      "PBPTRI")

(define instruction-infos
  (list
   ;;    MNEMONIC  BBTERM CONSUMES   DELTA HAS-IMM STACKONLY
   (list ins-push     #f     0         1    #t      #t)
   (list ins-pop      #f     1        -1    #f      #t)
   (list ins-swap     #f     2         0    #f      #t)
   (list ins-dup      #f     0         1    #f      #t)
   (list ins-rot      #f     3         0    #f      #t)
   (list ins-add      #f     2        -1    #f      #t)
   (list ins-sub      #f     2        -1    #f      #t)
   (list ins-eq       #f     2        -1    #f      #t)
   (list ins-lt       #f     2        -1    #f      #t)
   (list ins-mul      #f     2        -1    #f      #t)
   (list ins-div      #f     2        -1    #f      #t)
   (list ins-mod      #f     2        -1    #f      #t)
   (list ins-shl      #f     2        -1    #f      #t)
   (list ins-shr      #f     2        -1    #f      #t)
   (list ins-bor      #f     2        -1    #f      #t)
   (list ins-band     #f     2        -1    #f      #t)
   (list ins-getc     #f     0         1    #f      #t)
   (list ins-dump     #f     0         0    #f      #t)
   (list ins-pint     #f     1        -1    #f      #t)
   (list ins-pchr     #f     1        -1    #f      #t)
   
   (list ins-stor     #f     3        -2    #f      #f)
   (list ins-load     #f     2        -1    #f      #f)
   (list ins-aloc     #f     1         0    #f      #f)
   (list ins-rdrr     #f     0         1    #f      #f)
   (list ins-wtrr     #f     1        -1    #f      #f)
   (list ins-isnum    #f     1         0    #f      #t)
   (list ins-isptr    #f     1         0    #f      #t)
   (list ins-islconst #f     1         0    #f      #t)
   (list ins-ischr    #f     1         0    #f      #t)
   (list ins-isins    #f     1         0    #f      #t)
   (list ins-pbin     #f     1        -1    #f      #t)
   (list ins-pblconsti #f    1        -1    #f      #t)
   (list ins-pbvconsti #f    1        -1    #f      #t)
   (list ins-pbptri    #f    1        -1    #f      #t)
   
   (list ins-call    #t)
   (list ins-ret     #t)
   (list ins-jmp     #t)
   (list ins-jtrue   #t)
   (list ins-end     #t)))


(define is-instr?      (lambda (tok) (string=? (car tok) tag-instruction)))
(define is-number?     (lambda (tok) (string=? (car tok) tag-number)))
(define is-definition? (lambda (tok) (string=? (car tok) tag-definition)))

(define lookup-instr-info
  (lambda (tok)
    (assoc (cdr tok) instruction-infos)))

(define is-terminal-instr?
  (lambda (tok)
    (and (is-instr? tok)
	 (let ((info (lookup-instr-info tok)))
	   (and (not (null? info))
		(cadr info))))))

(define instr-consumption-depth
  (lambda (tok)
    (if (is-instr? tok)
	(let ((info (lookup-instr-info tok)))
	  (if (not (null? info))
	      (caddr info) 0)) 0)))

(define instr-stack-delta
  (lambda (tok)
    (if (is-instr? tok) (cadddr (lookup-instr-info tok)) 0)))

(define is-push-instr? 
  (lambda (tok) (and (is-instr? tok)
		     (string=? (cdr tok) ins-push))))

(define is-pop-instr? 
  (lambda (tok) (and (is-instr? tok)
		     (string=? (cdr tok) ins-pop))))
(define is-swap-instr? 
  (lambda (tok) (and (is-instr? tok)
		     (string=? (cdr tok) ins-swap))))

(define is-rdrr-instr? (lambda (tok)
			 (and (is-instr? tok)
			      (string=? ins-rdrr (cdr tok)))))

(define is-load-instr? (lambda (tok)
			 (and (is-instr? tok)
			      (string=? ins-load (cdr tok)))))

(define is-binop-instr?
  (lambda (tok) (and (string=? (car tok) tag-instruction)
		     (or (string=? (cdr tok) ins-add)
			 (string=? (cdr tok) ins-sub)
			 (string=? (cdr tok) ins-eq)
			 (string=? (cdr tok) ins-lt)
			 (string=? (cdr tok) ins-mul)
			 (string=? (cdr tok) ins-div)
			 (string=? (cdr tok) ins-mod)
			 (string=? (cdr tok) ins-shl)
			 (string=? (cdr tok) ins-shr)
			 (string=? (cdr tok) ins-bor)
			 (string=? (cdr tok) ins-band)
			 ))))

(define is-stack-only-instr?
  (lambda (tok)
    (let ((info (assoc (cdr tok) instruction-infos)))
      (and (is-instr? tok)
	   (not    (cadr info))
	   (cadddr (cddr info))))))

(define instr-has-immediate?
  (lambda (tok)
    (let ((info (assoc (cdr tok) instruction-infos)))
      (and (is-instr? tok)
	   (not   (cadr info))
	   (caddr (cddr info))))))

(define tag-instruction "instruction")
(define tag-number      "number")
(define tag-pointer     "pointer")
(define tag-vm-const    "vm-const")
(define tag-lang-const  "lang-const")
(define tag-string      "string")
(define tag-char        "char")
(define tag-eof         "eof")
(define tag-definition  "definition")
(define tag-reference   "reference")

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
      (cons (cons tag-number (car n)) (cdr n)))))

(define asm-reader-state-pointer
  (lambda (c)
    (let ((base (asm-read-num c)))
      (if (char=? (cdr base) #\,)
	  (let ((offset (asm-read-num (cdr base))))
	    (cons (list tag-pointer (car base) (car offset))
		  (cdr offset)))
	  (begin
	    (display "Error reading pointer literal, expected ',' got: ")
	    (display (cdr base))
	    (quit))))))

(define asm-reader-state-vm-const 
  (lambda (c)
    (let ((n (asm-read-num c)))
      (cons (cons tag-vm-const (car n)) (cdr n)))))

(define asm-reader-state-lang-const 
  (lambda (c)
    (let ((n (asm-read-num c)))
      (cons (cons tag-lang-const (car n)) (cdr n)))))

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
      (let ((r (state-unescaped '() c)))
	(cons (cons tag-string (list->string (car r))) (cdr r))))))

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
	    (cons (cons tag-char the-char) (read-char))
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
	    (cons (cons tag-definition (cons (car ident) (car size)))
		  (cdr size)))
	  (cons (cons tag-definition (cons (car ident) '()))
		(cdr ident))))))

(define asm-reader-state-reference
  (lambda (c)
    (let ((ident (asm-read-identifier c)))
      (cons (cons tag-reference (car ident))
	    (cdr ident)))))


(define asm-reader-state-instruction 
  (lambda (c)
    (letrec ((helper (lambda (acc c)
		       (if (is-upcase-letter? c)
			   (helper (cons c acc) (read-char))
			   (cons (reverse acc) c)))))
      (let ((i (helper (list c) (read-char))))
	(let ((str (list->string (car i))))	  	      
	  (cons (if (string=? "FALSE" str)
		    (cons tag-vm-const #f)
		    (if (string=? "TRUE" str)
			(cons tag-vm-const #t)
			(if (string=? "EOF" str)
			    (cons tag-eof '())
			    (cons tag-instruction str))))
		(cdr i)))))))
 
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
      (if (string=? typ tag-number)
      	  (begin
      	    (display #\n)
      	    (display val))
      	  (if (string=? typ tag-vm-const)
      	      (if (equal? val #t) (display "FALSE")
		  (if (equal? val #f) (display "TRUE")
		      (begin (display #\v) (display val))))
	      (if (string=? typ tag-eof)
		  (display "EOF")
		  (if (string=? typ tag-lang-const)
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
						(quit)))))))))))))))

(define print-basic-block 
  (lambda (l)
    (if (null? l) #t (begin 
		       (print-asm-tok (car l))
		       (newline)
		       (print-basic-block (cdr l))))))

(define asm-read-basic-block 
  (lambda ()
    (letrec ((helper (lambda (acc)
		       (let ((tok (asm-next-token)))
			 (if (not tok) 
			     (begin
			       (reverse acc))
			     (begin
			       ;; (display "got token: ")
			       ;; (display tok)
			       ;; (display "\n")
			       (if (or (is-terminal-instr? tok)
				     (is-definition? tok))
				 (reverse (cons tok acc))
				 (helper (cons tok acc)))))))))
      (helper '()))))

			 
(define split
  (letrec ((helper (lambda (l n a)
		     (if (= n 0) (cons (reverse a) l)
			 (helper (cdr l) (- n 1) (cons (car l) a))))))
    (lambda (l n) (helper l n '()))))

(define fold (lambda (f s l) (if (null? l) s (fold f (f s (car l)) (cdr l)))))

(define matches
  (lambda (pat seq)
    (if (null? pat) #t
	(if (null? seq) #f
	    (and ((car pat) (car seq))
		 (matches (cdr pat) (cdr seq)))))))

(define mkinstr (lambda (i) (cons tag-instruction i)))
(define mknum   (lambda (n) (cons tag-number n)))

(define inliners
  (list (cons ins-add +)
	(cons ins-sub -)
	(cons ins-eq  =)
	(cons ins-lt <)
	(cons ins-mul *)
	(cons ins-div quotient)
	(cons ins-mod remainder)
	(cons ins-shl ash)
	(cons ins-shr (lambda (a b) (ash a (- 0 b))))
	(cons ins-bor logior)
	(cons ins-band logand)))

(define find-consumer
  (letrec ((helper (lambda (bb depth)
		     (if (null? bb) '()
			 (if (< (instr-consumption-depth (car bb))  depth) bb
			     (helper (cdr bb) (+ depth (instr-stack-delta (car bb)))))))))
    (lambda (bb) (helper bb 0))))			    

(define peephole 
  (let ((optimizers (list
		     ;; (push x pop) -> ()
		     (list 3 (lambda (xs) (and (is-push-instr? (car xs))
					       (is-pop-instr? (caddr xs))))
		     	   (lambda (xs ys) ys))
		     ;; (swap swap) -> ()
		     (list 2 (lambda (xs) (and (is-swap-instr? (car xs))
		     			       (is-swap-instr? (cadr xs))))
		     	   (lambda (xs ys) ys))
		     ;; (binop pop) -> (pop pop)
		     (list 2 (lambda (xs) (and (is-binop-instr? (car xs))
		     			       (is-pop-instr? (cadr xs))))
		     	   (lambda (xs ys) (cons (cons tag-instruction ins-pop)
		     				 (cons (cons tag-instruction ins-pop) ys))))
		     ;; (push x push y add)
		     (list 5 (lambda (xs) (matches (list is-push-instr?
		     					 is-number?
		     					 is-push-instr?
		     					 is-number?
		     					 is-binop-instr?) xs))
		     	   (lambda (xs ys)
		     	     (let ((n0 (cdr (cadr xs)))
		     		   (n1 (cdr (cadddr xs)))
		     		   (op (cdr (cadr (cdddr xs)))))
		     	       (append
		     		(list (cons tag-instruction ins-push)
		     		      (cons tag-number
		     			    ((cdr (assoc op inliners)) n0 n1)))
		     		ys))))
		     ;; accessing two local variables in a row
		     ;; (RDRR PUSH n0 LOAD PUSH x    LOAD RDRR PUSH n0 LOAD) ->
		     ;; (RDRR PUSH n0 LOAD DUP  PUSH x    LOAD SWAP)
		     (list 11 (lambda (xs)
				(matches (list is-rdrr-instr?
					       is-push-instr?
					       (lambda (x) (and (is-number? x)
								(= (cdr x) 0)))
					       is-load-instr?
					       is-push-instr?
					       is-number?
					       is-load-instr?
					       is-rdrr-instr?
					       is-push-instr?
					       (lambda (x) (and (is-number? x)
								(= (cdr x) 0)))
					       is-load-instr?) xs))
			   (lambda (xs ys) (let ((off (car (cddr (cdddr xs)))))
					     (append
					      (list (mkinstr ins-rdrr)
						    (mkinstr ins-push)
						    (mknum 0)
						    (mkinstr ins-load)
						    (mkinstr ins-dup)
						    (mkinstr ins-push)
						    off
						    (mkinstr ins-load)
						    (mkinstr ins-swap))
					      ys)))
			   ))))

    (letrec ((helper 
	      (lambda (changed bb acc)
		(if (= (length bb) 0) (cons changed (reverse acc))
		    (let ((changed-block  (fold (lambda (changed-block optimizer)
						  (let ((changed (car changed-block))
							(bb      (cdr changed-block))
							(len     (car optimizer))
							(check   (cadr optimizer))
							(repl    (caddr optimizer)))
						    (if (>= (length bb) len)
							(let ((s (split bb len)))
							  (let ((xs (car s))
								(ys (cdr s)))
							    ;; (display "Checking sequence: ")
							    ;; (display xs)
							    ;; (display "\n")
							    (if (check xs)
       								(cons #t (repl xs ys))
								(cons changed bb))))
							(cons changed bb))))
						(cons changed bb)
						optimizers)))
		      (let ((changed (car changed-block))
			    (bb      (cdr changed-block)))
			(if (null? bb) (cons changed (reverse acc))
			    (helper changed (cdr bb) (cons (car bb) acc)))))))))
      (lambda (bb)
	(let ((changed-block (helper #f bb '())))
	  (if (car changed-block)
	      (peephole (cdr changed-block))
	      (cdr changed-block)))))))


(define go
  (lambda ()
    (let ((bb  (peephole (asm-read-basic-block))))
      (if (not (null? bb))
	  (begin
	    ;; (display ";; basic block\n")
	    (display "")
	    (print-basic-block bb)
	    (go))
	  '()))))

(go)
				  
