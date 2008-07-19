(define print-string-list 
  (lambda (x)
    (if (null? x) nil
	(begin
	  (print-char (car x))
	  (print-string-list (cdr x))))))

(define print-string
  (lambda (s) (print-string-list (string->list s))))

(print-string "The string \"Hello world\\n\" is ")
(print-num (string-length "Hello world\n"))
(print-string " characters long.\n")