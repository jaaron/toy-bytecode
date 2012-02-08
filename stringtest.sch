; Test of basic string handling functions
(define print-string-list 
  (lambda (x)
    (if (null? x) '()
	(begin
	  (print-char (car x))
	  (print-string-list (cdr x))))))

(define print-string
  (lambda (s) (print-string-list (string->list s))))

(print-string "The string \"Hello world\\n\" is ")
(print-num (string-length "Hello world\n"))
(print-string " characters long.\n")

(if (string? 5)
    (print-string "5 is a string")
    (print-string "5 is not a string"))
(print-char #\newline)

(let ((x  (make-string 8 #\c)))
  (string-set! x 6 #\d)
  (print-string x)
)

(print-char #\newline)
(print-char #\")
(print-char #\newline)
(print-char #\')
(print-char #\newline)
(print-char #\\)
(print-char #\newline)
