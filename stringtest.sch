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
  (display x)
)

(begin
  (print-char #\newline)
  (print-char #\")
  (print-char #\newline)
  (print-char #\')
  (print-char #\newline)
  (print-char #\\)
  (print-char #\newline)
  )
