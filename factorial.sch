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

(define factorial (lambda (x) (if (= x 0) 1 
				  (* x (factorial (- x 1))))))
(define repeat (lambda (f n) 
	      (if (= n 1) (f)
		  (begin
		    (f)
		    (repeat f (- n 1))))))

(repeat (lambda ()
	  (begin
	    (print-num (factorial 10))
	    (print-char #\newline)))
	  100)
