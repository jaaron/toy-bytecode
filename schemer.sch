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

; utility functions
(define last (lambda (l) (if (and (not (null? l)) 
				  (and (pair? l)
				       (and (pair? (cdr l))
					    (not (null? (cdr l))))))
			     (last (cdr l))
			     l)))

(define string-is-numeric? (lambda (str)
			     (let ((first-char (string-ref str 0))
				   (strlen     (string-length str))
				   (test (lambda (c v) (if v (char-is-digit? c) v))))
			       (and (< 0 strlen)
				    (or (and (< 1 strlen)
					     (and
					      (or (char=? first-char #\-)
						  (or (char=? first-char #\+)
						      (char-is-digit? first-char)))
					      (string-fold test #t str 1 (string-length str))))
					(char-is-digit? first-char))))))


; The underlying VM uses a tagged memory model that differentiates between
; numbers, pointers, vm constants, and language constants. 
; Numeric values in the assembly must be tagged with an appropriate identifier
; to convince the assembler to tag the cell with the appropriate type.  
;

; string constants for each instruction
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

;; The instructions are all used at most once in the compiler so its
;; cheaper to include them as string literals then to clutter up the
;; environment with them.

;; (define ins-mul	"MUL")
;; (define ins-div	"DIV")
;; (define ins-mod	"MOD")
;; (define ins-shl     	"SHL")
;; (define ins-shr     	"SHR")
;; (define ins-bor     	"BOR")
;; (define ins-band	"BAND")
;; (define ins-getc	"GETC")
;; (define ins-dump	"DUMP")
;; (define ins-pint	"PINT")
;; (define ins-pchr	"PCHR")
;; (define ins-islconst	"ISLCONST")
;; (define ins-ischr	"ISCHR")
;; (define ins-isins	"ISINS")

; Language constants
(define false-value  "FALSE")
(define true-value   "TRUE")

; return a string containing the asm representation of a number
(define asm-number  (lambda (x) (string-append "n" (if (number? x) (number->string x) x))))
; return a string containing the asm representation of a pointer
(define asm-pointer (lambda (x y) (string-append "p" (if (number? x) (number->string x) x) 
						 "," (if (number? x) (number->string y) y)
						 )))
; return a string containing the asm representation of a language constant
(define asm-lang-const (lambda (x) (string-append "l" (if (number? x) (number->string x) x))))

(define asm-label-reference  (lambda (name) (string-append "@" name)))
(define asm-label-definition (lambda (name) (string-append ":" name)))
(define asm-label-definition-sz (lambda (name sz)
				  (string-append (asm-label-definition name) 
						 (string-append "," (number->string sz)))))


(define ptr-type-offset (asm-number 0))

(define consbox-size       2)
(define asm-consbox-size   (asm-number consbox-size))
(define consbox-car-offset (asm-number 0))
(define consbox-cdr-offset (asm-number 1))

(define vector-type-flag     (asm-lang-const 2))
(define vector-length-offset (asm-number 1))
(define raw-vector-elems-offset 2)
(define vector-elems-offset  (asm-number raw-vector-elems-offset))
(define string-type-flag     (asm-lang-const 3))
(define string-length-offset vector-length-offset)
(define string-chars-offset  vector-elems-offset)
(define symbol-type-flag     (asm-lang-const 3))


; A few builtin forms are handled specially
; by the compiler.  Some of these will later
; be subsumed by macro capabilities but for right
; now they are just special voodoo.
;
; This list associates the symbols with special
; compiler functions.
(define special-forms 
  (lambda ()
    (list
     (cons "set!"   compile-set!)    
     (cons "lambda" compile-lambda)
     (cons "let"    compile-let)
     (cons "letrec" compile-letrec)
     (cons "if"     compile-if)
     (cons "define" compile-define)  
     (cons "begin"  compile-begin)
     (cons "quote"  compile-quote)
     (cons "and"    compile-and)
     (cons "or"     compile-or)
     )
    ))

; search the list of builtin forms for a 
; particular form   
(define find-special
    (lambda (f) 
      (letrec ((helper (lambda (ss)
			 (if (null? ss) #f
			     (if (string=? f (car (car ss))) 
				 (cdr (car ss))
				 (helper (cdr ss)))))))
	(helper (special-forms)))))

; The top-level-env is a list containing the list of symbols 
; defined by the compiler at the top level.
;
; The runtime environment is represented as a list of vectors
; references to symbols are replaced with a traversal of this
; structure based on the level of enclosing scope where the symbol was
; defined, and the index of the variable in that scope.  This is taken
; directly from the SECD machine's representation of the environment.
;
; For example suppose we have something like:
;   (((lambda (x) 
;        (lambda (z) (+ x z)))
;        5)
;      7)
;
; The inner lambda (that gets returned and applied to the arg 7) refers to three
; different symbols:  '+' 'x' and 'z' that are each declared at a different depth
; in the enclosing environment. 
; When this lambda is evaluated (with the argument 7) the environment will look like:
;   ([7]
;    [5]
;    ["=" "null?" "cons" "car" "cdr" "+" ...])
;
; So the reference to symbol 'z' will be compiled to (vector-ref (car env) 0)
;    the reference to symbol 'x' will be compiled to (vector-ref (car (cdr env)) 0) and
;    the reference to symbol '+' will be compiled to (vector-ref (car (cdr (cdr env))) 5)
;
; Note: this isn't strictly accurate since the symbols 'vector-ref',
;       'car' and 'cdr' are themselves defined in the environment and
;       would thus require lookups making this expansion impossible.
;       What really happens is that a non-closure form of the car and
;       cdr procedures are invoked directly.  See the functions
;       u-call-* below.

(define top-level-env 
  (quote ((("equal?"             "equal")
	   ("="			 "equal")
	   ("<"			 "less_than")
	   ("null?"	         "null_q")
	   ("cons"		 "cons")
	   ("car"		 "car")
	   ("cdr"		 "cdr")
	   ("+"			 "add")	   
	   ("-"		         "subtract")
	   ("*"		         "multiply")
	   ("%"		         "modulo")
	   ("/"		         "divide") 	   
	   ("print-char"	 "print_char") 
	   ("print-num"		 "print_num")
	   ("string?"	         "string_q") 
	   ("number?"	         "number_q")
	   ("char?"	         "char_q")
	   ("pair?"	         "pair_q")
	   ("read-char"		 "read_char")
	   ("quit"		 "quit")
	   ("set-car!"	         "set_car")
	   ("set-cdr!"	         "set_cdr") 
	   ("make-vector"	 "make_vector")
	   ("vector-ref"	 "vector_ref")
	   ("vector-set!"        "vector_set")
	   ("vector-length"	 "vector_length") 
	   ("make-string"	 "make_string")
	   ("string-set!"	 "vector_set")
	   ("string-ref"	 "vector_ref")
	   ("string-length"	 "vector_length") 
	   ("vapply"             "vapply")
	   ("char=?"		 "equal")     ;; for characters
	   ("char<?"		 "less_than") ;; for characters
	   ("eof-object?"	 "eof_object_q")))))

(define top-level-env-endptr  (last (car top-level-env)))

(define initial-env-label "__initial_environment")

(define append-named-consbox
  (lambda (name car-value cdr-value)
    (append-instructions 
     (asm-label-definition-sz name consbox-size)
     car-value
     cdr-value)
    name))

(define append-consbox 
  (lambda (car-value cdr-value)
    (append-named-consbox (fresh-label) car-value cdr-value)))

(define append-list-as-vector 
  (lambda (vec-list)
    (let ((lbl (fresh-label)))
      (append-instructions
	(asm-label-definition-sz lbl (+ (length vec-list) raw-vector-elems-offset))
	vector-type-flag
	(asm-number (length vec-list)))
      (apply append-instructions vec-list)
      lbl)))

(define append-initial-env
  (lambda ()
    (append-named-consbox "__nil" 
			  (asm-label-reference "__nil")
			  (asm-label-reference "__nil"))
    (append-named-consbox
     initial-env-label    
     (asm-label-reference 
      (append-list-as-vector (map (lambda (l) (asm-label-reference (cadr l))) (car top-level-env))))
     (asm-label-reference "__nil"))))

; (fresh-label) is used to generate labels for each lambda 
; expression and for string constants.  
(define label-counter 0)
(define fresh-label (lambda ()
		      (set! label-counter (+ label-counter 1))
		      (string-append "__anonymous" (number->string label-counter))
		      ))

; append an instruction to the ouptut stream
(define append-instruction
  (lambda (ins)
    (begin 
      (display ins)
      (display "\n"))))
  
; append a list of instructions to the output stream
(define append-instructions 
  (lambda inss
    (letrec ((helper (lambda (inss)
		       (if (null? inss) (quote ())
			   (begin
			     (append-instruction (car inss))
			     (helper (cdr inss)))))))
      (helper inss))))


;
; We're now actually into the guts of the compiler.  
; The conventions are as follows:
;   Functions of the form 
;       - 'assembly-foo' take no arguments, append asm instructions
;                        to the output stream for completing the task
;                        foo, and return no useful result.
;
;       - 'u-call-foo'   serve as a wrapper around the assembly-foo
;                        functions.  For larger blocks of assembly code
;                        the u-call-foo insert a CALL to the definitions,
;                        shorter ones are inlined.  Again, no useful result
;                        is returned.
;
;       - 'compile-foo'  these are the main compiler functions.  All of 
;                        these take atleast two arguments, the s-expression 
;                        to compile and the symbolic environment list used
;                        to resolve references.  Some of these take a boolean
;                        'rest' argument which is a bad hack to support tail-call
;                        optimization.  If 'rest' is false it means their is 
;                        definitely no continuation to this expression and so
;                        a closure invocation can be optimized by not storing the
;                        return environment and using a JMP rather than a CALL
;                        (see assembly-funcall vs. assembly-tailcall below).  
;                        All compile-* functions must return either 0-arity function
;                        or false.  The 0-arity function represents work that is 
;                        being delayed until after the compilation of the main
;                        program body, e.g., the body of lambda expressions.
;                        It is vital that these return values be propagated out
;                        to the main compiler loop 'do-compiler-task'

			   
; assembly for the primitive list functions car, cdr, and cons
; (ptr) -> ((car ptr))
(define assembly-car  (lambda () (append-instructions ins-push consbox-car-offset ins-load)))

; (ptr) -> ((cdr ptr))
(define assembly-cdr  (lambda () (append-instructions  ins-push consbox-cdr-offset ins-load)))


; (cdr car) -> ((cons car cdr))
(define assembly-cons (lambda ()
			(append-instructions 
			 ins-push asm-consbox-size	; (car cdr 2)
			 ins-aloc			; (car cdr hp)
			 ins-push consbox-cdr-offset	; (car cdr hp 1)
			 ins-stor			; (car hp) cdr stored
			 ins-push consbox-car-offset	; (car hp 0)
			 ins-stor)			; (hp)  cdr stored
			))


; top is the cons box to set, then the new value
(define assembly-set-car 
  (lambda () (append-instructions ins-push consbox-car-offset ins-stor)))
(define assembly-set-cdr 
  (lambda () (append-instructions ins-push consbox-cdr-offset ins-stor)))

; these define how to call the three primitives car, cdr, and cons as
; part of larger compiler generated sequences (e.g., function application)
; for car, we just inline the assembly.  For cdr and cons we do a machine level
; call into a function.
(define u-call-car  (lambda () (assembly-car))) ; car is 3 instructions, a function call is the same length
					        ; so there is no reason not to inline it.

(define u-call-cdr  (lambda () (assembly-cdr))) ; same with cdr.

(define u-call-cons ; cons is really big (13 instructions)! we'll never inline it
  (lambda () (append-instructions ins-push (asm-label-reference "__u_cons") ins-call)))

(define u-call-set-car (lambda () (assembly-set-car)))
(define u-call-set-cdr (lambda () (assembly-set-cdr)))

(define u-call-make-vector (lambda () (append-instructions 
					ins-push (asm-label-reference "__u_make_vector_nofill")
					ins-call)))

; function application convention
; top of stack is the closure to apply, then the arguments
; this is tricky.  We need to cons the argument list onto 
; the closure's environment,  store the existing
; environment pointer to the stack, set the environment 
; pointer to the new list, invoke the closure's code,
; then restore the environment pointer on return.
;
(define assembly-make-args-helper (lambda (nr-args)
				    (if (= nr-args 0) #f
					(begin 
					  (append-instructions 
					   ins-push (asm-number (+ (+ raw-vector-elems-offset nr-args) -1))
					   ins-stor)
					  (assembly-make-args-helper (- nr-args 1))))))

(define assembly-make-args (lambda (nr-args)
			     (append-instructions
			      ins-push  (asm-number nr-args))
			     (u-call-make-vector)
			     (assembly-make-args-helper nr-args)))

;; special case for referencing arguments to this function (i.e., depth = 0).
(define assembly-get-arg
  (lambda (idx)
    (append-instruction ins-rdrr)
    (u-call-car)
    (append-instructions
     ins-push (asm-number (+ raw-vector-elems-offset idx))
     ins-load)))

(define assembly-set-arg
  (lambda (idx)
    (append-instruction ins-rdrr)
    (u-call-car)
    (append-instructions 
     ins-push (asm-number (+ raw-vector-elems-offset idx))
     ins-stor
     ins-pop)))

(define assembly-nrargs
  (lambda ()
    (append-instruction ins-rdrr)
    (u-call-car)
    (append-instructions 
     ins-push vector-length-offset
     ins-load)))

; (args clos) -> ((clos args)) 
(define assembly-funcall (lambda ()
			   (append-instructions ; (args clos rp)
			    (asm-label-definition "__funcall_tramp")
			    ins-rot             ; (rp args clos)
			    ins-dup)            ; (rp args clos clos)
			    (u-call-car)        ; (rp args clos env)
			    (append-instructions 
			     ins-swap		; (rp args env clos)
			     ins-rot)		; (rp clos args env)
			    (u-call-cons)	; (rp clos (args . env)*)
			    (append-instructions
			     ins-rdrr		; (rp clos (args . env) renv)
			     ins-swap		; (rp clos renv (args . env) )
			     ins-wtrr		; (rp clos renv) rr = (args . env)
			     ins-rot)		; (renv rp clos) 
			    (u-call-cdr)        ; (renv rp clos-code)
			    (append-instruction ins-jmp)))

(define u-call-funcall (lambda ()
			 (append-instructions ins-push (asm-label-reference "__funcall_tramp")
					      ins-call
					      ins-swap
					      ins-wtrr)))

; tail calls are sneakier we avoid saving the current
; env pointer. 
; (args clos) -> ((clos args))
 (define assembly-tailcall (lambda ()
			    (append-instructions
			     (asm-label-definition "__tailcall_tramp")
			     ins-dup)				; (renv rp args clos clos)
			    (u-call-car)			; (renv rp args clos env)			    
			    (append-instructions
			     ins-swap				; (renv rp args env clos)
			     ins-rot)				; (renv rp clos args env)
			    (u-call-cons)			; (renv rp clos (args . env)* )
			    (append-instruction ins-wtrr)	; (renv rp clos) rr = (args . env)
								; note that we didn't store the current env
								; this is a tail call so we'll return straight
								; to the current renv/rp!
			    (u-call-cdr)			; (renv rp code)
			    (append-instruction ins-jmp)	; we jump into the call with 
								;   (renv rp) 
								; on return we'll have pc = rp, and
								;   (renv rval) on the stack
								; just as on return from non-tail call above.
			    ))

(define u-call-tailcall (lambda ()
			  (append-instructions "PUSH" (asm-label-reference "__tailcall_tramp")
					       "JMP")))

; returning is simple since cleanup is handled by the caller
(define assembly-funret (lambda () (append-instruction ins-ret)))
			   
; Assembly for loading a cell from the environment.
; assembly-env-cell places the cons box whose car is 
; at the desired offsets on the stack.  
; assembly-env-val actually loads the value.  
(define assembly-env-vec
  (lambda (depth)
    (append-instructions 
     ins-rdrr						; (env)
     ins-push (asm-number depth)			; (env d)
     ins-push (asm-label-reference "__u_nth_cell")	; (env d u_nth)
     ins-call)
    (u-call-car)))

(define assembly-env-val
  (lambda (env-length depth idx)
    (if (= env-length (+ depth 1))  ;; getting something from top-level-env
	(begin
	  (append-instructions 
	   ins-push (asm-label-reference initial-env-label))
	  (u-call-car)
	  (append-instructions 
	   ins-push (asm-number (+ raw-vector-elems-offset idx))
	   ins-load))
	(if (= depth 0) 
	    (assembly-get-arg idx)
	    (begin
	      (assembly-env-vec depth)
	      (append-instructions
	       ins-push (asm-number (+ raw-vector-elems-offset idx))
	       ins-load))))))

(define assembly-set-env-val
  (lambda (env-length depth idx)
    (if (= env-length (+ depth 1))
	(begin 
	  (append-instructions
	   ins-push (asm-label-reference initial-env-label))
	  (u-call-car)
	  (append-instructions 
	   ins-push (asm-number (+ raw-vector-elems-offset idx))
	   ins-stor))
	(begin
	  (assembly-env-vec depth)
	  (append-instructions
	   ins-push (asm-number (+ raw-vector-elems-offset idx)) ins-stor)))))

(define assembly-nil      
  (lambda ()
    (append-instructions  ins-push (asm-label-reference "__nil") )))

; Lookup functions,  find a particular symbol in the symbolic environment
; list. These are complimentary to the assembly-env-* functions above.
(define lookup-reference-offset 
  (lambda (r e cont)
    (if (null? e) (cont #f)		
	(if (string=? r (if (list? (car e)) (car (car e)) (car e)))
	    (cont 0)
	    (lookup-reference-offset r (cdr e)
				     (lambda (z) 
				       (cont (if z (+ z 1) z))))))))

(define lookup-reference-depth
  (lambda (r e cont)
    (if (null? e) (cont #f)
	(lookup-reference-offset r (car e) 
				 (lambda (z) 
				   (cont 
				    (if z 
					(cons 0 z)
					(lookup-reference-depth r (cdr e) 
								(lambda (w) 
								  (if w 
								      (cons (+ (car w) 1) (cdr w))
								      #f))))))))))

(define lookup-reference 
  (lambda (r e)
    (lookup-reference-depth r e (lambda (x) x))))

; do-compile-task is the main compiler loop. 
; it takes a 0-arity function to invoke (or false),
; and recurs on the result of invoking the function.
(define do-compile-task 
  (lambda (t) (if t (do-compile-task (t)) #f)))

; Compilation functions

(define compile-number 
  (lambda (c env) (append-instructions ins-push (asm-number c)) #f))


(define calculate-string-list-length
  (lambda (strl n)
    (if (null? strl) n	
	(calculate-string-list-length
	 (if (char=? (car strl) #\\) (cdr (cdr strl)) (cdr strl))
	 (+ n 1)))))

(define calculate-string-length
  (lambda (str)
    (calculate-string-list-length (string->list str) -2)))

(define compile-string 
 (lambda (s env) 
   (let ((strlabel (fresh-label))
	 (strlen (calculate-string-length s)))
     (append-instructions ins-push (asm-label-reference strlabel))
     (lambda ()         
       (append-instructions 
	(asm-label-definition-sz strlabel (+ strlen 2))
	string-type-flag
	(asm-number strlen)
	s)
       #f))))

(define calculate-symbol-length 
  (lambda (s) 0))

(define compile-symbol
  (lambda (s env)
    (let ((symlabel (fresh-label))
	  (symlen (calculate-symbol-length s)))
      (append-instructions ins-push (asm-label-reference symlabel))
      (lambda ()
	(append-instructions 
	 (asm-label-definition-sz symlabel (+ symlen 2))
	 symbol-type-flag
	 (asm-number symlen)
	 (string-append "\"" (string-append s "\"")))
	#f))))

; this doesn't handle escaped chars except newline, tab, quote, double quote and backslash
(define compile-char
  (lambda (s env)
    (append-instructions 
     ins-push 	   
     (string-append "'" 
		    (string-append
		     (if (string=? s "#\\tab") 
			 "\\t"
			 (if (string=? s "#\\newline")
			     "\\n" 
			     (if (string=? s "#\\\\")
				 "\\\\"
				 (if (string=? s "#\\'")
				     "\\'"
				     (if (string=? s "#\\\"") "\\\""
					 (if (string=? s "#\\space")
					     " "
					     (substring s 2 3))))))) 
		     "'"))) #f))

(define compile-reference 
  (lambda (r env)
    (let ((i (lookup-reference r env)))
      (if i 
	  (begin
	    (append-instruction (string-append ";; Resolving symbol " r))
	    (assembly-env-val (length env) (car i) (cdr i))
	    (append-instruction (string-append ";; Resolved symbol " r)))
	  ;; this is an error
	  (begin
	    ;; this should really write to stderr.
	    (display (string-append "Undefined symbol: " r))
	    (newline)
	    (quit))
	  )
      #f
      )))

(define compile-atom 
  (lambda (x env quoted) 
    (if (string=? x "#t")
	(begin (append-instructions ins-push true-value) #f)
	(if (string=? x "#f")
	    (begin (append-instructions ins-push false-value) #f)
	    (if (string-is-numeric? x)
		(compile-number x env) 
		(if (char=? (car (string->list x)) #\")
		    (compile-string x env)
		    (if (char=? (car (string->list x)) #\#)
			(compile-char x env)
			(if (string=? "nil" x) 
			    (begin (assembly-nil) #f)
			    (if quoted
				(compile-symbol x env)
				(compile-reference x env))))))))))

(define list-part
  (lambda (l)
    (letrec ((helper (lambda (l acc)
		       (if (pair? l)
			   (helper (cdr l) (cons (car l) acc))
			   (reverse acc)))))
      (helper l '()))))

(define process-params
  (lambda (plist)
    (if (list? plist) 
	(begin ;; (display "plist is list")
	       ;; (newline)
	       plist)
	(let ((fixed-params    (list-part plist))
	      (variadic-param  (if (pair? plist) (cdr (last plist)) plist)))
	  (let ((nr-fixed-params (length fixed-params)))
		(append-instructions
		 ins-push (asm-number nr-fixed-params)
		 ins-push (asm-label-reference "__u_make_varargs_list")
		 ins-call
		 ins-pop)
		(append fixed-params (list variadic-param)))))))

; Hm, we should probably be flagging code pointers with something
; so that we can avoid gc'ing them. Right now the VM just assumes the 
; code is statically defined below initial heap pointer but in order
; to support eval we'll have to do something more clever later.
(define compile-lambda 
  (lambda (l env rest)
    (let ((label (fresh-label)))
      (append-instructions
       ins-rdrr ins-push (asm-label-reference label) )
      (u-call-cons)
      (lambda ()	
	(append-instruction (asm-label-definition label))
	(let ((r (compile-sequence (cddr l)
				   (cons 
				    (process-params (cadr l))
				    env) #f)))
	  (assembly-funret)
	  r)))))

(define compile-let-bindings
  (lambda (bs env)
    (if (null? bs)  #f
	(let ((r2 (compile-sexp (car (cdr (car bs))) env #t))
	      (r1 (compile-let-bindings (cdr bs) env)))
	  (lambda ()
	    (do-compile-task r1)
	    (do-compile-task r2))))))

(define compile-let
  (lambda (l env rest)
    (let ((r1 (compile-let-bindings (car (cdr l)) env))
	  (e (map (lambda (x) (car x)) (car (cdr l)))))
      (assembly-make-args (length (cadr l)))
      (append-instruction ins-rdrr)
      (u-call-cons)
      (append-instruction ins-wtrr)
      (let ((r2 (compile-sequence (cdr (cdr l)) (cons e env) rest)))
	(if rest
	    (begin
	      (append-instruction ins-rdrr)
	      (u-call-cdr)
	      (append-instruction ins-wtrr))
	    #f)
	(lambda ()
	  (do-compile-task r1)
	  (do-compile-task r2))
	))))

(define compile-set! 
  (lambda (l env rest)
    (let ((cell-id (lookup-reference (cadr l) env)))
      (let ((r (compile-sexp (caddr l) env #t)))
	(assembly-set-env-val (length env) (car cell-id) (cdr cell-id))
	r))))

(define compile-letrec
  (lambda (l env rest)
    (letrec ((empty-binders (map (lambda (b) (list (car b) (list "quote" '())))
				 (cadr l)))
	     (helper        (lambda (binders body)
			      (if (null? binders) body
				  (helper (cdr binders)
					  (cons (cons "set!" (car binders))
						body))))))
      (compile-sexp 
       (cons "let" 
	     (cons empty-binders
		   (helper (reverse (cadr l))
			   (cddr l))))
       env rest))))

(define compile-begin
  (lambda (l env rest) (compile-sequence (cdr l) env rest)))

(define compile-sequence 
  (lambda (l env rest)
    (if (null? l) #f
	(let ((r1 (compile-sexp (car l) env (if (null? (cdr l)) rest #t) )))
	  (if (not (null? (cdr l)))
	      (append-instruction ins-pop) 
	      #f
	      )
	  (let ((r2 (compile-sequence (cdr l) env rest)))
	    (lambda ()
	      (do-compile-task r1)
	      (do-compile-task r2)
	      ))))))

; define is really sneaky in that it has to modify
; the environment (both symbolic and the non) so that
; whatever symbol is being defined can be referenced in
; lambda bodies that were declared previously (which is
; part of why lambda body compilation is delayed until
; after the main compilation). This involves using set-car!
; to modify both environment pointers such that
; (car post-env) == (cons v (car pre-env))
; where v is the value of the defined symbol and pre-env and
; post-env are the environments before and after the call.
(define compile-define
  (lambda (l env rest)
    (append-instruction (string-append ";; Definition of " (car (cdr l))))
    (let ((v (lookup-reference (car (cdr l)) env))
	  (r (compile-sexp (car (cdr (cdr l))) env #t)))
      (if v
	  (begin
	    (append-instruction (string-append ";; Updating binding " (car (cdr l))))
	    (assembly-set-env-val (length env) (car v) (cdr v)))
	  (begin
	    (assembly-set-env-val (length env) (- (length env) 1) (length (car top-level-env)))
	    (set-cdr! top-level-env-endptr
		      (cons (list (car (cdr l)) "__nil")
			    (cdr top-level-env-endptr)))
	    (set! top-level-env-endptr (cdr top-level-env-endptr))))
      (append-instruction ins-pop)
      r)))
  
(define compile-and
  (lambda (l env rest)
    (let ((out-label (fresh-label)))
      (let ((r1    (compile-sexp (cadr l) env #t))
	    (xx    (append-instructions ins-dup
					ins-push false-value
					ins-eq
					ins-push (asm-label-reference out-label)
					ins-jtrue
					ins-pop))
	    (r2    (compile-sexp (caddr l) env #t)))
	(append-instruction (asm-label-definition out-label))
	(lambda ()
	  (do-compile-task r1)
	  (do-compile-task r2))))))
      
(define compile-or
  (lambda (l env rest)
    (let ((out-label (fresh-label))
	  (t2-label  (fresh-label)))
      (let ((r1      (compile-sexp (cadr l) env #t))
	    (xx      (append-instructions ins-dup
					  ins-push false-value
					  ins-eq
					  ins-push (asm-label-reference t2-label)
					  ins-jtrue
					  ins-push  (asm-label-reference out-label)
					  ins-jmp
					  (asm-label-definition t2-label)
					  ins-pop))
	    (r2      (compile-sexp (caddr l) env #t)))
	(append-instructions (asm-label-definition out-label))
	(lambda ()
	  (do-compile-task r1)
	  (do-compile-task r2))))))

; when we can detect application of a builtin
; we can avoid function call overhead and just inline the assembly
(define compile-if
  (lambda (l env rest)
    (if (not (= (length l) 4)) 
	(begin 
	  (display "Error in compile-if wrong number of arguments\n\t")
	  (display l)
	  (newline)
	  (quit))
	(let ((false-label (fresh-label))
	      (join-label (fresh-label))
	      (conditional (car (cdr l)))
	      (true-case  (car (cdr (cdr l))))
	      (false-case (car (cdr (cdr (cdr l))))))
	  (let ((r1 (compile-sexp conditional env #t))
		(x  (append-instructions 
		     ins-push false-value
		     ins-eq
		     ins-push (asm-label-reference false-label)
		     ins-jtrue))
		(r2 (compile-sexp true-case env rest))
		(y  (append-instructions ins-push (asm-label-reference join-label) ins-jmp
					 (asm-label-definition false-label)))
		(r3 (compile-sexp false-case env rest)))
	    (append-instruction (asm-label-definition join-label))
	    (lambda ()
	      (do-compile-task r1)
	      (do-compile-task r2)
	      (do-compile-task r3)
	      )
	    )
	  ))))

(define compile-quoted-sexp
  (lambda (s env rest)
    (if (pair? s)
	(let ((r2 (compile-quoted-sexp (car s) env #t))
	      (r1 (compile-quoted-sexp (cdr s) env #t))
	      )
	  (u-call-cons)
	  (lambda () 
	    (do-compile-task r1) 
	    (do-compile-task r2) 
	    #f))
	(if (null? s)
	    (begin 
	      (assembly-nil)
	      #f)
	    (compile-atom s env #t)))))

(define compile-quote 
  (lambda (s env rest)
    (compile-quoted-sexp (car (cdr s)) env rest)))

(define compile-arguments
  (lambda (n l env)
    (if (null? l) 
	(assembly-make-args n)
	(let ((r2 (compile-sexp (car l) env #t))
	      (r1 (compile-arguments n (cdr l) env)))
	  (lambda () 
	    (do-compile-task r1)
	    (do-compile-task r2)
	    )))))

(define compile-list
  (lambda (l env rest)
    (let ((s (find-special (car l))))
      (if s 
	  (s l env rest)
	  (let ((r1 (compile-arguments (length (cdr l)) (cdr l) env))
		(r2 (compile-sexp (car l) env #t)))	   	    
	    (if rest
		(u-call-funcall)
		(u-call-tailcall)
		)
	    (lambda ()
	      (do-compile-task r1)
	      (do-compile-task r2)))))))

(define compile-sexp 
  (lambda (s env rest)
    (if (list? s) 
	(compile-list s env rest) 
	(compile-atom s env #f))))

(define assembly-builtin-header 
  (lambda (name)
    (let ((uu-name (string-append "__" name)))
      (append-named-consbox name 
			    (asm-label-reference initial-env-label)
			    (asm-label-reference uu-name))
      (append-instruction (asm-label-definition uu-name)))))

(define define-builtin-functions
  (lambda (initial-env)
    (begin
      (let ((loop            (fresh-label))
	    (out             (fresh-label)))
						;; (display "variadic plist")
	(append-instructions 
	 (asm-label-definition "__u_make_varargs_list")
	 ins-swap)                              ;; (rp nr-fixed-params)
	(assembly-nil)				;; (rp nr-fixed-params nil)
	(assembly-nrargs)			;; (rp nr-fixed-params nil nr-args)
	(append-instructions 
	 (asm-label-definition loop)		;; (rp nr-fixed-params l i)
	 ins-rot				;; (rp i nr-fixed-params l)
	 ins-rot				;; (rp l i nr-fixed-params)
	 ins-dup				;; (rp l i nr-fixed-params nr-fixed-params)
	 ins-rot				;; (rp l nr-fixed-params i nr-fixed-params)
	 ins-swap				;; (rp l nr-fixed-params nr-fixed-params i)
	 ins-dup				;; (rp l nr-fixed-params nr-fixed-params i i)
	 ins-rot				;; (rp l nr-fixed-params i i nr-fixed-params)
	 ins-sub				;; (rp l nr-fixed-params i (- i nr-fixed-params))
	 ins-push (asm-number 0)		;; (rp l nr-fixed-params i (- i nr-fixed-params) 0)
	 ins-eq					;; (rp l nr-fixed-params i (= (- i nr-fixed-params) 0))
	 ins-push (asm-label-reference out)	;; (rp l nr-fixed-params i (= (- i nr-fixed-params) 0) out)
	 ins-jtrue				;; (rp l nr-fixed-params i)
	 ins-rot				;; (rp i l nr-fixed-params)
	 ins-rot				;; (rp nr-fixed-params i l)
	 ins-swap				;; (rp nr-fixed-params l i)
	 ins-push (asm-number 1)		;; (rp nr-fixed-params l i 1)
	 ins-sub				;; (rp nr-fixed-params l (- i 1))
	 ins-dup				;; (rp nr-fixed-params l (- i 1) (- i 1))
	 ins-rot				;; (rp nr-fixed-params (- i 1) i (- i 1))
	 ins-rdrr)				;; (rp nr-fixed-params (- i 1) i (- i 1) env)
	(u-call-car)				;; (rp nr-fixed-params (- i 1) i (- i 1) (car env))
	(append-instructions 
	 ins-swap				;; (rp nr-fixed-params (- i 1) l (car env) (- i 1))
	 ins-push vector-elems-offset		;; (rp nr-fixed-params (- i 1) l (car env) (- i 1) vector-elems-offset)
	 ins-add				;; (rp nr-fixed-params (- i 1) l (car env) (+ (- i 1) vector-elems-offset))
	 ins-load				;; (rp nr-fixed-params (- i 1) l (aref (car env) (- i 1)))
	 ins-swap)				;; (rp nr-fixed-params (- i 1) (aref (car env) (- i 1)) l)
	(u-call-cons)				;; (rp nr-fixed-params (- i 1) (cons (aref (car env) (- i 1)) l))
	(append-instructions
	 ins-swap				;; (rp nr-fixed-params (cons (aref (car env) (- i 1)) l) (- i 1))
	 ins-push (asm-label-reference loop)	;; (rp nr-fixed-params (cons (aref (car env) (- i 1)) l) (- i 1) loop)
	 ins-jmp
	 (asm-label-definition out)		;; (rp l nr-fixed-params i)
	 ins-pop				;; (rp l nr-fixed-params)
	 ins-rdrr)				;; (rp l nr-fixed-params env)
	(u-call-car)				;; (rp l nr-fixed-params (car env)))
	(append-instructions
	 ins-swap				;; (rp l (car env) nr-fixed-params)
	 ins-push vector-elems-offset		;; (rp l (car env) nr-fixed-params vector-elems-offset)
	 ins-add				;; (rp l (car env) (+ nr-fixed-params vector-elems-offset))
	 ins-stor				;; (rp (car env))
	 ins-ret)))				;; ((car env))

    (begin
      (append-instructions
       (asm-label-definition "__u_nth_cell")			; (l n r)
       ins-rot							; (r l n)
       (asm-label-definition "__u_nth_cell_loop")
       ins-dup							; (r l n n)
       ins-push (asm-number 0)					; (r l n n 0)
       ins-eq							; (r l n (= n 0))
       ins-push (asm-label-reference "__u_nth_cell_done")	; (r l n (= n 0) u_nth_cell_done)
       ins-jtrue						; (r l n)
       ins-push (asm-number -1)					; (r l n -1)
       ins-add							; (r l (- n 1))
       ins-swap)						; (r (- n 1) l)
      (u-call-cdr)						; (r (- n 1) (cdr l))
      (append-instructions 
       ins-swap							; (r (cdr l) (- n 1))
       ins-push (asm-label-reference "__u_nth_cell_loop")	; (r (cdr l) (- n 1) u_nth_cell_loop)
       ins-jmp
       (asm-label-definition "__u_nth_cell_done")		; (r l 0)
       ins-pop							; (r l)
       ins-ret))
    
    (begin
      (append-instructions
       (asm-label-definition "__u_cons")	; this is the internal entry point
					; stack is (car cdr rp)
       ins-rot
       ins-push (asm-label-reference "__cons_body")
       ins-jmp)					; stack is (rp  car cdr)
      (assembly-builtin-header "cons")
      (assembly-get-arg 0)
      (assembly-get-arg 1)
      (append-instruction (asm-label-definition "__cons_body"))
      (assembly-cons)
      (assembly-funret))

    (begin
      (assembly-builtin-header "car")
      (assembly-get-arg 0)					; stack is (renv rp arg)
      (append-instruction (asm-label-definition "__car_body"))	; now we're at the body
      (assembly-car)						; dump the assembly for car
      (assembly-funret))					; and a return statement

								; this is completely analogous to above
    (begin
      (assembly-builtin-header "cdr")
      (assembly-get-arg 0)
      (append-instruction (asm-label-definition "__cdr_body"))
      (assembly-cdr)
      (assembly-funret))

	; arithmetic ops
	; these aren't used internally so we just define them.
    (begin 
      (assembly-builtin-header "add")
      (assembly-get-arg 0)
      (assembly-get-arg 1)
      (append-instruction ins-add)
      (assembly-funret))

    (begin
      (assembly-builtin-header "subtract")
      (assembly-get-arg 0)
      (assembly-get-arg 1)    
      (append-instruction ins-sub)
      (assembly-funret))

    (begin
      (assembly-builtin-header "multiply")
      (assembly-get-arg 0)
      (assembly-get-arg 1)
      (append-instruction "MUL")
      (assembly-funret))

    (begin
      (assembly-builtin-header "divide")
      (assembly-get-arg 0)
      (assembly-get-arg 1)
      (append-instruction "DIV")
      (assembly-funret))

    (begin
      (assembly-builtin-header "modulo")
      (assembly-get-arg 0)
      (assembly-get-arg 1)
      (append-instruction "MOD")
      (assembly-funret))

	; equality comparison
    (begin 
      (assembly-builtin-header "equal")
      (assembly-get-arg 0)
      (assembly-get-arg 1)
      (append-instruction ins-eq)
      (assembly-funret))

	; less than comparison
    (begin 
      (assembly-builtin-header "less_than")
      (assembly-get-arg 1)
      (assembly-get-arg 0)
      (append-instruction ins-lt)
      (assembly-funret))
    
	; assignments
    (begin
      (assembly-builtin-header "set_car")
      (assembly-get-arg 1)
      (assembly-get-arg 0)
      (u-call-set-car)
      (assembly-funret))
    
    (begin
      (assembly-builtin-header "set_cdr")
      (assembly-get-arg 1)
      (assembly-get-arg 0)
      (u-call-set-cdr)
      (assembly-funret))


	; questions
    (begin
      (assembly-builtin-header "null_q")
      (assembly-get-arg 0)
      (assembly-nil)
      (append-instruction ins-eq)
      (assembly-funret))

    (begin
      (assembly-builtin-header "char_q")
      (assembly-get-arg 0)
      (append-instruction "ISCHR")
      (assembly-funret))

    (begin
      (assembly-builtin-header "number_q")
      (assembly-get-arg 0)
      (append-instruction ins-isnum)
      (assembly-funret))

    (begin
      (assembly-builtin-header "string_q")
      (assembly-get-arg 0)
      (append-instructions
       ins-dup 
       ins-isptr
       ins-push (asm-label-reference "__string_q_is_ptr")
       ins-jtrue 
       ins-pop
       ins-push false-value)
      (assembly-funret)
      (append-instructions
       (asm-label-definition "__string_q_is_ptr")
       ins-push ptr-type-offset
       ins-load
       ins-push string-type-flag
       ins-eq)
      (assembly-funret))    
    
    (begin
      (assembly-builtin-header "print_num")
      (assembly-get-arg 0)
      (append-instructions ins-dup "PINT")
      (assembly-funret))

    (begin
      (assembly-builtin-header "print_char")
      (assembly-get-arg 0)
      (append-instructions ins-dup "PCHR")
      (assembly-funret))

    (begin
      (assembly-builtin-header "read_char")
      (append-instruction "GETC")
      (assembly-funret))

    (begin
      (assembly-builtin-header "quit")
      (append-instruction ins-end))

    (begin
      (assembly-builtin-header "pair_q")
      (assembly-get-arg 0)
      (append-instructions 
       ins-dup						; (rp x x)
       ins-isptr					; (rp x)
       ins-push (asm-label-reference "__pair_q_isptr")	; (rp x (is-ptr? x) @pair_q_isptr)
       ins-jtrue					; (rp x)
       (asm-label-definition "__pair_q_isnil")
       ins-pop						; (rp)
       (asm-label-definition "__pair_q_islconst") 
       ins-push false-value)				; (rp false)
      (assembly-funret)
							; this isn't the greatest heuristic,
							; but at this point if the target of a 
							; pointer is not a language constant,
							; then the pointer points to a cons box.
      (append-instructions
       (asm-label-definition "__pair_q_isptr")		; (rp x)
       ins-dup						; (rp x x)
       ins-push (asm-label-reference "__nil")		; (rp x x nil)
       ins-eq						; (rp x (= x nil))
       ins-push (asm-label-reference "__pair_q_isnil")	; (rp x (= x nil) @pair_q_is_nil)
       ins-jtrue					; (rp x)
       ins-push ptr-type-offset				; (rp x 0)
       ins-load    
       "ISLCONST"
       ins-push (asm-label-reference "__pair_q_islconst")
       ins-jtrue	       
       ins-push true-value)
      (assembly-funret))
    
	; getting the length of a string is easy
    (begin
      (assembly-builtin-header "vector_length")
      (assembly-get-arg 0)
      (append-instructions ins-push string-length-offset ins-load)
      (assembly-funret))
    
    (begin
      (assembly-builtin-header "vector_set")
      (assembly-get-arg 2)		; (c)
      (assembly-get-arg 0)		; (c vec)
      (assembly-get-arg 1)		; (c vec n)
      (append-instructions
       ins-push vector-elems-offset	; (c vec n 2)
       ins-add				; (c vec (+ n 2))
       ins-stor)			; (vec)      
      (assembly-funret))
    
    (begin 
      (assembly-builtin-header "vector_fill")
      (assembly-get-arg 0)					; (s)
      
      (append-instructions 
       ins-push (asm-number 0)					; (s 0)
       (asm-label-definition "__vector_fill_loop")		; (s n)
       ins-swap							; (n s)
       ins-dup							; (n s s)
       ins-rot							; (s n s)
       ins-push vector-length-offset				; (s n s vector-length-offset)
       ins-load							; (s n total-length)
       ins-swap							; (s total-length n)
       ins-dup							; (s total-length n n)
       ins-rot							; (s n total-length n)
       ins-eq							; (s n (= total-length n))
       ins-push (asm-label-reference "__vector_fill_done")	; (s n (= total-length n) @done)
       ins-jtrue						; (s n)
       ins-dup							; (s n n)
       ins-rot							; (n s n)
       ins-push vector-elems-offset				; (n s n offset)
       ins-add)							; (n s (+ n offset))
      (assembly-get-arg 1)					; (n s (+ n offset) v)
      (append-instructions
       ins-rot							; (n v s (+ n offset))
       ins-stor							; (n s)
       ins-swap							; (s n)
       ins-push (asm-number 1)					; (s n 1)
       ins-add							; (s (+ n 1))
       ins-push (asm-label-reference "__vector_fill_loop")
       ins-jmp
       (asm-label-definition "__vector_fill_done")		; (s n)
       ins-pop)
      (assembly-funret))

    (begin 
      (append-instructions 
       (asm-label-definition "__u_make_vector_nofill")	; (n rp)
       ins-swap						; (rp n)
       ins-dup						; (n n)
       ins-push vector-elems-offset	       		; (n n vector-elems-offset)
       ins-add						; (n (+ n vector-elems-offset))
       ins-aloc						; (n v)
       ins-push vector-type-flag			; (n v vector-type-flag)
       ins-swap						; (n vector-type-flag v)
       ins-push ptr-type-offset				; (n vector-type-flag v ptr-type-offset)
       ins-stor						; (n v)
       ins-push vector-length-offset			; (n v vector-size-offset)
       ins-stor						; (v)
       ins-ret)
      (assembly-builtin-header "make_vector")
      (assembly-get-arg 0)				; n
      (append-instructions
       ins-push (asm-label-reference "__u_make_vector_nofill")
       ins-call)					; (v)
      (assembly-set-arg 0)
      (append-instructions 
       ins-push (asm-label-reference "__vector_fill") 
       ins-jmp))

    (begin
      (assembly-builtin-header "make_string")
      (assembly-get-arg 0)							; (n)
      (append-instructions 
       ins-dup									; (n n)
       ins-push string-chars-offset      					; (n n 2)
       ins-add									; (n (+ n 2))
       ins-aloc									; (n s)
       ins-push string-length-offset     					; (n s 1)
       ins-stor									; (s)
       ins-push string-type-flag						; (s string-type-flag)
       ins-swap									; (string-type-flag s)
       ins-push ptr-type-offset							; (string-type-flag s 0)
       ins-stor									; (s)
       )
      (assembly-nrargs)								; (s nr-args)
      (append-instructions 
       ins-push (asm-number 2)							; (s nr-args 2)
       ins-eq									; (s (= nr-args 2))
       ins-push (asm-label-reference "__make_string_two_args")			; (s (is-char? c?) make_string_two_args)
       ins-jtrue)								; (s)
      (assembly-funret)
      (append-instruction (asm-label-definition "__make_string_two_args"))	; (s)
      (assembly-set-arg 0)
      (append-instructions 
       ins-push (asm-label-reference "vector_fill")
       ins-jmp))

    (begin
      (assembly-builtin-header "vector_ref")
      (assembly-get-arg 0)
      (assembly-get-arg 1)
      (append-instructions 
       ins-push vector-elems-offset
	     ins-add
	     ins-load)
      (assembly-funret)) 			    
    
    (begin 
      (assembly-builtin-header "vapply")
      (assembly-get-arg 1)
      (assembly-get-arg 0)) ; fall thru into tailcall    
    (assembly-tailcall)     ; do not move the definition of tailcall!!

    (assembly-funcall)

    (begin 
      (assembly-builtin-header "eof_object_q")
      (assembly-get-arg 0)
      (append-instructions
       ins-push "EOF" ins-eq)
      (assembly-funret))))
	   

; compiler-run is the entry point into the compilation system.
; it reads a sexp, checks for EOF compiles the sexp, and then 
; calls do-compile-task with a function that will call compiler-run
; again and then return the delayed work.  If EOF is found, and END
; instruction is written and the delayed work finally gets evaluated.
(define compiler-run 
  (lambda ()
    (let ((sp (read-sexp)))
      (if sp
	  (let ((r (compile-sexp sp top-level-env #t)))
	    (do-compile-task 
	     (lambda () 
	       (compiler-run) 
	       r)))
	  (begin
	    (append-instruction ins-end))))))


; Into the reader.  
;
; The reader is pretty simple.  
; read-sexp calls either read-atom or read-list
; read-atom reads an atom and returns a pair with the atom read, 
; and the next character of the input stream.
;
; read-list reads sexps until the returned 'next-char' is a close peren, 
; then returns the list read, and the next non-whitespace character.
; reader utility functions
(define is-space?     (lambda (c) (if (eof-object? c) #t
				      (if (char=? c #\space) #t
					  (if (char=? c #\tab) #t
					      (if (char=? c #\newline) #t #f))))))
(define is-delimiter? (lambda (c) (if (is-space? c) #t
				      (if (char=? c #\() #t
					  (if (char=? c #\)) #t
					      (if (char=? c #\") #t
						  (if (char=? c #\; ) #t 
						      (if (char=? c #\.) #t
							  #f))))))))
(define char-is-digit?(lambda (c) (if (char>=? c #\0) (char<=? c #\9) #f)))
(define is-char-name? (lambda (s) (if (string=? s "newline") #t 
				      (if (string=? s "space") #t
					  (if (string=? s "tab") #t #f)))))

(define drop-chars-until (lambda (f) (let ((x (read-char))) (if (f x) x (drop-chars-until f)))))
(define next-non-ws      (lambda ()  (drop-chars-until (lambda (z) (not (is-space? z))))))

(define reader-state-hash-backslash 
  (lambda (c)
    (if (char=? c #\space)
	(cons "#\\space" (read-char))
	(if (char=? c #\tab)
	    (cons "#\\tab" (read-char))
	    (if (char=? c #\newline)
		(cons "#\\newline" (read-char))
		(if (char=? c #\()
		    (cons "#\\(" (read-char))
		    (if (char=? c #\))
			(cons "#\\)" (read-char))
			(if (char=? c #\")
			    (cons "#\\\"" (read-char))
			    (if (char=? c #\;)
				(cons "#\\;" (read-char))
				(if (char=? c #\.)
				    (cons "#\\." (read-char))
				    (let ((r (reader-state-wordchar c)))
				      (cons (string-append "#\\" (car r)) (cdr r)))))))))))))

(define reader-state-hash
  (lambda (c)
    (if (char=? c #\\)
	(reader-state-hash-backslash (read-char))
	(if (char=? c #\t)
	    (cons "#t" (read-char))
	    (if (char=? c #\f)
		(cons "#f" (read-char))
		(begin
		  (display "ERROR: Unrecognized # sequence\n")
		  (quit)))))))

(define reader-state-wordchar
  (lambda (c) 
    (letrec ((helper (lambda (c)
		       (if (eof-object? c) (cons '() c)
			   (if (is-delimiter? c) (cons '() c)
			       (let ((r (helper (read-char))))
				 (cons (cons c (car r)) (cdr r))))))))
      (let ((r (helper c)))
	(cons (list->string (car r)) (cdr r))))))

(define reader-state-semicolon 
  (lambda (c) 
    (if (eof-object? c) #f
	(if (char=? c #\newline) 
	    (reader-state-entrance (read-char))
	    (reader-state-semicolon (read-char))))))

(define reader-state-string-escaped
  (lambda (c) 
    (if (eof-object? c)
	(begin
	  (display "ERROR: EOF Encountered while scanning string\n")
	  (quit))
	(let ((r (reader-state-string-unescaped (read-char))))
	  (cons (cons c (car r)) (cdr r))))))

(define reader-state-string-unescaped
  (lambda (c) 
    (if (eof-object? c)
	(begin
	  (display "ERROR: EOF Encountered while scanning string\n")
	  (quit))
	(if (char=? #\" c) 
	    (cons (cons c '()) (read-char))
	    (let ((r (if (char=? #\\ c)
			 (reader-state-string-escaped (read-char))
			 (reader-state-string-unescaped (read-char)))))
	      (cons (cons c (car r)) (cdr r)))))))
	    

(define reader-state-open-string
  (lambda (c)
    (let ((r (reader-state-string-unescaped c)))
      (cons (list->string (cons #\" (car r))) (cdr r)))))

(define reader-state-entrance
  (lambda (c)
    (if (eof-object? c)
	#f
	(if (is-space? c)
	    (reader-state-entrance (read-char))
	    (if (char=? c #\#)
		(reader-state-hash (read-char))
		(if (char=? c #\")
		    (reader-state-open-string (read-char))
		    (if (char=? c #\()
			(cons "(" (read-char))
			(if (char=? c #\))
			    (cons ")" (read-char))				   
			    (if (char=? c #\')
				(cons "'" (read-char))
				(if (char=? c #\.)
				    (cons "." (read-char))
				    (if (char=? c #\;)
					(reader-state-semicolon (read-char))					
					(reader-state-wordchar c))))))))))))

(define look-ahead #f)
(define next-token
  (lambda ()
    (let ((z (if look-ahead
		 (reader-state-entrance look-ahead)
		 (reader-state-entrance (read-char)))))
      (if z
	  (begin
	    (set! look-ahead (cdr z))	    
	    (car z))
	  z))))

(define parse-token
  (lambda (tok)
    (if tok
	(if (string=? tok "(")
	    (read-list)
	    (if (string=? tok "'")
		(cons "quote" (cons (read-sexp) '()))
		tok)) #f)))

(define read-dotted-cdr
  (lambda ()
    (let ((tok    (next-token))
	  (cparen (next-token)))
      (if (if tok (not cparen) #f)
	  (begin
	    (display "ERRROR: EOF Encountered while reading list\n")
	    (quit))
	  (if (not (string=? cparen ")"))
	      (begin
		(display "ERROR: Expected ) in dotted cdr. Got: ")
		(display cparen)
		(newline)
		(quit))
	      (parse-token tok))))))

(define read-list 
  (lambda ()
    (let ((x (next-token)))
      (if (not x)
	(begin
	  (display "ERRROR: EOF Encountered while reading list\n")
	  (quit))  
	(if (string=? x ")") '() 
	    (if (string=? x ".")
		(read-dotted-cdr)
		(cons (parse-token x) (read-list))))))))


(define read-sexp
  (lambda () (parse-token (next-token))))

; then run the compiler.
(append-instructions 
 ins-push (asm-label-reference initial-env-label) ins-wtrr)
(compiler-run)
(append-initial-env)
(define-builtin-functions initial-env-label)
