; utility functions
(define for-all? (lambda (f l) (if (null? l) 
				   #t
				   (if (f (car l))
				       (for-all? f (cdr l)) #f))))

; check if a string represents a numeric constant.
; i.e., the string "123" is numeric
(define is-numeric? (lambda (str)
		      (let ((r (string->list str)))
			(if (null? r) #f
			    (if (or (char=? (car r) #\-) (char=? (car r) #\+))
				(if (null? (cdr r)) #f (for-all? is-number? (cdr r)))
				(for-all? is-number? r))))))

(define calculate-string-list-length
  (lambda (strl n)
    (if (null? strl) n	
	(calculate-string-list-length
	 (if (char=? (car strl) #\\) (cdr (cdr strl)) (cdr strl))
	 (+ n 1)))))

(define calculate-string-length
  (lambda (str)
    (calculate-string-list-length (string->list str) -2)))

; The underlying VM uses a tagged memory model that differentiates between
; numbers, pointers, vm constants, and language constants. 
; Numeric values in the assembly must be tagged with an appropriate identifier
; to convince the assembler to tag the cell with the appropriate type.  
;
; return a string containing the asm representation of a number
(define asm-number  (lambda (x) (string-append "n" (if (number? x) (number->string x) x))))
; return a string containing the asm representation of a pointer
(define asm-pointer (lambda (x y) (string-append "p" (if (number? x) (number->string x) x) 
						 "," (if (number? x) (number->string y) y)
						 )))
; return a string containing the asm representation of a language constant
(define asm-lang-const (lambda (x) (string-append "l" (if (number? x) (number->string x) x))))

; Language constants
(define false-value  "FALSE")
(define true-value   "TRUE")
(define string-type-flag (asm-lang-const 2))
(define symbol-type-flag (asm-lang-const 3))

; A few builtin forms are handled specially
; by the compiler.  Some of these will later
; be subsumed by macro capabilities but for right
; now they are just special voodoo.
;
; This list associates the symbols with special
; compiler functions.
(define builtin-forms 
  (lambda ()
    (list
     (cons "lambda" compile-lambda)
     (cons "let"    compile-let)
     (cons "if"     compile-if)
     (cons "define" compile-define)  
     (cons "begin"  compile-begin)
     (cons "quote"  compile-quote)
     )
    ))

; search the list of builtin forms for a 
; particular form
(define find-builtin-helper
  (lambda (f ss)
    (if (null? ss) #f
	(if (and (string? f) 
		 (string=? f (car (car ss))))
	    (cdr (car ss))
	    (find-builtin-helper f (cdr ss))))))
    
(define find-builtin 
  (lambda (f) (find-builtin-helper f (builtin-forms))))    

; The top-level-env is a list containing the list of symbols 
; defined by the compiler at the top level. It is very important
; that the order of symbols in this list match the order defined 
; in the assembly-preamble function way down below.  
;
; The runtime environment is represented as a list of lists
; references to symbols are replaced with a traversal of this structure
; based on the level of enclosing scope where the symbol was defined, and
; the index of the variable in that scope.  This is taken directly from the
; SECD machine's representation of the environment. 
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
;   ((7)
;    (5)
;    ("=" "null?" "cons" "car" "cdr" "+" ...))
;
; So the reference to symbol 'z' will be compiled to (car (car env))
;    the reference to symbol 'x' will be compiled to (car (car (cdr env))) and
;    the reference to symbol '+' will be compiled to (car (cdr (cdr (cdr (cdr (car (cdr (cdr env))))))))
;
; Note: this isn't strictly accurate since the symbols 'car' and 'cdr' are themselves
;       defined in the environment and would thus require lookups making this expansion
;       impossible.  What really happens is that a non-closure form of the car and cdr 
;       procedures are invoked directly.  See the functions u-call-* below.
(define top-level-env (quote (("=" "<" "null?" "cons" "car" "cdr" "+" "-" "*" "%" "/" 
			       "string->list"
			       "print-char" "print-num" "string-length" 
			       "string?" "number?" "char?" "pair?"
			       "read-char" "list" "quit"
			       "set-car!" "set-cdr!"
			       ; these aren't defined yet!
			       "and" "or"                                            ; boolean operators 
			       "string=?" "char=?" "char<=?" "char>=?" "eof-object?" ; comparison functions 
			       "list->string"                                        ; list functions			       
			       "string-append" "substring" "make-string"             ; string functions 
			       "set!"                                                ; modifiers
			       ))))

; The compiler's internal notion of what instruction is currently being written
(define instruction-pointer 0)
(define increment-ip (lambda ()
  (set! instruction-pointer (+ 1 instruction-pointer))))

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
      (display "\n")
      (increment-ip)
    )
))
  
; append a list of instructions to the output stream
(define append-instructions 
  (lambda (inss)
    (if (null? inss) (quote ())
	(begin
	  (append-instruction (car inss))
	  (append-instructions (cdr inss))))))


; append assembly code for storing a value to the next free
; heap cell.
; (val)  -> (hp) , hp' = hp+1, heap[hp] = val
(define store-to-heap (lambda ()
			(append-instructions
			 (list "PUSH" (asm-number 1)   ; (thing 1)
			       "ALOC"                  ; (thing hp)
			       "DUP"                   ; (thing hp hp)
			       "ROT"                   ; (hp thing hp)
			       "PUSH" (asm-number 0)   ; (hp thing hp 0)
			       "STOR"))))

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
(define assembly-car  (lambda () (append-instructions (list "PUSH" (asm-number 0) "LOAD"))))

; (ptr) -> ((cdr ptr))
(define assembly-cdr  (lambda () (append-instructions (list "PUSH" (asm-number 1) "LOAD"))))


; (cdr car) -> ((cons car cdr))
(define assembly-cons (lambda ()
			(append-instructions 
			 (list "PUSH" (asm-number 2) ; (cdr car 2)
			       "ALOC"                ; (cdr car &hp)
			       "DUP"                 ; (cdr car hp hp)
			       "ROT"                 ; (cdr hp car hp)
			       "PUSH" (asm-number 0) ; (cdr hp car hp 0)
			       "STOR"                ; (cdr hp) car stored
			       "DUP"                 ; (cdr hp hp)
			       "ROT"                 ; (hp cdr hp)
			       "PUSH" (asm-number 1) ; (hp cdr hp 1)
			       "STOR"))              ; (hp)  cdr stored
			))


; top is the cons box to set, then the new value
(define assembly-set-car 
  (lambda () (append-instructions (list "PUSH" (asm-number 0) "STOR"))))
(define assembly-set-cdr 
  (lambda () (append-instructions (list "PUSH" (asm-number 1) "STOR"))))

; these define how to call the three primitives car, cdr, and cons as
; part of larger compiler generated sequences (e.g., function application)
; for car, we just inline the assembly.  For cdr and cons we do a machine level
; call into a function.
(define u-call-car  (lambda () (assembly-car))) ; car is 3 instructions, a function call is the same length
					        ; so there is no reason not to inline it.

(define u-call-cdr  (lambda () (assembly-cdr))) ; same with cdr.

(define u-call-cons ; cons is really big (13 instructions)! we'll never inline it
  (lambda () (append-instructions (list "PUSH" "@__u_cons" "CALL"))))

(define u-call-set-car (lambda () (assembly-set-car)))
(define u-call-set-cdr (lambda () (assembly-set-cdr)))

; function application convention
; top of stack is the closure to apply, then the arguments
; this is tricky.  We need to cons the argument list onto 
; the closure's environment,  store the existing
; environment pointer to the stack, set the environment 
; pointer to the new list, invoke the closure's code,
; then restore the environment pointer on return.
;
; (args clos) -> ((clos args)) 
(define assembly-funcall (lambda ()
			   (append-instruction "DUP")      ; (args clos clos)
			    (u-call-car)                   ; (args clos env)
			    (append-instructions 
			     (list "SWAP"                  ; (args env clos)
				   "ROT"                   ; (clos args env)
				   "SWAP"))                ; (clos env  args)
			    (u-call-cons)	           ; (clos (args . env)*)
			    (append-instructions
			     (list "RDRR"                  ; stack is (clos (args . env) renv)
				   "SWAP"                  ; stack is (clos renv (args . env) )
				   "WTRR"                  ; stack is (clos renv) rr = (args . env)
				   "SWAP"))                ; stack is (renv clos) 
			    (u-call-cdr)                   ; stack is (renv clos-code)
			    (append-instructions
			     (list "CALL"                  ; make the call.  we'll have (renv rval)
				   "SWAP"                  ; stack is (rval renv)
				   "WTRR"                  ; stack is (rval) rr = renv
				   ))))

; tail calls are sneakier we avoid saving the current
; env pointer. 
; (args clos) -> ((clos args))
(define assembly-tailcall (lambda ()
			    (append-instruction "DUP")  ; (renv rp args clos clos)
			    (u-call-car)                ; (renv rp args clos env)			    
			    (append-instructions
			     (list "SWAP"               ; (renv rp args env clos)
				   "ROT"                ; (renv rp clos args env)
				   "SWAP"))             ; (renv rp clos env args)
			    (u-call-cons)               ; (renv rp clos (args . env)* )
			    (append-instruction "WTRR") ; (renv rp clos) rr = (args . env)
					                ; note that we didn't store the current env
					                ; this is a tail call so we'll return straight
			                                ; to the current renv/rp!
			    (u-call-cdr)                ; (renv rp code)
			    (append-instruction "JMP")  ; we jump into the call with 
                                                        ;   (renv rp) 
			                                ; on return we'll have pc = rp, and
			                                ;   (renv rval) on the stack
				                   	; just as on return from non-tail call above.
			    ))

; returning is simple since cleanup is handled by the caller
(define assembly-funret (lambda () (append-instruction "RET")))
			   
; Assembly for loading a cell from the environment.
; assembly-env-cell places the cons box whose car is 
; at the desired offsets on the stack.  
; assembly-env-val actually loads the value.  
; The distinction is made for the purpose of set!
(define assembly-env-cell
  (lambda (depth idx)
    (append-instructions 
     (list "RDRR"                     ; (env)
	   "PUSH" (asm-number depth)  ; (env d)
	   "PUSH" "@__u_nth_cell"     ; (env d u_nth)
	   "CALL"))
    (u-call-car)                      ; ((nth env depth))
    (append-instructions
     (list "PUSH" (asm-number idx)    ; ((nth env depth) idx)
	   "PUSH" "@__u_nth_cell"     ; ((nth env depth) idx u_nth)
	   "CALL"                     ; ((nth (nth env depth) idx))
    ))))

(define assembly-env-val 
  (lambda (depth idx)
    (assembly-env-cell depth idx)
    (u-call-car)))

(define assembly-nil      
  (lambda ()
    (append-instructions (list "PUSH" "@__nil" ))))

; Lookup functions,  find a particular symbol in the symbolic environment
; list. These are complimentary to the assembly-env-* functions above.
(define lookup-reference-offset 
  (lambda (r e cont)
    (if (null? e) (cont #f)	
	(if (string=? r (car e))
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
  (lambda (c env) (append-instructions (list "PUSH" (asm-number c))) #f))

(define compile-string 
 (lambda (s env) 
   (let ((strlabel (fresh-label))
	 (strlen (calculate-string-length s)))
     (append-instructions (list "PUSH" (string-append "@" strlabel)))
     (lambda ()         
       (append-instructions 
	(list (string-append ":" (string-append strlabel (string-append "," (number->string (+ strlen 2)))))
	      string-type-flag
	      (asm-number strlen)
	      s))
       #f))))

(define calculate-symbol-length 
  (lambda (s) 0))

(define compile-symbol
  (lambda (s env)
    (let ((symlabel (fresh-label))
	  (symlen (calculate-symbol-length s)))
      (append-instructions (list "PUSH" (string-append "@" symlabel)))
      (lambda ()
	(append-instructions 
	 (list (string-append ":" (string-append symlabel (string-append "," (number->string (+ symlen 2)))))
	       symbol-type-flag
	       (asm-number symlen)
	       (string-append "\"" (string-append s "\""))))
	#f))))

; this doesn't handle escaped chars except newline.
(define compile-char
  (lambda (s env)
    (append-instructions 
     (list "PUSH" 	   
	   (string-append "'" 
			  (string-append
			   (if (string=? s "#\\newline")
			       "\\n" 
			       (if (string=? s "#\\space")
				   " "
				   (substring s 2 3))) "'")))) #f))

(define compile-reference 
  (lambda (r env)
    (let ((i (lookup-reference r env)))
      (if i 
	  (assembly-env-val (car i) (cdr i))
	  ; this is an error
	  (begin
	    ; this should really write to stderr.
	    (display (string-append "Undefined symbol: " r))
	    (quit))
	  )
      #f
      )))

(define compile-atom 
  (lambda (x env quoted) 
    (if (string=? x "#t")
	(begin (append-instructions (list "PUSH" true-value)) #f)
	(if (string=? x "#f")
	    (begin (append-instructions (list "PUSH" false-value)) #f)
	    (if (is-numeric? x)
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


; Hm, we should probably be flagging code pointers with something
; so that we can avoid gc'ing them. Right now the VM just assumes the 
; code is statically defined below initial heap pointer but in order
; to support eval we'll have to do something more clever later.
(define compile-lambda 
  (lambda (l env rest)
    (let ((label (fresh-label)))
      (append-instructions
       (list "PUSH" (string-append "@" label) "RDRR"))
      (u-call-cons)
      (lambda ()	
	(append-instruction (string-append ":" label))
	(let ((r (compile-sequence (cdr (cdr l)) (cons (car (cdr l)) env) #f) ))
	  (assembly-funret) 
	  r)	
	)
      )))

(define compile-let-bindings
  (lambda (bs env)
    (if (null? bs) 
	(begin 
	  (assembly-nil)
	  #f)
	(let ((r1 (compile-let-bindings (cdr bs) env))
	      (r2 (compile-sexp (car (cdr (car bs))) env #t)))
	  (u-call-cons)
	  (lambda ()
	    (do-compile-task r1)
	    (do-compile-task r2))))))

(define compile-let
  (lambda (l env rest)
    (let ((lbl (fresh-label))
	  (r1 (compile-let-bindings (car (cdr l)) env))
	  (e (map (lambda (x) (car x)) (car (cdr l)))))
      (append-instructions (list "PUSH" (string-append "@" lbl) "RDRR"))
      (u-call-cons)
      (if rest
	  (assembly-funcall)
	  (assembly-tailcall))
      (lambda ()
	(do-compile-task r1)
	(do-compile-task 
	 (lambda ()
	   (append-instruction (string-append ":" lbl))
	   (let ((r (compile-sequence (cdr (cdr l)) (cons e env) rest)))
	     (assembly-funret)
	     r)))
	))))

(define compile-begin
  (lambda (l env rest) (compile-sequence (cdr l) env rest)))

(define compile-sequence 
  (lambda (l env rest)
    (if (null? l) #f
	(let ((r1 (compile-sexp (car l) env (if (null? (cdr l)) rest #t) )))
	  (if (not (null? (cdr l)))
	      (append-instruction "POP") 
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
    (let ((v (lookup-reference (car (cdr l)) env))
	  (r (compile-sexp (car (cdr (cdr l))) env #t)))
      (if v 
	  (begin
	    (assembly-env-cell (car v) (cdr v))
	    (u-call-set-car))
	  (begin
	    (set-car! top-level-env (cons (car (cdr l)) (car top-level-env)))
	    (append-instruction "RDRR")                  ; (v env)
	    (u-call-car)                                 ; (v (car env))
	    (append-instruction "SWAP")                  ; ((car env) v)
	    (u-call-cons)                                ; ((v . (car env)))
	    (append-instruction "RDRR")                  ; ((v . (car env)) env)
	    (u-call-set-car)))                           ; () ; env is updated
	  r)))

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
		     (list "PUSH" false-value
			   "EQ"
			   "PUSH" (string-append "@" false-label)
			   "JTRUE")))
		(r2 (compile-sexp true-case env rest))
		(y  (append-instructions (list "PUSH" (string-append "@" join-label) "JMP"
					       (string-append ":" false-label))))
		(r3 (compile-sexp false-case env rest)))
	    (append-instruction (string-append ":" join-label))
	    (lambda ()
	      (do-compile-task r1)
	      (do-compile-task r2)
	      (do-compile-task r3)
	      )
	    )
	  ))))

(define compile-quoted-sexp
  (lambda (s env)
    (if (pair? s)
	(let ((r1 (compile-quoted-sexp (cdr s) env))
	      (r2 (compile-quoted-sexp (car s) env)))
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
    (compile-quoted-sexp (car (cdr s)) env)))

(define compile-arguments
  (lambda (l env)
    (if (null? l) 
	(begin (assembly-nil) #f)
	(let ((r1 (compile-arguments (cdr l) env))
	      (r2 (compile-sexp (car l) env #t)))
	  (u-call-cons)
	  (lambda () 
	    (do-compile-task r1)
	    (do-compile-task r2)
	    )))))

(define compile-list
  (lambda (l env rest)
    (let ((s (find-builtin (car l))))
      (if s 
	  (s l env rest)
	  (let ((r1 (compile-arguments (cdr l) env))
		(r2 (compile-sexp (car l) env #t)))
	    (if rest
		(assembly-funcall)
		(assembly-tailcall)
		)
	    (lambda ()
	      (do-compile-task r1)
	      (do-compile-task r2)))))))
 
(define compile-sexp 
  (lambda (s env rest) 
    (if (list? s) 
	(compile-list s env rest) 
	(compile-atom s env #f))))


; The builtin functions are a little peculiar.
; The convention is 
;     :foo is the closure cell for the foo function,
;     :__foo is the entry point when called as a closure
;            (i.e., arguments are retrieved from the env pointer)
;     :__u_foo is the entry point for internal invocations
;            (e.g., during closure invocation). arguments are read
;            from below the rp on the stack
;     :__foo_body is the actual implementation of foo,  arguments
;            are read from the top of stack
;
(define define-builtin-functions
  (lambda ()
    (append-instructions
     (list ":__u_nth_cell"              ; (l n r)
	   "ROT"                        ; (r l n)
	   ":__u_nth_cell_loop"
	   "DUP"                        ; (r l n n)
	   "PUSH" (asm-number 0)        ; (r l n n 0)
	   "EQ"                         ; (r l n (= n 0))
	   "PUSH" "@__u_nth_cell_done"  ; (r l n (= n 0) u_nth_cell_done)
	   "JTRUE"                      ; (r l n)
	   "PUSH" (asm-number -1)       ; (r l n -1)
	   "ADD"                        ; (r l (- n 1))
	   "SWAP"))                     ; (r (- n 1) l)
    (u-call-cdr)                        ; (r (- n 1) (cdr l))
    (append-instructions 
     (list "SWAP"                       ; (r (cdr l) (- n 1))
	   "PUSH" "@__u_nth_cell_loop"  ; (r (cdr l) (- n 1) u_nth_cell_loop)
	   "JMP"
	   ":__u_nth_cell_done"         ; (r l 0)
	   "POP"))                      ; (r l)
    (append-instruction "RET")
    

    (append-instructions     
     (list ":cons,2" "@__initial_env" "@__cons" ; this is the closure cell
	   ":__u_cons"                  ; this is the internal entry point
	                                ; stack is (cdr car rp)
	   "ROT"))                      ; stack is (rp  cdr car)
    (assembly-cons)                     ; this is the internal cons used when building arg lists
    (assembly-funret)                   
    (append-instruction ":__cons")      ; this is the exposed cons, it plays a little trick
					; with the environment list by loading the second arg,
					; grabbing the cons box containing the first arg,
    (assembly-env-val 0 1)              ; then overwriting the cdr pointer and returning.
					; this is safe since the cons boxes surrounding the 
					; arguments are allocated in assembly-funcall
    (assembly-env-cell 0 0)             ; so it is guaranteed to not be shared.
    (append-instructions                ; stack is (cdr &car)
     (list "DUP"                        ; (cdr &car &car)                       
	   "ROT"                        ; (&car cdr &car)
	   "PUSH" (asm-number 1)        ; (&car cdr &car 1)
	   "STOR"))                     ; (&car)
    (assembly-funret)					

    (append-instructions
     (list ":car,2" "@__initial_env" "@__car"   ; closure cell	   
	   ":__u_car"                   ; internal entry point
	                                ; (arg rp)
	   "SWAP"                       ; (rp arg)
	   "PUSH" "@__car_body"         ; (rp arg __car_body)
	   "JMP"                        ; jump to the body
	   ":__car"))                   ; closure entry point (arg is in env)
    (assembly-env-val 0 0)              ; stack is (renv rp arg)
    (append-instruction ":__car_body")  ; now we're at the body
    (assembly-car)                      ; dump the assembly for car
    (assembly-funret)                   ; and a return statement

    (append-instructions                ; this is completely analogous to above
     (list ":cdr,2" "@__initial_env" "@__cdr" 
	   ":__cdr"))
    (assembly-env-val 0 0)
    (append-instruction ":__cdr_body")
    (assembly-cdr)
    (assembly-funret)

    ; arithmetic ops
    ; these aren't used internally so we just define them.
    (append-instructions
     (list ":add,2" "@__initial_env" "@__add :__add"))
    (assembly-env-val 0 0)
    (assembly-env-val 0 1)
    (append-instruction "ADD")
    (assembly-funret)

    (append-instructions
     (list ":subtract,2" "@__initial_env" "@__subtract :__subtract"))
    (assembly-env-val 0 0)
    (assembly-env-val 0 1)    
    (append-instruction "SUB")
    (assembly-funret)

    (append-instructions
     (list ":multiply,2" "@__initial_env" "@__multiply" ":__multiply"))
    (assembly-env-val 0 0)
    (assembly-env-val 0 1)
    (append-instruction "MUL")
    (assembly-funret)

    (append-instructions 
     (list ":divide,2" "@__initial_env" "@__divide" ":__divide"))
    (assembly-env-val 0 0)
    (assembly-env-val 0 1)
    (append-instruction "DIV")
    (assembly-funret)

    (append-instructions 
     (list ":modulo,2" "@__initial_env" "@__modulo" ":__modulo"))
    (assembly-env-val 0 0)
    (assembly-env-val 0 1)
    (append-instruction "MOD")
    (assembly-funret)

    ; equality comparison
    (append-instructions
     (list ":equal,2" "@__initial_env" "@__equal" ":__equal"))
    (assembly-env-val 0 0)
    (assembly-env-val 0 1)
    (append-instruction "EQ")
    (assembly-funret)

    ; less than comparison
    (append-instructions
     (list ":less_than,2" "@__initial_env" "@__less_than" ":__less_than"))
    (assembly-env-val 0 0)
    (assembly-env-val 0 1)
    (append-instruction "LT")
    (assembly-funret)
    
    ; assignments
    (append-instructions
     (list ":set_car,2" "@__initial_env" "@__set_car" ":__set_car"))
    (assembly-env-val 0 1)
    (assembly-env-val 0 0)
    (u-call-set-car)
    (assembly-funret)
    
    (append-instructions
     (list ":set_cdr,2" "@__initial_env" "@__set_cdr" ":__set_cdr"))
    (assembly-env-val 0 1)
    (assembly-env-val 0 0)
    (u-call-set-cdr)
    (assembly-funret)


    ; questions
    (append-instructions
     (list ":null_q,2" "@__initial_env" "@__null_q" ":__null_q"))
    (assembly-env-val 0 0)
    (assembly-nil)
    (append-instruction "EQ")
    (assembly-funret)

    (append-instructions
     (list ":char_q,2" "@__initial_env" "@__char_q" ":__char_q"))
    (assembly-env-val 0 0)
    (append-instruction "ISCHR")
    (assembly-funret)

    (append-instructions
     (list ":number_q,2" "@__initial_env" "@__number_q" ":__number_q"))
    (assembly-env-val 0 0)
    (append-instruction "ISNUM")
    (assembly-funret)

    (append-instructions 
     (list ":string_q,2" "@__initial_env" "@__string_q" ":__string_q"))
    (assembly-env-val 0 0)
    (append-instructions 
     (list "DUP" 
	   "ISPTR"
	   "PUSH" "@__string_q_is_ptr"
	   "JTRUE" 
	   "POP"
	   "PUSH" "FALSE"))
    (assembly-funret)
    (append-instructions
     (list ":__string_q_is_ptr"
	   "PUSH" (asm-number 0)
	   "LOAD"
	   "PUSH" string-type-flag
	   "EQ"))
    (assembly-funret)

    (append-instructions
     (list ":print_num,2" "@__initial_env" "@__print_num" ":__print_num"))
    (assembly-env-val 0 0)
    (append-instructions (list "DUP" "PINT"))
    (assembly-funret)

    (append-instructions
     (list ":print_char,2" "@__initial_env" "@__print_char" ":__print_char"))
    (assembly-env-val 0 0)
    (append-instructions (list "DUP" "PCHR"))
    (assembly-funret)

    (append-instructions
     (list ":read_char,2" "@__initial_env" "@__read_char" ":__read_char"))
    (append-instruction "GETC")
    (assembly-funret)

    (append-instructions
     (list ":quit,2" "@__initial_env" "@__quit" ":__quit"))
    (append-instruction "END")

    (append-instructions
     (list ":pair_q,2" "@__initial_env" "@__pair" ":__pair"))
    (assembly-env-val 0 0)
    (append-instructions 
     (list "DUP"
	   "ISPTR"
	   "PUSH" "@__pair_q_isptr" 
	   "JTRUE"
	   "POP"
	   "PUSH" false-value))
    (assembly-funret)
    ; this isn't the greatest heuristic,
    ; but at this point if the target of a 
    ; pointer is not a language constant,
    ; then the pointer points to a cons box.
    (append-instructions
     (list ":__pair_q_isptr"
	   "PUSH" (asm-number 0)
	   "LOAD"
	   "ISLCONST"
	   "PUSH" "@__pair_q_islconst"
	   "PUSH" true-value))
    (assembly-funret)
    (append-instructions 
     (list ":__pair_q_islconst" "PUSH" false-value))
    (assembly-funret)
	   
    ; list is sneaky and takes advantage of the fact
    ; that the arguments are passed in as a list.
    (append-instructions
     (list ":list,2" "@__initial_env" "@__list" ":__list"))
    (assembly-env-cell 0 0)
    (assembly-funret)

    ; getting the length of a string is easy
    (append-instructions
     (list ":string_length,2" "@__initial_env" "@__string_length" ":__string_length"))
    (assembly-env-val 0 0)
    (append-instructions (list "PUSH" (asm-number 1) "LOAD"))
    (assembly-funret)

    ; converting a string to a list is a PIA. 
    (append-instructions 
     (list ":string_list,2" "@__initial_env" "@__string_list" ":__string_list"))
    (assembly-env-val 0 0)
    (append-instructions
     (list "DUP"                        ; (str-ptr str-ptr)
	   "PUSH" (asm-number 1)        ; (str-ptr str-ptr 0)
	   "LOAD"                       ; (str-ptr str-len)
	   "PUSH" (asm-number 1)        ; (str-ptr str-len 1)
	   "ADD"))                      ; (str-ptr (+ str-len 1))
    (assembly-nil)                      ; (str-ptr str-len nil)
    (u-call-cons)                       ; (str-ptr (nil . (+ str-len 1) ) )
					; Note: On each iteration through the loop 
					;       we want to grab the last element of the 
					;       string that hasn't been copied.
					;       the layout of the string is:
					;       [type-flag, length, data....]
					;       So the last character of the string
					;       is at address (+ str-ptr length 1)
					;       and in general, the last uncopied char
					;       is at (+ str-ptr len-left 1)
    (append-instructions
     (list ":__string_list_loop"        ; loop header
					; (str-ptr (rval . (+ len-left 1) ))
	   "DUP"))                      ; (str-ptr (rval . (+ len-left 1) ) (rval . (+ len-left 1)) )
    (u-call-cdr)                        ; (str-ptr (rval . (+ len-left 1)) (+ len-left 1))
    (append-instructions
     (list "PUSH" (asm-number 1)        ; (str-ptr (rval . (+ len-left 1)) (+ len-left 1) 1)
	   "EQ"
	   "PUSH" "@__string_list_done" ; (str-ptr (rval . (+ len-left 1)) (+ len-left 1) 1 @__string_list_done)
	   "JTRUE"                      ; (str-ptr (rval . (+ len-left 1)))
	   "DUP"))                      ; (str-ptr (rval . (+ len-left 1)) (rval . (+ len-left 1)))
    (u-call-cdr)                        ; (str-ptr (rval . (+ len-left 1)) (+ len-left 1))
					; this could be hoisted to a DUP above the loop test
    (append-instructions 
     (list "SWAP"                       ; (str-ptr (+ len-left 1) (rval . (+ len-left 1) ) )
	   "ROT"                        ; ((rval . (+ len-left 1)) str-ptr (+ len-left 1))
	   "SWAP"                       ; ((rval . (+ len-left 1)) (+ len-left 1) str-ptr)
	   "DUP"                        ; ((rval . (+ len-left 1)) (+ len-left 1) str-ptr str-ptr)
	   "ROT"                        ; ((rval . (+ len-left 1)) str-ptr (+ len-left 1) str-ptr)
	   "SWAP"                       ; ((rval . (+ len-left 1)) str-ptr str-ptr (+ len-left 1))
	   "LOAD"                       ; ((rval . (+ len-left 1))) str-ptr str[(+ len-left 1)])
	   "SWAP"                       ; ((rval . (+ len-left 1)) str[(+ len-left 1)] str-ptr)
	   "ROT"                        ; (str-ptr (rval . (+ len-left 1)) str[(+ len-left 1)])
	   "SWAP"                       ; (str-ptr str[(+ len-left 1)] (rval . (+ len-left 1)))
	   "DUP"                        ; (str-ptr str[(+ len-left 1)] (rval . (+ len-left 1)) (rval . (+ len-left 1)))
	   "ROT"))                      ; (str-ptr (rval . (+ len-left 1)) str[(+ len-left 1)] (rval . (+ len-left 1)))
    (u-call-car)                        ; (str-ptr (rval . (+ len-left 1)) str[(+ len-left 1)] rval)
    (append-instruction "SWAP")         ; (str-ptr (rval . (+ len-left 1)) rval str[(+ len-left 1)])
    (u-call-cons)                       ; (str-ptr (rval . (+ len-left 1)) (str[(+ len-left 1)] . rval))
    (append-instructions 
     (list "SWAP"                       ; (str-ptr (str[(+ len-left 1)] . rval) (rval . (+ len-left 1)))
	   "DUP"                        ; (str-ptr (str[(+ len-left 1)] . rval) (rval . (+ len-left 1)) (rval . (+ len-left 1)))
	   "ROT"))                      ; (str-ptr (rval . (+ len-left 1)) (str[(+ len-left 1)] . rval) (rval . (+ len-left 1)))
    (u-call-set-car)                    ; (str-ptr (rval . (+ len-left 1))) -- rval is updated
    (append-instructions
     (list "DUP"                        ; (str-ptr (rval . (+ len-left 1)) (rval . (+ len-left 1)))
	   "DUP"))                      ; (str-ptr (rval . (+ len-left 1)) (rval . (+ len-left 1)) (rval . (+ len-left 1)))
    (u-call-cdr)                        ; (str-ptr (rval . (+ len-left 1)) (rval . (+ len-left 1)) (+ len-left 1))
    (append-instructions 
     (list "PUSH" (asm-number -1)       ; (str-ptr (rval . (+ len-left 1)) (rval . (+ len-left 1)) (+ len-left) -1)
	   "ADD"                        ; (str-ptr (rval . (+ len-left 1)) (rval . (+ len-left 1)) len-left)
	   "SWAP"))                     ; (str-ptr (rval . (+ len-left 1)) len-left (rval . (+ len-left 1)) )
    (u-call-set-cdr)                    ; (str-ptr (rval . len-left)) --len-left has been updated
    (append-instructions
     (list "PUSH" "@__string_list_loop" ; (str-ptr (rval . len-left) @__string_list_loop)
	   "JMP"))
    (append-instructions
     (list ":__string_list_done"        ; out of the loop. stack is (str-ptr (rval . 1))
	   "SWAP"                       ; ((rval . 1) str-ptr)
	   "POP"))                      ; ((rval . 1))
    (u-call-car)                        ; (rval)
    (assembly-funret)
    
    )
  )
	   
    
(define assembly-preamble
  (lambda ()
    (append-instructions 
     (list 
      ; initial trampoline past the definition of nil
      "PUSH" "@__main" "JMP"
      ; definition of null/nil
      ":__nil,2"               "@__nil"         "@__nil"
      ; This is the definition of the initial environment.
      ; each row here defines a cons box where the car
      ; is the closure cell for the builtin function, and 
      ; the cdr is the previous row (or nil)
      ":__set_cdr_box"         "@set_cdr"       "@__nil"
      ":__set_car_box"         "@set_car"       "@__set_cdr_box"
      ":__quit_box"            "@quit"          "@__set_car_box"
      ":__list_box"            "@list"          "@__quit_box"
      ":__read_char_box"       "@read_char"     "@__list_box"
      ":__pair_q_box"          "@pair_q"        "@__read_char_box"
      ":__char_q_box,2"        "@char_q"        "@__pair_q_box"
      ":__number_q_box,2"      "@number_q"      "@__char_q_box"
      ":__string_q_box,2"      "@string_q"      "@__number_q_box"
      ":__string_length_box,2" "@string_length" "@__string_q_box"
      ":__print_num_box,2"     "@print_num"     "@__string_length_box"
      ":__print_char_box,2"    "@print_char"    "@__print_num_box"
      ":__string_list_box,2"   "@string_list"   "@__print_char_box"
      ":__divide_box,2"        "@divide"        "@__string_list_box"
      ":__modulo_box,2"        "@modulo"        "@__divide_box"
      ":__multiply_box,2"      "@multiply"      "@__modulo_box"
      ":__subtract_box,2"      "@subtract"      "@__multiply_box"
      ":__add_box,2"           "@add"           "@__subtract_box"
      ":__cdr_box,2"           "@cdr"           "@__add_box"
      ":__car_box,2"           "@car"           "@__cdr_box"
      ":__cons_box,2"          "@cons"          "@__car_box"
      ":__null_q_box,2"        "@null_q"        "@__cons_box"
      ":__less_than_box,2"     "@less_than"     "@__null_q_box"
      ":__equal_box,2"         "@equal"         "@__less_than_box"
      ; actual initial env definition
      ":__initial_env,2"       "@__equal_box"   "@__nil"
      ))
     (define-builtin-functions)
     (append-instructions 
      (list 
       ":__main" ; now initialize the heap pointer to point somewhere
       "PUSH" "@__initial_env"
       "WTRR")) ; set the initial env pointer
     ))

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
	  (append-instruction"END" )))))


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
(define is-space?     (lambda (c) (or (eof-object? c) (char=? c #\space ) (char=? c #\tab) (char=? c #\newline))))
(define is-delimiter? (lambda (c) (or (is-space? c) (or (char=? c #\() (or (char=? c #\)) (or (char=? c #\") (char=? c #\; )))))))
(define is-number?    (lambda (c) (and (char>=? c #\0) (char<=? c #\9))))
(define is-char-name? (lambda (s) (or (string=? s "newline") (or (string=? s "space") (string=? s "tab")))))

(define drop-chars-until (lambda (f) (let ((x (read-char))) (if (f x) x (drop-chars-until f)))))
(define next-non-ws      (lambda ()  (drop-chars-until (lambda (z) (not (is-space? z))))))

(define reader-state-hash-backslash 
  (lambda (c)
    (if (is-delimiter? c)
	(cons (string-append "#\\" (make-string 1 c)) (read-char))
	(let ((r (reader-state-wordchar c)))
	  (cons (string-append "#\\" (car r)) (cdr r))))))

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

(define reader-state-wordchar-helper
  (lambda (c)
    (if (or (eof-object? c) (is-delimiter? c) ) (cons '() c)
	(let ((r (reader-state-wordchar-helper (read-char))))
	  (cons (cons c (car r)) (cdr r))))))

(define reader-state-wordchar
  (lambda (c) (let ((r (reader-state-wordchar-helper c)))
		 (cons (list->string (car r)) (cdr r)))))

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
				(if (char=? c #\;)
				    (reader-state-semicolon (read-char))			     
				    (reader-state-wordchar c)))))))))))

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

(define read-list 
  (lambda ()
    (let ((x (next-token)))
      (if (not x)
	(begin
	  (display "ERRROR: EOF Encountered while reading list\n")
	  (quit))  
	(if (string=? x ")") '() (cons (parse-token x) (read-list)))))))


(define read-sexp
  (lambda () (parse-token (next-token))))

; Dump the preamble
(assembly-preamble)
; then run the compiler.
; (display (read-datum (next-non-ws-skip-comments)))
; (newline)
(compiler-run)
