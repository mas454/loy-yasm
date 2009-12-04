(define (object? code)
  (or (string? code) (number? code)
      (eq? 'nil code)
      (eq? 'true code) (eq? 'false code)))

(define (program-list-compile code-list meth-argp)
  (let ((rcode-list (reverse code-list)))
    (reverse (cons (compile (car rcode-list) meth-argp)
		   (map (lambda (a)
			  (compile a #f)) (cdr rcode-list))))))

(define (if-compile code poped)
  (let ((else_part (gensym)) (end (gensym)))
  `(,(compile (car code) #t) (branchunless ',else_part)
    ,(compile (cadr code) poped) (jump ',end) 
    (_ ',else_part) ,(compile (caddr code) poped)
    (_ ',end))))

(define (cond-compile code poped)
  (let ((next (gensym)) (end (gensym)))
    (append
     (list (compile (caar code) #t)
	   `(branchunless ',next)
	   (program-list-compile (cdar code) poped)
	   `(jump ',end))
     (map (lambda (c) ;((eq? a 'hello) (puts a))
	    (let ((n next))
	      (set! next (gensym))
	      (if (eq? (car c) 'else)
		  `((_ ',n)
		    ,(program-list-compile (cdr c) poped)
		    (jump ',end))
		  `((_ ',n)
		    ,(compile (car c) #t)
		    (branchunless ',next)
		    ,(program-list-compile (cdr c) poped)
		    (jump ',end)))))
	    (cdr code))
     `((_ ',next))
     `((_ ',end)))))
(define (let-compile lis)
  `(lamcall (-> ,(map car (car lis)) ,@(cdr lis))
	    ,@(map cadr (car lis))))

(define true 'true)
(define false 'false)
(define nil 'nil)

(define (compile code poped)
  (cond 
   ((object? code) `(putobject ,code ,poped))
   ((symbol? code) `(get ',code))
   ((=? code)
    (set! symbol-list (cons (cadr code) symbol-list))
    `(,(compile (caddr code) #t) (set ',(cadr code))))
   ((cons? code)
    (list (compile (cadr code) #t)
	  (compile (caddr code) #t)
	 '(newarray 2)))
   ((car? code)
    (list (compile (cadr code) #t)
	  '(putobject 0 #t)
	  '(send '|[]| 1)))
   ((cdr? code)
    (list (compile (cadr code) #t)
	  '(putobject 1 #t)
	  '(send '|[]| 1)))
   ((if? code)
    (if-compile (cdr code) poped))
   ((cond? code)
    (cond-compile (cdr code) poped))
   ((let? code)
    (compile (let-compile (cdr code)) poped))
   ((def? code)
    `(def ',(cdr code)))
   ((->? code)
    `(-> ',(cdr code)))
   ((infix? code)
    (let ((arg-lis `(,(compile (cadr code) #t)
		     ,(compile (caddr code) #t)
		     (send ',(car code) 1))))
      (cons arg-lis (infix-args-compile (cdddr code) (car code)))))
   ((binfix? code)
    `(,(compile (cadr code) #t)
      ,(compile (caddr code) #t)
      (send ',(car code) 1)))
   ((mcall? code) 
    (if poped
	(append `(,(compile (cadr code) #t))
		(args-compile (cdddr code))
		`((send ',(caddr code) ,(length (cdddr code)))))
	(append `(,(compile (cadr code) #t))
		(args-compile (cdddr code))
		`((send ',(caddr code) ,(length (cdddr code))))
		'((pop)))))
   ((ccall? code) 
    (if poped
	(append `((cgetconstant ',(cadr code)))
		(args-compile (cdddr code))
		`((send ',(caddr code) ,(length (cdddr code)))))
	(append `((cgetconstant ',(cadr code)))
		(args-compile (cdddr code))
		`((send ',(caddr code) ,(length (cdddr code))))
		'((pop)))))
   ((run? code)
    (if poped
	(append
	 '((putnil))
	 (args-compile (cdr code))     
	 `((call ',(car code) ,(length (cdr code)))))
	(append
	 (args-compile (cdr code))     
	 `((call ',(car code) ,(length (cdr code)))))))
   (else
    (error code))))

(define (cond? code)
  (tagged-list? code 'cond))

(define (car? code)
  (tagged-list? code 'car))

(define (cdr? code)
  (tagged-list? code 'cdr))

(define (cons? code)
  (tagged-list? code 'cons))

(define (ccall? code)
  (tagged-list? code 'ccall))

(define (mcall? code)
  (tagged-list? code 'mcall))

(define (->? code)
  (tagged-list? code '->))

(define (run? code)
  (symbol? (car code)))

(define (infix-args-compile code inf)
  (map (lambda (co)
	 `(,(compile co #t) (send ',inf 1))) code))
	   
 

(define (args-compile code-list)
  (map (lambda (code)
	 (compile code #t)) code-list))
       



(define (dprint . lis)
  (map (lambda (code)
	 (display code out-p)) lis))
	 

(define (atom? a)
  (not (pair? a)))

(define (infix? exp)
  (memq (car exp) '(+ - * / % **  ^ &)))

(define (binfix? exp)
  (memq (car exp) '(< > <= ==)))

(define (=? exp)
  (tagged-list? exp '=))

(define (def? exp)
  (tagged-list? exp 'def))

(define (let? exp)
  (tagged-list? exp 'let))

(define (if? exp)
  (tagged-list? exp 'if))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (send memth argc)
  (dprint "send :" memth ", " argc "\n"))

(define (putobject atom flag)
  (letrec ((print-obj (lambda (atom)
		       (cond ((string? atom)
			      (dprint "\"")
			      (dprint atom)
			      (dprint "\""))
			     (else
			      (dprint atom))))))		      
    (dprint "putobject ")
    (print-obj atom)
    (if (not flag)
	(dprint "\npop"))
    (dprint "\n")))

(define (newarray code)
  (dprint "newarray " code "\n"))

(define (cgetconstant sym)
  (if (symbol? sym)
      (dprint "_ :lstart\n" 
	      "getinlinecache 0, :lend\n"
	      "getconstant :" sym "\n"
	      "setinlinecache :lstart\n"
	      "_ :lend\n")
      (begin 
	(dprint "_ :lstart\n" "getinlinecache 0, :lend\n")
	(map (lambda (s)
	       (dprint "getconstant :" s "\n")) sym)
	(dprint "setinlinecache :lstart\n"
		"_ :lend\n"))))
	
      
(define (_ label)
  (dprint "_ :" label "\n")) 

(define (pop)
  (dprint "pop\n"))
(define (jump label)
  (dprint "jump :" label "\n"))

(define (branchunless label)
  (dprint "branchunless :" label "\n"))

(define (branchif label)
  (dprint "branchif :" label "\n"))

(define (get sym)
  (if blo
      (dprint "getdynamic :" sym "\n")
      (dprint "getlocal :" sym "\n")))

(define (set sym)
  (if blo
      (dprint "setdynamic :" sym "\n")
      (dprint "setlocal :" sym "\n")))

(define (call fn len)
  (dprint "call :" fn ", " len "\n"))

(define (putnil)
  (dprint "putnil\n"))

(define (asm-print asm)
 (dprint asm "\n"))

(define (def code)
  (dprint "definemethod(:" (car code) ",") 
  (method-compile-print code)
  (dprint ")\n" "pop\n"))

(define (-> code)
  (dprint "putnil\n" "call :lambda, 0, ")
  (lam-compile-print code)
  (dprint "\n"))

(define (lam-compile-print code)
  (let ((temp1 symbol-list) (temp2 blo))
    (set! blo #t)
    (set! symbol-list (car code))
    (let ((asm-list (append (program-list-compile (cdr code) #t))))
      
      (dprint "block([")
      (symbol-list-print symbol-list)
      (dprint "]){\n")
      (compile-print asm-list)
      (dprint "leave\n" "}"))
    (set! symbol-list temp1)
    (set! blo temp2)))
  

(define (method-compile-print code)
  (define temp symbol-list)
  (set! symbol-list (cadr code))
  (let ((asm-list (append (program-list-compile (cddr code) #t))))
    (dprint "YASM.method(:" (car code) ", [")
    (symbol-list-print symbol-list)
    (dprint "]){\n")
    (compile-print asm-list)
    (dprint "leave\n" "}"))
  (set! symbol-list temp))

(define symbol-list '())
(define blo #f)
(define (loy-compile code-list)
    (let ((asm-list (append '((putnil))
			    (program-list-compile code-list  #f))))
      (dprint "require \'yasm\'\n")
      (dprint "require \'loy\'\n")
      (dprint "iseq = YASM.toplevel([")
      (symbol-list-print symbol-list)
      (dprint "]){\n")
      (compile-print asm-list)
      (dprint "leave\n" "}\n #puts iseq.disasm\n iseq.eval\n")))
  
(define (symbol-list-print sym-lis)
  (if (not (null? sym-lis))
      (if (not (null? (cdr sym-lis)))
	  (begin
	    (dprint ":" (car sym-lis) ",")
	    (symbol-list-print (cdr sym-lis)))
	  (dprint ":" (car sym-lis)))))
      
(define (compile-print asm-list)
  (if (not (null? asm-list))
      (if (symbol? (car asm-list))
	  (eval asm-list (interaction-environment))
	  (begin
	      (compile-print (car asm-list))
	      (compile-print (cdr asm-list))))))
(define out-p (open-output-file "c-test.rb"))


;(loy-compile '((if false
	;	   (puts 10)
		;   (if true
		 ;      (puts "hello")
		  ;     (puts "world")))))
(define let-test '(
		   (let ((a 10) (b 20))
			 (puts (+ a b)))
		   )
  )
  
(define list-test '(
		    (def cons (a b)
			 (-> (f)
			   (lamcall f  a b)))
		    (def car (f)
			 (lamcall f  (-> (a b)
					   a)))
		    (def cdr (f)
			 (if (== f nil)
			     nil
			     (lamcall f  (-> (a b)
					     b))))
		    (= x (cons "a" (cons "b" nil)))
		    (puts (cdr (cdr x)))))
		    
			 
(define cond-test '((cond 
		     (false
		      (puts "hello"))
		     (else
		      (= x 10)
		      (puts x)))))
(display (let-compile (cdar let-test)))
(newline)
;(loy-compile let-test)


