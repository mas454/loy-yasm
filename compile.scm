(define (object? code)
  (or (string? code) (number? code) 
      (eq? 'true code) (eq? 'false code)))

(define (program-list-compile code-list asm-list)
  (if (null? code-list)
      (append '((putnil))(reverse asm-list))
      (let ((asm (compile (car code-list) #f)))
	(program-list-compile (cdr code-list) (cons asm asm-list)))))

(define (if-compile code meth-argp)
  `(,(compile (car code) #t) (branchunless 'else_part)
    ,(compile (cadr code) meth-argp) (jump 'end) 
    (_ 'else_part) ,(compile (caddr code) meth-argp)
    (_ 'end)))

(define true 'true)
(define false 'false)

(define (compile code meth-argp)
  (cond 
   ((object? code) `(putobject ,code ,meth-argp))
   ((symbol? code) `(getlocal ',code))
   ((=? code)
    (set! symbol-list (cons (cadr code) symbol-list))
    `(,(compile (caddr code) meth-argp) (setlocal ',(cadr code))))
   ((if? code)
    (if-compile (cdr code) meth-argp))
   ((def? code)
    `(def ',(cdr code)))
   ((infix? code)
    `(,(compile (cadr code) #t)
      ,(compile (caddr code) #t)
      (send ',(car code) 1)))
   ((run? code)
    (if meth-argp
	(append
	 '((putnil))
	 (args-compile (cdr code) '())     
	 `((call ',(car code) ,(length (cdr code)))))
	(append
	 (args-compile (cdr code) '())     
	 `((call ',(car code) ,(length (cdr code)))))))
   (else
    (error code))))

(define (run? code)
  (symbol? (car code)))

(define (args-compile code arg-list)
  (if (null? code)
      (reverse arg-list)
      (args-compile (cdr code)
		    (cons (compile (car code) #t) arg-list))))


(define (dprint . lis)
  (letrec ((print-list (lambda (lis)
			 (if (not (null? lis))
			     (begin
			       (display (car lis) out-p)
			       (print-list (cdr lis)))))))
    (print-list lis)))

(define (atom? a)
  (not (pair? a)))

(define (infix? exp)
  (assoc (car exp) '((+ #t) (- #t))))

(define (=? exp)
  (tagged-list? exp '=))

(define (def? exp)
  (tagged-list? exp 'def))

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

(define (_ label)
  (dprint "_ :" label "\n")) 

(define (jump label)
  (dprint "jump :" label "\n"))

(define (branchunless label)
  (dprint "branchunless :" label "\n"))

(define (branchif label)
  (dprint "branchif :" label "\n"))

(define (getlocal sym)
  (dprint "getlocal :" sym "\n"))

(define (setlocal sym)
  (dprint "setlocal :" sym "\n"))

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

(define (method-compile-print code)
  (define temp symbol-list)
  (set! symbol-list (cadr code))
  (let ((asm-list (program-list-compile (cddr code) '())))
    (dprint "YASM.method(:" (car code) ", [")
    (symbol-list-print symbol-list)
    (dprint "]){\n")
    (compile-print asm-list)
    (dprint "leave\n" "}"))
  (set! symbol-list temp))

(define symbol-list '())
(define (loy-compile code-list)
    (let ((asm-list (program-list-compile code-list '())))
      (dprint "require \'yasm\'\n")
      (dprint "require \'lispu\'\n")
      (dprint "iseq = YASM.toplevel([")
      (symbol-list-print symbol-list)
      (dprint "]){\n")
      (compile-print asm-list)
      (dprint "leave\n" "}\n iseq.eval\n")))
  
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

(loy-compile '((puts (+ 10 20))))
