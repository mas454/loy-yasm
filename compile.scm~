(define (object? code)
  (or (string? code) (number? code)))

(define (program-list-compile code-list asm-list)
  (if (null? code-list)
      (append '((putnil))(reverse asm-list))
      (let ((asm (compile (car code-list) #f)))
	(program-list-compile (cdr code-list) (cons asm asm-list)))))

(define (compile code meth-argp)
  (cond 
   ((object? code) `(putobject ,code))
   ((symbol? code) `(getlocal ',code))
   ((=? code)
    (set! symbol-list (cons (cadr code) symbol-list))
    `(,(compile (caddr code) meth-argp) (setlocal ',(cadr code))))
   ((def? code)
    `(def ',(cdr code)))
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

(define (=? exp)
  (tagged-list? exp '=))

(define (def? exp)
  (tagged-list? exp 'def))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (putobject atom)
  (letrec ((print-obj (lambda (atom)
		       (cond ((string? atom)
			      (dprint "\"")
			      (dprint atom)
			      (dprint "\""))
			     (else
			      (dprint atom))))))		      
    (dprint "putobject ")
    (print-obj atom)
    (dprint "\n")))
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
  (dprint ")\n"))

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
(loy-compile '((= a 10) (= b 20) (puts (+ a (+ b 20)))))
