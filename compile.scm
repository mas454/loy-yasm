(define (object? code)
  (or (string? code) (number? code)))

(define (program-list-compile code-list asm-list)
  (if (null? code-list)
      (append '((putnil))(reverse asm-list))
      (let ((asm (compile (car code-list))))
	(program-list-compile (cdr code-list) (cons asm asm-list)))))

(define (compile code)
  (cond 
   ((object? code) `(putobject ,code))
   ((symbol? code) `(getlocal ',code))
   ((=? code)
    (set! symbol-list (cons (cadr code) symbol-list))
    `(,(compile (caddr code)) (setlocal ',(cadr code))))
   ((run? code)
    (append
     (args-compile (cdr code) '())     
	  `((call ',(car code) ,(length (cdr code))))))
   (else
    (error code))))

(define (run? code)
  (symbol? (car code)))

(define (args-compile code arg-list)
  (if (null? code)
      (reverse arg-list)
      (args-compile (cdr code)
		    (cons (compile (car code)) arg-list))))


(define (dprint . lis)
  (letrec ((print-list (lambda (lis)
			 (if (not (null? lis))
			     (begin
			       (display (car lis))
			       (print-list (cdr lis)))))))
    (print-list lis)))

(define (atom? a)
  (not (pair? a)))

(define (=? exp)
  (tagged-list? exp '=))

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
    (newline)))
(define (getlocal sym)
  (dprint "getlocal :" sym "\n"))

(define (setlocal sym)
  (dprint "setlocal :" sym "\n"))

(define (call fn len)
  (dprint "call :")
  (dprint fn)
  (dprint ",")
  (dprint len)
  (newline))

(define (putnil)
  (dprint "putnil")
  (newline))

(define (asm-print asm)
 (dprint asm)
 (newline))

(define symbol-list '())
(define (loy-compile code-list)
    (let ((asm-list (program-list-compile code-list '())))
      (dprint "iseq = YASM.toplevel([")
      (symbol-list-print symbol-list)
      (dprint "]){\n")
      (compile-print asm-list)
      (dprint "leave\n" "}\n iseq.eval\n")))
  
(define (symbol-list-print sym-lis)
  (if (not (null? (cdr sym-lis)))
      (begin
	(dprint ":" (car sym-lis) ",")
	(symbol-list-print (cdr sym-lis)))
      (dprint ":" (car sym-lis))))
      
(define (compile-print asm-list)
  (if (not (null? asm-list))
      (if (symbol? (car asm-list))
	    (eval asm-list (interaction-environment))
	    (begin
	      (compile-print (car asm-list))
	      (compile-print (cdr asm-list))))))

(loy-compile '((= x 20) (puts x)))
;(loy-compile '(puts 10))