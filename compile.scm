(define (object? code)
  (or (string? code) (number? code)))

(define (compile code)
  (cond 
   ((object? code) `(putobject ,code))
   ((symbol? (car code))
    (append
     '((putnil))
     (args-compile (cdr code) '())     
	  `((call ',(car code) ,(length (cdr code))))))))
(define (
(define (args-compile code arg-list)
  (if (null? code)
      (reverse arg-list)
      (args-compile (cdr code)
		    (cons (compile (car code)) arg-list))))
(define (dprint a)
  (display a))
(define (atom? a)
  (not (pair? a)))

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


(define (loy-compile code)
  (let ((asm-list (compile code)))
	(dprint "iseq = YASM.toplevel([]){\n")
	(compile-print asm-list)
	(dprint "leave\n")
	(dprint "}\n iseq.eval\n")))
  

(define (compile-print asm-list)
  (if (not (null? asm-list))
      (if (symbol? (car asm-list))
	    (eval asm-list (interaction-environment))
	    (begin
	      (compile-print (car asm-list))
	      (compile-print (cdr asm-list))))))

;(compile-print (compile '(puts (+ 10 20))))
(loy-compile '(puts 10))
;(asm-print (compile '(puts 10)))
