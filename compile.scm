(define (object? code)
  (or (string? code) (number? code)))

(define (compile code)
  (cond 
   ((object? code) `(putobject ,code))
   ((symbol? (car code))
    (append (args-compile (cdr code) '())     
	  `((call ,(car code) ,(length (cdr code))))))))

(define (args-compile code arg-list)
  (if (null? code)
      (reverse arg-list)
      (args-compile (cdr code)
		    (cons (compile (car code)) arg-list))))
(define (atom? a)
  (not (list? a)))
(define (putobject num)
  (display "putobject ")
  (display num)
  (newline))

(define (asm-print asm)
 (display asm)
 (newline))

(eval '(putobject 10) (interaction-environment))
;(asm-print (compile '(puts (+ 10 20))))
;(asm-print (compile '(puts (+ 10 20))))