(define (object? code)
  (or (string? code) (number? code)
      (eq? 'nil code)
      (eq? 'true code) (eq? 'false code)))

(define (=? exp)
  (tagged-list? exp '=))

(define (def-arg-print lis)
  (dprint (car lis))
  (map (lambda (code)
	 (dprint ", " code)) (cdr lis)))

(define (dprint . lis)
  (map (lambda (code)
	 (display code out-p)) lis))

(define (set v)
  (dprint v " = "))

(define (putobject code argp)
  (if (string? code)
      (dprint "\"" code "\"")
      (dprint code))
  (if (not argp)
      (dprint "\n")))

(define true 'true)
(define false 'false)
(define nil 'nil)

(define (cons? code)
  (tagged-list? code 'cons))

(define (car? code)
  (tagged-list? code 'car))
(define (cdr? code)
  (tagged-list? code 'cdr))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (lparen)
  (dprint "("))
(define (rparen argp)
  (dprint ")" (if argp "" "\n")))

(define (args-compile args argp)
  (let ((code1 (list (compile (car args) #t))))
	(append
	 (list '(lparen))
	 (list code1)
	 (if (null? (cdr args))
	     '()
	     (map (lambda (code)
		    (list '(canma) 
			  (compile code #t)))
		  (cdr args)))
	 (list (list 'rparen argp)))))

(define (run? code)
  (symbol? (car code)))

(define (compile code argp)
  (cond ((object? code) (list 'putobject code argp))
	((symbol? code) (list 'putobject (list 'quote code) argp))
	((quote? code)
	 (if (symbol? (cadr code))
	     (list (list 'comprint ":")
		   (list 'putobject (list 'quote (cadr code)) argp))
	     (compile (quote-compile (cadr code)) argp)))
	((=? code) 
	 (list (list 'set (list 'quote (cadr code)))
		     (compile (caddr code) #f)))
	((cons? code)
	 (list '(lbrack) (compile (cadr code) #t) 
	   '(canma) (compile (caddr code) #t)
	   (list 'rbrack argp)))
	((car? code)
	 (list (compile (cadr code) #t) 
	   '(lbrack) '(putobject 0 #t) (list 'rbrack argp)))
	((cdr? code)
	 (list (compile (cadr code) #t) 
	   '(lbrack) '(putobject 1 #t) (list 'rbrack argp)))
	((if? code)
	 (if-compile (cdr code) argp))
	((cond? code)
	 (cond-compile (cdr code) argp))
	((let? code)
	 (compile (let-compile (cdr code)) argp))
	((def? code)
	 (list 'def (list 'quote (cdr code))))
	((->? code)
	 (list '-> (list 'quote (cdr code)) argp))
	((infix? code)
	 (infix-compile (car code) (cdr code)))
	((binfix? code)
	 ;(display (car code))(newline)
	 (binfix-compile (car code) (cadr code) (caddr code)))
	((run? code)
	 (list (list 'putobject (list 'quote (car code)) #t)
	       (if (null? (cdr code))
		       (list '0-args argp)
		       (args-compile (cdr code) argp))))
	 
	)
)

(define (symbol-printx sym)
  (dprint ":" sym))
(define (infix? exp)
  (memq (car exp) '(+ - * / % **  ^ &)))
(define (binfix? exp)
  (memq (car exp) '(< > <= ==)))
(define (binfix-compile inf arg1 arg2)
  (list (compile arg1 #t) (list 'comprint (list 'quote inf))
	(compile arg2 #t)))

(define (infix-compile inf args-list)
  (append
   (list (compile (car args-list) #t))
   (map (lambda (args)
	  (list (list 'comprint (list 'quote inf)) (compile args #t)))
	(cdr args-list))))


(define (let-compile lis)
  (append (list 'lamcall (append (list '-> (map car (car lis))) (cdr lis)))
	    (map cadr (car lis))))

(define (quote-compile lis)
  (cond 
   ((null? lis) nil)
   ((symbol? lis)
    (list 'quote lis))
   ((object? lis) lis)
   (else
    (list 'cons 
	  (quote-compile (car lis))
	  (quote-compile (cdr lis))))))


(define (quote? exp)
  (tagged-list? exp 'quote))
(define (def? exp)
  (tagged-list? exp 'def))
(define (->? code)
  (tagged-list? code '->))
(define (def code)
  (dprint "def " (car code) "(")
  (def-arg-print (cadr code)) (dprint ")\n")
  (compile-print (program-list-compile (cddr code)))
  (dprint "end\n"))

(define (program-list-compile code-list)
  (map (lambda (a)
	 (compile a #f)) code-list))
(define (let? exp)
  (tagged-list? exp 'let))
(define (if-compile code argp)
  (list '(comprint "if ") (compile (car code) #f)
    (compile (cadr code) #f)
    (if (null? (cddr code))
	 '()
	 (list '(comprint "else\n") 
	       (compile (caddr code) #f)))
    '(comprint "end") (if argp '(comprint "") '(comprint "\n"))))

(define (cond? code)
  (tagged-list? code 'cond))
(define (cond-compile code argp)
  (append
   (list '(comprint "if ") (compile (caar code) #f)
     (program-list-compile (cdar code)))
   (if (null? (cdr code))
       '()
       (map (lambda (con)
	      (if (eq? 'else (car con))
		  (list '(comprint "else\n")
			(program-list-compile (cdr con)))
		  (list '(comprint "elsif ") (compile (car con) #f)
			(program-list-compile (cdr con)))))
	    (cdr code))
	    )
   (list '(comprint "end")) (if argp '((comprint "")) '((comprint "\n"))))
   )
  

(define (comprint abc)
  (dprint abc))
(define (if? exp)
  (tagged-list? exp 'if))

(define (0-args argp)
  (dprint "()" (if argp "" "\n")))
(define (lbrack)
  (dprint "["))

(define (rbrack argp)
  (dprint "]" (if argp "" "\n"))
)

(define (canma)
  (dprint ",")
  )

(define (-> lam-list argp)
  (display lam-list)
  (newline)
  (dprint "lambda {|")
  (def-arg-print (car lam-list)) 
  (dprint "|\n")
  (compile-print (program-list-compile (cdr lam-list)))
  (dprint "}" (if argp "" "\n"))
  )

(define (compile-print asm-list)
  (if (not (null? asm-list))
      (if (symbol? (car asm-list))
	  (eval asm-list (interaction-environment))
	  (begin
	      (compile-print (car asm-list))
	      (compile-print (cdr asm-list))))))

(define out-p (open-output-file "l2r-test.rb"))
(define if-test '((= x 10)(if true (puts x)))) ;(puts "else"))))
(define def-test '((def test (a b) (puts a b)) (test 20 30)))
(define lambda-test '(
		      (= lam (-> (a b)
				 (puts a b)))
		      (lam.call 10 20)
		      )
)

(define cond-test '(
		    (cond (false (puts "hello, world"))
			  (false "hello")
			  (else (puts 'a)))
		    )
  )
(define let-test '(
		   (let ((a 10) (b 20))
			 (puts  (== a b)))
		   )
  )
(define quote-test '(
		      '((a b) c)
		      )
  )
  
(define (l2r code-list)
  ;(dprint "require \"loy\"\n")
  (compile-print (program-list-compile code-list))
  )
;(display (program-list-compile quote-test))
;(newline)

(l2r quote-test)