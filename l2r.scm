(define (object? code)
  (or (string? code) (number? code)
      (eq? 'nil code)
      (eq? 'true code) (eq? 'false code)))
 
(define (=? exp)
  (tagged-list? exp '=))
 
(define (def-arg-print lis)
  (if (not (null? lis))
      (begin
(dprint (car lis))
(map (lambda (code)
(dprint ", " code)) (cdr lis)))))
 
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
      #f))
(define (lparen)
  (dprint "("))
(define (rparen argp)
  (dprint ")" (if argp "" "\n")))
 
(define (block? lis)
  (if (null? lis)
      '()
      (if (tagged-list? (car lis) 'block)
(cdar lis)
(block? (cdr lis)))))
 
(define (args-compile args argp)
  (let ((code1 (list (compile (car args) #t))))
(append
(list '(lparen))
(list code1)
(if (null? (cdr args))
'()
(map (lambda (code)
(if (tagged-list? code 'block)
'()
(list '(canma)
(compile code #t))))
(cdr args)))
(list (list 'rparen #t))
(let ((block (block? args)))
(if (null? block)
'()
(list 'block-compile (list'quote block))))
(if argp
'()
'((comprint "\n"))))))
 
 
(define (run? code)
  (and (pair? code) (symbol? (car code))))
 
(define (compile code argp)
  (cond ((object? code) (list 'putobject code argp))
((symbol? code) (list 'putobject (list 'quote code) argp))
((quote? code)
(if (symbol? (cadr code))
(list (list 'comprint ":")
(list 'putobject
(if (eq? (cadr code) '=)
"="
(list 'quote (cadr code)))
argp))
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
((begin? code)
(list 'begin-print (list 'quote (cdr code))))
((infix? code)
(infix-compile (car code) (cdr code) argp))
((binfix? code)
(binfix-compile (car code) (cadr code) (caddr code) argp))
((run? code)
(list (list 'putobject (list 'quote (car code)) #t)
(if (null? (cdr code))
(list '0-args argp)
(if (and (null? (cddr code))
(tagged-list? (cadr code) 'block))
(list (list '0-args #t)
(list 'block-compile (list 'quote
(cdadr code))))
(args-compile (cdr code) argp)))))
(else
(error "unknown expression type -- compile" code))
 
)
)
 
(define (symbol-printx sym)
  (dprint ":" sym))
 
(define (infix? exp)
  (memq (car exp) '(+ - * / % ** ^ & or)))
 
(define (binfix? exp)
  (memq (car exp) '(< > <= ==)))
(define (binfix-compile inf arg1 arg2 argp)
  (list (compile arg1 #t)
(list 'comprint " ")
(list 'comprint (list 'quote inf))
(list 'comprint " ")
(compile arg2 argp)))
 
(define (infix-compile inf args-list argp)
  (append
   (list (compile (car args-list) #t))
   (map (lambda (args)
(list
(list 'comprint " ")
(list 'comprint (list 'quote inf))
(list 'comprint " ")
(compile args #t)))
(cdr args-list))
   (if argp
       '()
       '(comprint "\n"))))
 
 
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
  (or
   (tagged-list? code 'lambda)
   (tagged-list? code '->)))
 
(define (def code)
  (if (symbol? (car code))
      (begin
(dprint "def " (car code) "(")
(def-arg-print (cadr code)) (dprint ")\n")
(compile-print (program-list-compile (cddr code))))
      (begin
(dprint "def " (caar code) "(")
(def-arg-print (cdar code)) (dprint ")\n")
(compile-print (program-list-compile (cdr code)))))
  
  
  (dprint "end\n\n"))
 
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
 
(define (begin? exp)
  (tagged-list? exp 'begin))
 
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
(define (block-compile bl-list)
  (dprint " {|")
  (def-arg-print (car bl-list))
  (dprint "|\n")
  (compile-print (program-list-compile (cdr bl-list)))
  (dprint "}\n")
  )
(define (-> lam-list argp)
  (dprint "lambda {|")
  (def-arg-print (car lam-list))
  (dprint "|\n")
  (compile-print (program-list-compile (cdr lam-list)))
  (dprint "}" (if argp "" "\n"))
  )
 
(define (begin-print code-list)
  (compile-print (program-list-compile code-list)))
 
(define (compile-print asm-list)
  (if (not (null? asm-list))
      (if (symbol? (car asm-list))
(eval asm-list (interaction-environment))
(begin
(compile-print (car asm-list))
(compile-print (cdr asm-list))))))
 
 
(define if-test '((= x 10)(if true (puts x)))) ;(puts "else"))))
(define def-test '((def (test a b) (puts a b)) (test 20 30)))
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
(puts (== a b)))
)
  )
(define quote-test '(
(display '((a b) c))
)
  )
  
(define (l2r code-list)
  (compile-print (program-list-compile code-list))
  )
 
(define list-fun '(
(def list_loop (lis num)
(if (== (lis.length) num)
nil
(cons (lis.get num)
(list_loop lis (+ num 1)))))
(def list (*b)
(list_loop b 0))
(display (list 20 30 40 50))))
(define kind-test '(
(= a "abc")
(puts 'a)
)
  )
(define block-test '(
(3.times (block
(i k)
(p i)))))
 
 
 
(define (s-read file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop ((ls1 '()) (s (read)))
(if (eof-object? s)
(reverse ls1)
(loop (cons s ls1) (read)))))))
(define out-p (open-output-file "l2r-test.rb"))
(define out-p '())
 
(define (lib-compile)
  (let ((program-list (s-read "lib/lib.loy")))
    (set! out-p (open-output-file "lib/lib.rb"))
    (l2r program-list)))
 
(define begin-test
  '(
    (begin
      (+ 10 20 30 40)
      (puts a))
    )
  )
(define reverse-test '(
(display (append '(a b c) '(d e f)))
(print "\n")
)
)
;(display (program-list-compile block-test))
;(newline)
 
(define (main args)
  (let ((program-list (s-read (cadr args))))
    (set! out-p (open-output-file (caddr args)))
    (dprint "require \"lib/lib.rb\"\n")
    (l2r program-list)))
(define (l2r-test t-list)
  (dprint "require \"lib/lib.rb\"\n")
  (l2r t-list))
;(l2r-test reverse-test)
;(lib-compile)
