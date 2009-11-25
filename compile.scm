(define (object? code)
  (or (string? code) (number? code)))

(define (compile code)
  (cond 
   ((object? code) (list 'putobject code))
   ((symbol? (car code))
    (list 'call (car code) (length (cdr code))))))

(display (compile 10))
(newline)