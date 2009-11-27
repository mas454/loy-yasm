require 'yasm'
iseq = YASM.toplevel([]){
putnil
definemethod(:fib,YASM.method(:fib, [:x]){
getlocal :x
putobject 1
send :<=, 1
branchunless :else_part
putobject 1
jump :end
_ :else_part
putnil
getlocal :x
putobject 2
send :-, 1
call :fib, 1
putnil
getlocal :x
putobject 1
send :-, 1
call :fib, 1
send :+, 1
_ :end
leave
})
pop
leave
}
 #puts iseq.disasm
 iseq.eval
def fibr(x)
  if x <= 1
    1
  else
    fibr(x-2) + fibr(x-1)
  end
end

require 'benchmark'
3.times {
  print "Scheme:", Benchmark.measure { fib(36)  }
  print "Ruby:  ", Benchmark.measure { fibr(36) }
}
