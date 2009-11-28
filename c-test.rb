require 'yasm'
iseq = YASM.toplevel([:x]){
putnil
putobject "lispu.rb"
call :require, 1
putnil
call :lambda, 0, block([:a]){
putnil
getdynamic :a
call :puts, 1
leave
}
setlocal :x
getlocal :x
putobject "hello"
send :call, 1
pop
leave
}
 #puts iseq.disasm
 iseq.eval
