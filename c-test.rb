require 'yasm'
require 'loy'
iseq = YASM.toplevel([]){
putnil
putnil
call :lambda, 0, block([:a]){
putnil
putnil
call :lambda, 0, block([:b]){
putnil
putnil
call :lambda, 0, block([:c]){
putnil
getdynamic :a
getdynamic :b
send :+, 1
getdynamic :c
send :+, 1
call :puts, 1
leave
}
putobject 30
call :lamcall, 2
leave
}
putobject 20
call :lamcall, 2
leave
}
putobject 10
call :lamcall, 2
leave
}
 #puts iseq.disasm
 iseq.eval
