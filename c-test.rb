require 'yasm'
iseq = YASM.toplevel([:x]){
putnil
putobject false
branchunless :G0
putobject "hello"
call :puts, 1
jump :G1
_ :G0
putobject 10
setlocal :x
getlocal :x
call :puts, 1
jump :G1
_ :G2
_ :G1
leave
}
 #puts iseq.disasm
 iseq.eval
