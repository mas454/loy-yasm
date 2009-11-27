require 'yasm'
require 'lispu'
iseq = YASM.toplevel([]){
putnil
putobject false
branchunless :else_part
putobject "hello"
call :puts, 1
jump :end
_ :else_part
putobject "false"
call :puts, 1
_ :end
leave
}
 iseq.eval
