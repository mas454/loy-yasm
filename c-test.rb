require 'yasm'
iseq = YASM.toplevel([]){
putnil
putobject "lispu.rb"
call :require, 1
putobject 10
putobject 20
putobject nil
newarray 2
newarray 2
putobject 1
send :[], 1
call :print, 1
putobject "
"
call :print, 1
leave
}
 #puts iseq.disasm
 iseq.eval
