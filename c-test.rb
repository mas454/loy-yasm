require 'yasm'
iseq = YASM.toplevel([:x]){
putnil
putobject "lispu.rb"
call :require, 1
_ :lstart
getinlinecache 0, :lend
getconstant :Test
setinlinecache :lstart
_ :lend
send :new, 0
setlocal :x
getlocal :x
putobject 10
send :abc, 1
pop
leave
}
 #puts iseq.disasm
 iseq.eval
