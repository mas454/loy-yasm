require 'yasm'
iseq = YASM.toplevel([:iseq]){
putnil
putobject "lispu.rb"
call :require, 1
_ :lstart
getinlinecache 0, :lend
getconstant :RubyVM
getconstant :InstructionSequence
setinlinecache :lstart
_ :lend
putobject "rbtest.rb"
send :compile_file, 1
setlocal :iseq
getlocal :iseq
send :disasm, 0
call :puts, 1
leave
}
 #puts iseq.disasm
 iseq.eval
