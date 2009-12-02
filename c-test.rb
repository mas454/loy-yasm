require 'yasm'
require 'loy'
iseq = YASM.toplevel([:x]){
putnil
definemethod(:cons,YASM.method(:cons, [:a,:b]){
putnil
call :lambda, 0, block([:f]){
putnil
getdynamic :f
getdynamic :a
getdynamic :b
call :lamcall, 3
leave
}
leave
})
pop
definemethod(:car,YASM.method(:car, [:f]){
putnil
getlocal :f
putnil
call :lambda, 0, block([:a,:b]){
getdynamic :a
leave
}
call :lamcall, 2
leave
})
pop
definemethod(:cdr,YASM.method(:cdr, [:f]){
getlocal :f
putobject nil
send :==, 1
branchunless :G0
putobject nil
jump :G1
_ :G0
putnil
getlocal :f
putnil
call :lambda, 0, block([:a,:b]){
getdynamic :b
leave
}
call :lamcall, 2
_ :G1
leave
})
pop
putnil
putobject "a"
putnil
putobject "b"
putobject nil
call :cons, 2
call :cons, 2
setlocal :x
putnil
putnil
getlocal :x
call :cdr, 1
call :cdr, 1
call :puts, 1
leave
}
 #puts iseq.disasm
 iseq.eval
