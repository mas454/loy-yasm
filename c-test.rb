require 'yasm'
require 'lispu'
iseq = YASM.toplevel([:b,:a]){
putnil
putobject 10
setlocal :a
putobject 20
setlocal :b
putnil
getlocal :a
putnil
getlocal :b
putobject 20
call :+, 2
call :+, 2
call :puts, 1
leave
}
 iseq.eval
