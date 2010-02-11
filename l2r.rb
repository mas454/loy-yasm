#!ruby -Ks
require 'lib/lib.rb'
def string_list_append(str_list)
if null?(str_list)
""
else
str_list[0] + string_list_append(str_list[1])
end
end

nil
def map(fn, lis)
if null?(lis)
nil
else
[fn.call(lis[0]),map(fn, lis[1])]
end
end

nil
def object?(code)
number?(code) or eq?(nil, code) or symbol?(code) or eq?(true, code) or eq?(false, code)
end

nil
def run?(code)
pair?(code) & symbol?(code[0])
end

nil
def set?(exp)
tagged_list?(exp, :"=")
end

nil
def tagged_list?(exp, tag)
if pair?(exp)
eq?(exp[0], tag)
else
false
end
end

nil
def cons?(code)
tagged_list?(code, :"cons")
end

nil
def tostr(code)
code.to_s
end

def argp_print(argp)
if argp
""
else
"\n"
end
end

nil
def wqu()
"\""
end

nil
def macro_val_print?(code)
tagged_list?(code, :"macro_val_print")
end

nil
def compile(code, argp)
if string?(code)
wqu() + code + wqu() + argp_print(argp)
elsif object?(code)
"" + code.to_s + argp_print(argp)
elsif quote?(code)
if symbol?(cadr(code))
":" + wqu() + cadr(code) + wqu()
else
compile(quote_compile(cadr(code)), argp)
end
elsif rasm?(code)
cadr(code)
elsif set?(code)
"" + cadr(code) + " = " + compile(caddr(code), false)
elsif cons?(code)
"[" + compile(cadr(code), true) + "," + compile(caddr(code), true) + "]" + argp_print(argp)
elsif car?(code)
"" + compile(cadr(code), true) + "[0]" + argp_print(argp)
elsif cdr?(code)
"" + compile(cadr(code), true) + "[1]" + argp_print(argp)
elsif cond?(code)
cond_compile(code[1], argp)
elsif if?(code)
if_compile(code[1], argp)
elsif let?(code)
compile(let_compile(code[1]), argp)
elsif def?(code)
def_compile(code[1])
elsif define_macro?(code)
if symbol?(cadr(code))
$macro_list = [cadr(code),$macro_list]
else
$macro_list = [caadr(code),$macro_list]
end
macro_def_compile(code[1])
elsif macro?(code)
str = macro_compile(code, argp)
begin
lamcall(lambda {|mac|
compile(mac, argp) + argp_print(argp)
}, eval(str, TOPLEVEL_BINDING))
rescue SyntaxError
puts("syntax error")
display(code)
newline()
puts("macro compile =>")
p(str)

rescue => exc
puts("macro error")
display(code)
newline()
p(exc)

end
elsif macro_val_print?(code)
compile(cadr(code), true)
elsif lambda?(code)
lambda_compile(code[1], argp)
elsif begin?(code)
program_list_compile(code[1])
elsif infix?(code)
infix_compile(code[0], code[1], argp)
elsif binfix?(code)
binfix_compile(code[0], cadr(code), caddr(code), argp)
elsif run?(code)
run_compile(code, argp)
else
display("compile error ")
display(code)
newline()
exit(1)
end
end

nil
$macro_list = nil
nil
def define_macro?(exp)
tagged_list?(exp, :"macro")
end

nil
def macro?(exp)
if pair?(exp)
memq(exp[0], $macro_list)
else
false
end
end

nil
def macro_compile(code, argp)
"_" + code[0] + if null?(code[1])
"()" + argp_print(argp)
else
"(" + macro_args_compile(code[1]) + ")" + argp_print(argp)
end
end

nil
def run_compile(code, argp)
"" + code[0] + if null?(code[1])
"()" + argp_print(argp)
else
if null?(cddr(code)) & tagged_list?(cadr(code), :"block")
"()" + block_compile(cdadr(code))
else
args_compile(code[1], argp)
end
end
end

nil
nil
nil
nil
def infix?(exp)
memq(exp[0], [:"+",[:"-",[:"*",[:"/",[:"%",[:"**",[:"^",[:"&",[:"or",nil]]]]]]]]])
end

nil
def begin?(exp)
tagged_list?(exp, :"begin")
end

nil
def rasm?(exp)
tagged_list?(exp, :"rasm")
end

nil
def infix_compile(inf, arg_list, argp)
compile(arg_list[0], true) + string_list_append(map(lambda {|arg|
" " + inf + " " + compile(arg, true)
}, arg_list[1])) + argp_print(argp)
end

nil
def binfix?(exp)
memq(exp[0], [:"<",[:">",[:"<=",[:"==",[:"=~",nil]]]]])
end

nil
def binfix_compile(inf, arg1, arg2, argp)
compile(arg1, true) + " " + inf + " " + compile(arg2, argp)
end

nil
def def_arg_string(lis)
arg_comp = lambda {|arg|
if symbol?(arg)
arg
else
"" + arg[0] + "=" + compile(cadr(arg), true)
end
}
if null?(lis)
""
else
"" + arg_comp.call(lis[0]) + string_list_append(map(lambda {|code|
", " + arg_comp.call(code)
}, lis[1]))
end
end

nil
def def_compile(code)
if symbol?(code[0])
"def " + code[0] + "(" + def_arg_string(cadr(code)) + ")\n" + program_list_compile(cddr(code)) + "end\n\n"
else
"def " + caar(code) + "(" + def_arg_string(cdar(code)) + ")\n" + program_list_compile(code[1]) + "end\n\n"
end
end

def macro_def_compile(code)
"def _" + caar(code) + "(" + def_arg_string(cdar(code)) + ")\n" + program_list_compile(code[1]) + "end\n\n"
end

nil
def macro_args_compile(lis)
compile(quote_compile(lis[0]), true) + if null?(lis[1])
""
else
string_list_append(map(lambda {|a|
", " + compile(quote_compile(a), true)
}, lis[1]))
end
end

nil
nil
def quote_compile(lis)
if null?(lis)
nil
elsif symbol?(lis)
list(:"quote", lis)
elsif string?(lis)
lis
elsif object?(lis)
lis
else
list(:"cons", quote_compile(lis[0]), quote_compile(lis[1]))
end
end

nil
def lambda?(code)
tagged_list?(code, :"lambda") or tagged_list?(code, :"->")
end

nil
def lambda_compile(lam_list, argp)
"lambda {|" + def_arg_string(lam_list[0]) + "|\n" + program_list_compile(lam_list[1]) + "}" + argp_print(argp)
end

def block_compile(bl_list)
" {|" + def_arg_string(bl_list[0]) + "|\n" + program_list_compile(bl_list[1]) + "}\n"
end

nil
def quote?(exp)
tagged_list?(exp, :"quote")
end

nil
def if?(exp)
tagged_list?(exp, :"if")
end

nil
def if_compile(code, argp)
"if " + compile(code[0], false) + compile(cadr(code), false) + if null?(cddr(code))
""
else
"else\n" + compile(caddr(code), false)
end + "end" + argp_print(argp)
end

nil
def args_cdr_compile(args_list)
if null?(args_list)
""
else
", " + compile(args_list[0], true) + args_cdr_compile(args_list[1])
end
end

nil
def args_compile(args_list, argp)
"(" + compile(args_list[0], true) + string_list_append(map(lambda {|code|
if tagged_list?(code, :"block")
""
else
", " + compile(code, true)
end
}, args_list[1])) + ")" + lamcall(lambda {|bloc|
if null?(bloc)
""
else
block_compile(bloc)
end
}, block?(args_list)) + argp_print(argp)
end

nil
def block?(lis)
if null?(lis)
nil
else
if tagged_list?(lis[0], :"block")
cdar(lis)
else
block?(lis[1])
end
end
end

nil
def car?(code)
tagged_list?(code, :"car")
end

nil
def cdr?(code)
tagged_list?(code, :"cdr")
end

nil
def cond?(code)
tagged_list?(code, :"cond")
end

nil
def cond_compile(code, argp)
"if " + compile(caar(code), false) + program_list_compile(cdar(code)) + if null?(code[1])
""
else
string_list_append(map(lambda {|con|
if eq?(:"else", con[0])
"else\n" + program_list_compile(con[1])
else
"elsif " + compile(con[0], false) + program_list_compile(con[1])
end
}, code[1]))
end + "end" + argp_print(argp)
end

def let?(exp)
tagged_list?(exp, :"let")
end

def let_compile(lis)
car_lam = lambda {|a|
a[0]
}
cadr_lam = lambda {|a|
cadr(a)
}
append2(list(:"lamcall", append2(list(:"lambda", map(car_lam, lis[0])), lis[1])), map(cadr_lam, lis[0]))
end

nil
def def?(exp)
tagged_list?(exp, :"def")
end

nil
def main?(exp)
tagged_list?(exp, :"main")
end

nil
def program_list_compile(code_list)
string_list_append(map(lambda {|a|
compile(a, false)
}, code_list))
end

nil
def lib_compile()
in_file = open("lib/lib.loy", "r")
loop() {||
exp = read(in_file)
ruby_str = compile(exp, false)
eval(ruby_str, TOPLEVEL_BINDING)
if in_file.eof?()
break
end
}
end

nil
def quit()
exit(0)
end

