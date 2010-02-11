#!ruby -Ks
require 'lib/lib.rb'
require("l2r.rb")
def l2r_compile(in_file, out_file)
display("#!ruby -Ks\n", out_file)
display("require 'lib/lib.rb'\n", out_file)
loop() {||
exp = read(in_file)
ruby_str = compile(exp, false)
if not(define_macro?(exp))
display(ruby_str, out_file)
end
out_file.flush()
if def?(exp) or define_macro?(exp)
begin
eval(ruby_str, TOPLEVEL_BINDING)
rescue SyntaxError
puts("syntax error")
display(exp)
newline()
puts("compile => 
", ruby_str)

rescue => exec
puts("compile eval error")
display(exp)
newline()
puts("compile =>
", ruby_str)
p(exec)

end
end
if in_file.eof?()
quit()
end
}
end

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

def repl()
loop() {||
display(">")
begin
sexp = read()
expr = compile(sexp, false)
display(eval(expr, TOPLEVEL_BINDING))
newline()
rescue SyntaxError
puts("syntax error")
display(sexp)
newline()
puts("compile =>
", expr)

rescue => exec
puts("compile eval error")
display(sexp)
newline()
puts("compile =>
", expr)
p(exec)

end
}
end

nil
def main()
lib_compile()
if null?(ARGV.get(0))
repl()
else
if null?(ARGV.get(1))
l2r_compile(open(ARGV.get(0), "r"), open("out.rb", "w"))
else
l2r_compile(open(ARGV.get(0), "r"), open(ARGV.get(1), "w"))
end
end
end

nil
main()
nil
