#!ruby -Ks
require 'lib/lib.rb'
require("l2r.rb")
def interp(in_file)
loop() {||
exp = read(in_file)
ruby_str = compile(exp, false)
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
if in_file.eof?()
quit()
end
}
end

def interp_main()
lib_compile()
interp(open(ARGV.get(0), "r"))
end

interp_main()
