require("lispu.rb")
def con(a, b)
 lambda {|f|
        f.call(a, b)}
end
def car(f)
   f.call(lambda {|a, b|
		b})
end
puts car(con(10, 20)) 
#puts RubyVM::InstructionSequence.compile_file("rbtest.rb").disasm




