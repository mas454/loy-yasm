def test
end
a = 10
b = 20
p(a+b)
3.times{|i| p i}
puts RubyVM::InstructionSequence.compile_file("times.rb").disasm

