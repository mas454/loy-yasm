def test
end
a = 10
#3.times{|i| p i}
puts RubyVM::InstructionSequence.compile_file("rbtest.rb").disasm
#RubyVM::InstructionSequence.load( RubyVM::InstructionSequence.compile( "[1,2,3].each{|x|p x}" ).to_a).eval 
