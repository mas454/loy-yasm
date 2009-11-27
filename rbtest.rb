#3.times{|i| p i}
puts RubyVM::InstructionSequence.compile_file("rbtest.rb").disasm

